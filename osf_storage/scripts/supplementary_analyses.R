# =============================================================================
# SUPPLEMENTARY ANALYSES FOR v2 REVIEW RESPONSES
#
# Extracts numbers needed to address new reviewer concerns:
#   1. Fixed-effects school sensitivity model (ideology β with school dummies)
#   2. NS-only grouping for MG-CFA invariance
#   3. High ideology tertile characteristics (what is "High"?)
#   4. CFA modification indices / residual correlations
#   5. ICC for FR-LF and KSA-3 across classrooms
#   6. Attenuation-corrected ideology–HPT correlation
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lme4)
  library(lmerTest)
  library(lavaan)
  library(janitor)
  library(psych)
})

normalised_responses <- readRDS("student_responses.RDS")
dat_raw <- normalised_responses |> clean_names()

school_var      <- names(dat_raw)[names(dat_raw) %in% c("school_id", "school")][1]
class_label_var <- names(dat_raw)[names(dat_raw) %in%
  c("classroom_label", "class_label", "class")][1]

dat_raw <- dat_raw |>
  mutate(
    !!school_var       := as.factor(.data[[school_var]]),
    !!class_label_var  := as.factor(.data[[class_label_var]]),
    class_id = interaction(.data[[school_var]], .data[[class_label_var]], drop = TRUE)
  )

pop_items  <- paste0("pop", 1:3)
cont_items <- paste0("cont", 1:3)
rd_items   <- paste0("rd", 1:3)
ns_items   <- paste0("ns", 1:3)
a_items    <- paste0("a", 1:3)
u_items    <- paste0("u", 1:3)
k_items    <- paste0("k", 1:3)
ksa_items  <- c(a_items, u_items, k_items)
kn_items   <- paste0("kn", 1:6)
sdr_items  <- paste0("sdr", 1:5)

dat_raw <- dat_raw |>
  mutate(across(all_of(pop_items), ~ 5 - as.numeric(.), .names = "{.col}_rev"))

z <- function(x) as.numeric(scale(x))

dat <- dat_raw |>
  mutate(
    hpt_cont    = rowMeans(across(all_of(cont_items)),       na.rm = TRUE),
    hpt_pop_rev = rowMeans(across(paste0(pop_items, "_rev")), na.rm = TRUE),
    hpt_ctx6    = rowMeans(cbind(hpt_pop_rev, hpt_cont),     na.rm = TRUE),
    frlf_tot    = rowMeans(cbind(
      rowMeans(across(all_of(rd_items)), na.rm = TRUE),
      rowMeans(across(all_of(ns_items)), na.rm = TRUE)
    ), na.rm = TRUE),
    ns_tot      = rowMeans(across(all_of(ns_items)),  na.rm = TRUE),
    rd_tot      = rowMeans(across(all_of(rd_items)),  na.rm = TRUE),
    ksa3_tot    = rowMeans(across(all_of(ksa_items)), na.rm = TRUE),
    kn_total    = rowSums(across(all_of(kn_items)),   na.rm = TRUE),
    sdr5_tot    = rowMeans(across(all_of(sdr_items)), na.rm = TRUE)
  ) |>
  mutate(
    z_hpt_ctx6  = z(hpt_ctx6),
    z_frlf_tot  = z(frlf_tot),
    z_ksa3_tot  = z(ksa3_tot),
    z_kn_total  = z(kn_total),
    z_sdr5_tot  = z(sdr5_tot)
  ) |>
  drop_na(all_of(c(school_var, "class_id")))

cat("\n", strrep("=", 70), "\n")
cat("1. FIXED-EFFECTS SCHOOL SENSITIVITY MODEL\n")
cat(strrep("=", 70), "\n")

form_fe <- as.formula(paste0(
  "z_hpt_ctx6 ~ z_frlf_tot + z_ksa3_tot + z_kn_total + z_sdr5_tot + ",
  school_var
))
m_fe <- lm(form_fe, data = dat)
cf <- coef(summary(m_fe))
cat(sprintf("FR-LF beta = %.4f, SE = %.4f, t = %.3f, p = %.4f\n",
    cf["z_frlf_tot", "Estimate"],
    cf["z_frlf_tot", "Std. Error"],
    cf["z_frlf_tot", "t value"],
    cf["z_frlf_tot", "Pr(>|t|)"]))
confint_fe <- confint(m_fe, "z_frlf_tot")
cat(sprintf("95%% CI: [%.3f, %.3f]\n", confint_fe[1], confint_fe[2]))

cat("\n", strrep("=", 70), "\n")
cat("2. HIGH IDEOLOGY TERTILE CHARACTERISTICS\n")
cat(strrep("=", 70), "\n")

# Compute composite ideology z-score
dat <- dat |>
  mutate(
    frlf_z   = z(frlf_tot),
    ksa3_z   = z(ksa3_tot),
    ideo_z   = (frlf_z + ksa3_z) / 2
  )

qs <- quantile(dat$ideo_z, probs = c(1/3, 2/3), na.rm = TRUE)
dat <- dat |>
  mutate(ideology_group = case_when(
    ideo_z <= qs[1] ~ "Low",
    ideo_z >= qs[2] ~ "High",
    TRUE ~ "Mid"
  ))

group_stats <- dat |>
  filter(ideology_group %in% c("Low", "High")) |>
  group_by(ideology_group) |>
  summarise(
    n         = n(),
    frlf_M    = mean(frlf_tot, na.rm = TRUE),
    frlf_SD   = sd(frlf_tot,  na.rm = TRUE),
    frlf_max  = max(frlf_tot, na.rm = TRUE),
    ns_M      = mean(ns_tot,  na.rm = TRUE),
    ksa_M     = mean(ksa3_tot, na.rm = TRUE),
    .groups = "drop"
  )
print(group_stats)

# What proportion in High group exceeds scale midpoint (3) on FR-LF?
high_grp <- dat |> filter(ideology_group == "High")
above_mid <- mean(high_grp$frlf_tot > 3, na.rm = TRUE)
cat(sprintf("\nProportion of High-tertile with FR-LF > 3.0: %.1f%%\n",
            above_mid * 100))
cat(sprintf("Proportion of full sample with FR-LF > 3.0: %.1f%%\n",
            mean(dat$frlf_tot > 3, na.rm = TRUE) * 100))

cat("\n", strrep("=", 70), "\n")
cat("3. ICCs FOR FR-LF AND KSA-3 AT CLASSROOM LEVEL\n")
cat(strrep("=", 70), "\n")

icc_frlf <- lmer(frlf_tot ~ 1 + (1 | class_id), data = dat, REML = TRUE)
icc_ksa  <- lmer(ksa3_tot ~ 1 + (1 | class_id), data = dat, REML = TRUE)

extract_icc <- function(m, label) {
  vc <- as.data.frame(VarCorr(m))
  var_class <- vc$vcov[1]
  var_resid <- vc$vcov[2]
  icc <- var_class / (var_class + var_resid)
  cat(sprintf("%s: ICC_class = %.3f (class var = %.4f, resid var = %.4f)\n",
              label, icc, var_class, var_resid))
}
extract_icc(icc_frlf, "FR-LF")
extract_icc(icc_ksa,  "KSA-3")

cat("\n", strrep("=", 70), "\n")
cat("4. ATTENUATION-CORRECTED CORRELATION: FR-LF – HPT_CTX6\n")
cat(strrep("=", 70), "\n")

# Observed r
r_obs <- cor(dat$frlf_tot, dat$hpt_ctx6, use = "complete.obs")
# Reliability estimates (omega from Table 3; use CONT omega for HPT composite approx)
# HPT CTX6 = mean(POP_rev, CONT): omega_POP = .55, omega_CONT = .69
# Use average as rough estimate for the composite; FR-LF: not directly computed but alpha~.79
# A rough omega for FR-LF 6-item can be estimated
alpha_frlf <- psych::alpha(dat[, c(rd_items, ns_items)], check.keys = FALSE)$total$raw_alpha
cat(sprintf("FR-LF alpha (6 items): %.3f\n", alpha_frlf))
# Use omega_HPT_composite ≈ .65 (average of .55 and .69), omega_FRLF ≈ alpha as proxy
omega_hpt  <- 0.65
omega_frlf <- alpha_frlf
r_corrected <- r_obs / sqrt(omega_hpt * omega_frlf)
cat(sprintf("Observed r(FR-LF, HPT_CTX6) = %.3f\n", r_obs))
cat(sprintf("Reliability estimates: omega_HPT ~ %.2f, alpha_FRLF ~ %.3f\n",
            omega_hpt, omega_frlf))
cat(sprintf("Attenuation-corrected r = %.3f\n", r_corrected))
cat("(Correction formula: r_corrected = r_obs / sqrt(rel_x * rel_y))\n")

cat("\n", strrep("=", 70), "\n")
cat("5. CFA MODIFICATION INDICES\n")
cat(strrep("=", 70), "\n")

hpt_items_cfa <- c(paste0("pop", 1:3, "_rev"), paste0("roa", 1:3), paste0("cont", 1:3))
cfa_dat <- dat[, hpt_items_cfa]
# Rename for clean output
colnames(cfa_dat) <- c(paste0("POP", 1:3), paste0("ROA", 1:3), paste0("CONT", 1:3))

model_3f <- '
  POP  =~ POP1 + POP2 + POP3
  ROA  =~ ROA1 + ROA2 + ROA3
  CONT =~ CONT1 + CONT2 + CONT3
'

fit_3f <- cfa(model_3f, data = cfa_dat,
              ordered = colnames(cfa_dat),
              estimator = "WLSMV")

mi <- lavaan::modindices(fit_3f, sort. = TRUE, maximum.number = 10)
cat("Top 10 modification indices:\n")
print(mi[, c("lhs", "op", "rhs", "mi", "epc")], row.names = FALSE)

# Residual correlations check
resid_cor <- residuals(fit_3f, type = "cor")$cov
cat("\nLargest absolute residual correlations (|r| > 0.10):\n")
rc_mat <- resid_cor
diag(rc_mat) <- NA
large_resid <- which(abs(rc_mat) > 0.10, arr.ind = TRUE)
if (nrow(large_resid) > 0) {
  for (i in seq_len(nrow(large_resid)/2)) {
    r <- large_resid[i, 1]; c_ <- large_resid[i, 2]
    cat(sprintf("  %s – %s: r = %.3f\n",
                rownames(rc_mat)[r], colnames(rc_mat)[c_], rc_mat[r, c_]))
  }
} else {
  cat("  None exceeding |r| = 0.10\n")
}

cat("\n", strrep("=", 70), "\n")
cat("6. NS-ONLY MG-CFA INVARIANCE (METRIC LEVEL)\n")
cat(strrep("=", 70), "\n")

# Group by NS-only tertile
qs_ns <- quantile(dat$ns_tot, probs = c(1/3, 2/3), na.rm = TRUE)
dat <- dat |>
  mutate(ns_group = case_when(
    ns_tot <= qs_ns[1] ~ "Low",
    ns_tot >= qs_ns[2] ~ "High",
    TRUE ~ "Mid"
  ))

cfad_ns <- dat |>
  filter(ns_group %in% c("Low", "High")) |>
  transmute(
    ns_group,
    POP1 = .data[[paste0("pop1_rev")]],
    POP2 = .data[[paste0("pop2_rev")]],
    POP3 = .data[[paste0("pop3_rev")]],
    ROA1 = .data[[paste0("roa1")]],
    ROA2 = .data[[paste0("roa2")]],
    ROA3 = .data[[paste0("roa3")]],
    CONT1 = .data[[paste0("cont1")]],
    CONT2 = .data[[paste0("cont2")]],
    CONT3 = .data[[paste0("cont3")]]
  ) |>
  mutate(across(-ns_group, ~ suppressWarnings(as.numeric(.)))) |>
  mutate(across(-ns_group, ~ ifelse(. %in% 1:4, ., NA_real_)))

ord_items_ns <- setdiff(names(cfad_ns), "ns_group")

fit_conf_ns <- cfa(model_3f, data = cfad_ns, group = "ns_group",
                   ordered = ord_items_ns, estimator = "WLSMV")
fit_metr_ns <- cfa(model_3f, data = cfad_ns, group = "ns_group",
                   ordered = ord_items_ns, estimator = "WLSMV",
                   group.equal = "loadings")
fit_scal_ns <- cfa(model_3f, data = cfad_ns, group = "ns_group",
                   ordered = ord_items_ns, estimator = "WLSMV",
                   group.equal = c("loadings", "thresholds"))

extract_fit <- function(m, label) {
  f <- lavaan::fitmeasures(m, c("cfi", "rmsea", "srmr"))
  cat(sprintf("%-20s CFI = %.3f, RMSEA = %.3f, SRMR = %.3f\n",
              label, f["cfi"], f["rmsea"], f["srmr"]))
}
cat("NS-only grouping:\n")
extract_fit(fit_conf_ns, "Configural")
extract_fit(fit_metr_ns, "Metric")
extract_fit(fit_scal_ns, "Scalar")
cat(sprintf("Metric ΔCFI = %.3f, Scalar ΔCFI = %.3f\n",
            lavaan::fitmeasures(fit_metr_ns, "cfi") - lavaan::fitmeasures(fit_conf_ns, "cfi"),
            lavaan::fitmeasures(fit_scal_ns, "cfi") - lavaan::fitmeasures(fit_metr_ns, "cfi")))

cat("\nDone. All supplementary analyses complete.\n")
