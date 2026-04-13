# =============================================================================
# VERIFY STATISTICS FOR PCI PSYCHOLOGY MANUSCRIPT
#
# Extracts 5 sets of values flagged during peer review:
#   1. Bonferroni p-value for POP3_rev DIF test
#   2. TOST 90% CI endpoints for ideology coefficient
#   3. NS-only MG-CFA SRMR values (all 3 invariance steps)
#   4. Metric step largest modification index (main MG-CFA)
#   5. Table S1: GRM item parameters by group
#
# Run from project root:
#   Rscript submissions/pci_psychology/verify_statistics.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(mirt)
  library(lavaan)
  library(lme4)
  library(lmerTest)
  library(janitor)
})

# --- Data loading (uppercase variables, as in 04_dif Rmd) --------------------

load("normalised_responses.RData")
stopifnot(exists("normalised_responses"))
dat_raw <- normalised_responses

dat_raw <- dat_raw %>%
  mutate(
    school_id   = as.factor(school_id),
    class_label = as.factor(class_label),
    class_id    = interaction(school_id, class_label, drop = TRUE)
  )

hpt_items <- c(paste0("POP", 1:3), paste0("ROA", 1:3),
               paste0("CONT", 1:3))
dat_raw <- dat_raw %>%
  mutate(across(all_of(hpt_items),
                ~ suppressWarnings(as.numeric(.))))

POP_rev_items <- paste0("POP", 1:3)
dat_raw <- dat_raw %>%
  mutate(across(all_of(POP_rev_items),
                ~ 5 - ., .names = "{.col}_rev")) %>%
  mutate(
    HPT_POP_rev = rowMeans(across(paste0(POP_rev_items, "_rev")),
                           na.rm = TRUE),
    HPT_CONT = rowMeans(across(CONT1:CONT3), na.rm = TRUE),
    HPT_ROA  = rowMeans(across(ROA1:ROA3), na.rm = TRUE),
    HPT_CTX6 = rowMeans(cbind(HPT_POP_rev, HPT_CONT), na.rm = TRUE),
    HPT_TOT9 = rowMeans(cbind(HPT_POP_rev, HPT_CONT, HPT_ROA),
                        na.rm = TRUE)
  )

# Scoring
frlf_items <- c(paste0("RD", 1:3), paste0("NS", 1:3))
ksa_items  <- c(paste0("A", 1:3), paste0("U", 1:3),
                paste0("K", 1:3))
kn_items   <- paste0("KN", 1:6)
sdr_items  <- paste0("SDR", 1:5)

num_blocks <- c(frlf_items, ksa_items, kn_items, sdr_items)

dat <- dat_raw %>%
  mutate(across(all_of(num_blocks),
                ~ suppressWarnings(as.numeric(.)))) %>%
  rowwise() %>%
  mutate(
    FRLF_mean = mean(c_across(all_of(frlf_items)), na.rm = TRUE),
    KSA_mean  = mean(c_across(all_of(ksa_items)), na.rm = TRUE),
    KN_sum    = sum(c_across(all_of(kn_items)), na.rm = TRUE),
    SDR_mean  = mean(c_across(all_of(sdr_items)), na.rm = TRUE),
    NS_mean   = mean(c_across(c("NS1", "NS2", "NS3")), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    FRLF_z = as.numeric(scale(FRLF_mean)),
    KSA_z  = as.numeric(scale(KSA_mean)),
    IDEO_Z = (FRLF_z + KSA_z) / 2
  )

# Tertile groups
qs <- quantile(dat$IDEO_Z, probs = c(.3334, .6666), na.rm = TRUE)
dat <- dat %>%
  mutate(
    ideology_group = case_when(
      IDEO_Z <= qs[1] ~ "Low",
      IDEO_Z >= qs[2] ~ "High",
      TRUE ~ "Mid"
    )
  )

cat("\n", strrep("=", 70), "\n")
cat("Group sizes:\n")
print(table(dat$ideology_group))

# =============================================================================
# STEP 1: VERIFY BONFERRONI P-VALUE FOR POP3_REV
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("STEP 1: BONFERRONI P-VALUE FOR POP3_REV\n")
cat(strrep("=", 70), "\n")

anal <- dat[dat$ideology_group %in% c("Low", "High"), , drop = FALSE]
hpt_mat <- anal[, c("POP1", "POP2", "POP3",
                     "ROA1", "ROA2", "ROA3",
                     "CONT1", "CONT2", "CONT3"), drop = FALSE]
for (j in seq_along(hpt_mat)) {
  hpt_mat[[j]] <- suppressWarnings(as.numeric(hpt_mat[[j]]))
}
grp <- factor(anal$ideology_group, levels = c("Low", "High"))
keep <- rowSums(!is.na(hpt_mat)) >= 2
hpt_mat <- hpt_mat[keep, , drop = FALSE]
grp <- droplevels(grp[keep])

cat("DIF groups: Low =", sum(grp == "Low"),
    ", High =", sum(grp == "High"), "\n")

mod_base <- multipleGroup(
  data       = hpt_mat,
  model      = 1,
  group      = grp,
  itemtype   = "graded",
  invariance = c("slopes", "intercepts", "free_means", "free_var"),
  verbose    = FALSE
)

params_all <- mirt::mod2values(mod_base)$name
unique_pars <- unique(params_all)
pars_slope <- grep("^a", unique_pars, value = TRUE)
pars_thr   <- grep("^d\\d+$", unique_pars, value = TRUE)
pars_to_test <- c(pars_slope, pars_thr)

dif_out <- DIF(
  mod_base,
  which.par   = pars_to_test,
  scheme      = "drop",
  items2test  = colnames(hpt_mat),
  p.adjust    = "bonferroni",
  verbose     = FALSE
)

cat("\nFull DIF output for all items:\n")
dif_df <- as.data.frame(dif_out)
num_cols <- sapply(dif_df, is.numeric)
dif_df[num_cols] <- round(dif_df[num_cols], 6)
print(dif_df)

# Extract POP3 specifically
pop3_row <- as.data.frame(dif_out)["POP3", ]
cat("\nPOP3 row from DIF output:\n")
print(pop3_row)

# Also compute manual Bonferroni: raw_p * 9
cat("\nManual Bonferroni check:\n")
p_cols <- grep("^p\\.", names(as.data.frame(dif_out)), value = TRUE)
cat("P-value columns in DIF output:", paste(p_cols, collapse = ", "), "\n")
for (pc in p_cols) {
  raw_p <- as.data.frame(dif_out)["POP3", pc]
  cat(sprintf("  %s: raw = %.6f, manual_bonf (×9) = %.6f\n",
              pc, raw_p, raw_p * 9))
}

# The overall chi-square test for POP3
cat("\nOverall chi-square columns:\n")
chi_cols <- grep("^X2|^chi", names(as.data.frame(dif_out)),
                 value = TRUE, ignore.case = TRUE)
p_overall <- grep("^p$|^p\\.adj", names(as.data.frame(dif_out)),
                  value = TRUE)
cat("Chi-sq cols:", paste(chi_cols, collapse = ", "), "\n")
cat("P cols:", paste(p_overall, collapse = ", "), "\n")

# Print full column names for inspection
cat("\nAll column names in dif_out:\n")
cat(paste(names(as.data.frame(dif_out)), collapse = "\n"), "\n")

# =============================================================================
# STEP 2: TOST 90% CI ENDPOINTS
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("STEP 2: TOST 90% CI ENDPOINTS\n")
cat(strrep("=", 70), "\n")

# Prepare z-scored variables for TOST
z <- function(x) as.numeric(scale(x))

dat_tost <- dat %>%
  mutate(
    z_hpt_ctx6 = z(HPT_CTX6),
    z_frlf_tot = z(FRLF_mean),
    z_ksa3_tot = z(KSA_mean),
    z_kn_total = z(KN_sum),
    z_sdr5_tot = z(SDR_mean)
  )

SESOI <- 0.20

m_ctx6 <- lmer(
  z_hpt_ctx6 ~ z_frlf_tot + z_ksa3_tot + z_kn_total + z_sdr5_tot +
    (1 | school_id) + (1 | class_id),
  data = dat_tost
)

cf <- summary(m_ctx6)$coefficients
beta <- cf["z_frlf_tot", "Estimate"]
se   <- cf["z_frlf_tot", "Std. Error"]
df_val <- cf["z_frlf_tot", "df"]

# 90% CI (for TOST equivalence)
ci90_lo <- beta - qt(.95, df_val) * se
ci90_hi <- beta + qt(.95, df_val) * se

# 95% CI (standard)
ci95_lo <- beta - qt(.975, df_val) * se
ci95_hi <- beta + qt(.975, df_val) * se

# TOST
t_low  <- (beta - (-SESOI)) / se
t_high <- (beta -   SESOI)  / se
p_low  <- pt(t_low,  df = df_val, lower.tail = FALSE)
p_high <- pt(t_high, df = df_val, lower.tail = TRUE)
tost_p <- max(p_low, p_high)

cat(sprintf("HPT_CTX6 ~ FR-LF ideology:\n"))
cat(sprintf("  beta = %.4f, SE = %.4f, df = %.1f\n", beta, se, df_val))
cat(sprintf("  95%% CI [%.4f, %.4f]\n", ci95_lo, ci95_hi))
cat(sprintf("  90%% CI [%.4f, %.4f]  (for TOST)\n", ci90_lo, ci90_hi))
cat(sprintf("  SESOI = +/- %.2f\n", SESOI))
cat(sprintf("  TOST p_low = %.6f, p_high = %.6f, p_max = %.6f\n",
            p_low, p_high, tost_p))
cat(sprintf("  Equivalence: %s\n",
            ifelse(tost_p < .05, "YES", "NO")))
cat(sprintf("  90%% CI within SESOI: %s\n",
            ifelse(ci90_lo > -SESOI & ci90_hi < SESOI, "YES", "NO")))

# =============================================================================
# STEP 3: NS-ONLY MG-CFA SRMR VALUES
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("STEP 3: NS-ONLY MG-CFA SRMR VALUES\n")
cat(strrep("=", 70), "\n")

# NS tertile groups
qs_ns <- quantile(dat$NS_mean, probs = c(1/3, 2/3), na.rm = TRUE)
dat <- dat %>%
  mutate(ns_group = case_when(
    NS_mean <= qs_ns[1] ~ "Low",
    NS_mean >= qs_ns[2] ~ "High",
    TRUE ~ "Mid"
  ))

cat("NS-only group sizes:\n")
print(table(dat$ns_group))

cfad_ns <- dat %>%
  filter(ns_group %in% c("Low", "High")) %>%
  transmute(
    ns_group,
    POP1 = POP1_rev, POP2 = POP2_rev, POP3 = POP3_rev,
    ROA1, ROA2, ROA3,
    CONT1, CONT2, CONT3
  ) %>%
  mutate(across(-ns_group,
                ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(across(-ns_group,
                ~ ifelse(. %in% 1:4, ., NA_real_)))

ord_items_ns <- setdiff(names(cfad_ns), "ns_group")

model_3f <- '
  POP  =~ POP1 + POP2 + POP3
  ROA  =~ ROA1 + ROA2 + ROA3
  CONT =~ CONT1 + CONT2 + CONT3
'

fit_conf_ns <- cfa(model_3f, data = cfad_ns, group = "ns_group",
                   ordered = ord_items_ns, estimator = "WLSMV")
fit_metr_ns <- cfa(model_3f, data = cfad_ns, group = "ns_group",
                   ordered = ord_items_ns, estimator = "WLSMV",
                   group.equal = "loadings")
fit_scal_ns <- cfa(model_3f, data = cfad_ns, group = "ns_group",
                   ordered = ord_items_ns, estimator = "WLSMV",
                   group.equal = c("loadings", "thresholds"))

extract_fit_full <- function(m, label) {
  f <- fitmeasures(m, c("cfi.scaled", "rmsea.scaled", "srmr",
                         "chisq.scaled", "df.scaled",
                         "pvalue.scaled"))
  cat(sprintf("%-12s CFI=%.3f  RMSEA=%.3f  SRMR=%.3f  chi2=%.2f  df=%.0f  p=%.3f\n",
              label,
              f["cfi.scaled"], f["rmsea.scaled"], f["srmr"],
              f["chisq.scaled"], f["df.scaled"], f["pvalue.scaled"]))
  return(f)
}

cat("\nNS-only MG-CFA fit indices:\n")
f_conf <- extract_fit_full(fit_conf_ns, "Configural")
f_metr <- extract_fit_full(fit_metr_ns, "Metric")
f_scal <- extract_fit_full(fit_scal_ns, "Scalar")

cat(sprintf("\nDelta fit (metric vs configural):\n"))
cat(sprintf("  dCFI = %.3f, dRMSEA = %.3f, dSRMR = %.3f\n",
            f_metr["cfi.scaled"] - f_conf["cfi.scaled"],
            f_metr["rmsea.scaled"] - f_conf["rmsea.scaled"],
            f_metr["srmr"] - f_conf["srmr"]))
cat(sprintf("Delta fit (scalar vs metric):\n"))
cat(sprintf("  dCFI = %.3f, dRMSEA = %.3f, dSRMR = %.3f\n",
            f_scal["cfi.scaled"] - f_metr["cfi.scaled"],
            f_scal["rmsea.scaled"] - f_metr["rmsea.scaled"],
            f_scal["srmr"] - f_metr["srmr"]))

# =============================================================================
# STEP 4: METRIC STEP LARGEST MODIFICATION INDEX (MAIN MG-CFA)
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("STEP 4: METRIC STEP LARGEST MODIFICATION INDEX\n")
cat(strrep("=", 70), "\n")

# Main ideology MG-CFA (rerun)
cfad_main <- dat %>%
  filter(ideology_group %in% c("Low", "High")) %>%
  transmute(
    ideology_group,
    POP1 = POP1_rev, POP2 = POP2_rev, POP3 = POP3_rev,
    ROA1, ROA2, ROA3,
    CONT1, CONT2, CONT3
  ) %>%
  mutate(across(-ideology_group,
                ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(across(-ideology_group,
                ~ ifelse(. %in% 1:4, ., NA_real_)))

ord_items_main <- setdiff(names(cfad_main), "ideology_group")

fit_metr_main <- cfa(model_3f, data = cfad_main,
                     group = "ideology_group",
                     ordered = ord_items_main,
                     estimator = "WLSMV",
                     group.equal = c("loadings"))

mi <- modificationIndices(fit_metr_main, sort. = TRUE)
cat("\nTop 10 modification indices (metric model):\n")
print(head(mi, 10))

cat(sprintf("\nLargest MI: %.2f (op: %s, lhs: %s, rhs: %s, group: %d)\n",
            mi$mi[1], mi$op[1], mi$lhs[1], mi$rhs[1], mi$group[1]))

# =============================================================================
# STEP 5: TABLE S1 — GRM ITEM PARAMETERS BY GROUP
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("STEP 5: GRM ITEM PARAMETERS BY GROUP\n")
cat(strrep("=", 70), "\n")

item_pars <- coef(mod_base, simplify = TRUE)

cat("\nLow ideology group parameters:\n")
print(round(item_pars$Low$items, 3))

cat("\nHigh ideology group parameters:\n")
print(round(item_pars$High$items, 3))

# Build Table S1 as a data frame
build_table <- function(pars, group_label) {
  items_df <- as.data.frame(round(pars$items, 3))
  items_df$Item <- rownames(items_df)
  items_df$Group <- group_label
  items_df
}

table_s1 <- bind_rows(
  build_table(item_pars$Low, "Low"),
  build_table(item_pars$High, "High")
) %>%
  select(Group, Item, everything()) %>%
  arrange(Item, Group)

cat("\nTable S1 (combined):\n")
print(table_s1, row.names = FALSE)

# Save to CSV
write.csv(table_s1,
          "submissions/pci_psychology/table_s1_irt_parameters.csv",
          row.names = FALSE)
cat("\nTable S1 saved to: submissions/pci_psychology/table_s1_irt_parameters.csv\n")

cat("\n", strrep("=", 70), "\n")
cat("ALL EXTRACTIONS COMPLETE\n")
cat(strrep("=", 70), "\n")
