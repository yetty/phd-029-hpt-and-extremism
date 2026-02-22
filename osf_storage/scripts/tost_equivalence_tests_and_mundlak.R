# =============================================================================
# TOST EQUIVALENCE TESTS + MUNDLAK WITHIN/BETWEEN IDEOLOGY DECOMPOSITION
#
# Addresses v3 reviewer concerns:
#   1. Formal equivalence tests (TOST) for ideology -> HPT outcomes
#      SESOI = beta +/- 0.20 (pre-specified in manuscript as "smallest effect
#      of educational interest" based on ~4% variance explained, Cohen 1988)
#   2. Mundlak model: within-class vs between-class ideology predicting HPT
#      Tests classroom climate contamination pathway
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lme4)
  library(lmerTest)
  library(janitor)
})

load("student_responses.RData")
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
rd_items   <- paste0("rd",  1:3)
ns_items   <- paste0("ns",  1:3)
a_items    <- paste0("a",   1:3)
u_items    <- paste0("u",   1:3)
k_items    <- paste0("k",   1:3)
ksa_items  <- c(a_items, u_items, k_items)
kn_items   <- paste0("kn",  1:6)
sdr_items  <- paste0("sdr", 1:5)

dat_raw <- dat_raw |>
  mutate(across(all_of(pop_items), ~ 5 - as.numeric(.), .names = "{.col}_rev"))

z <- function(x) as.numeric(scale(x))

dat <- dat_raw |>
  mutate(
    hpt_cont    = rowMeans(across(all_of(cont_items)),        na.rm = TRUE),
    hpt_pop_rev = rowMeans(across(paste0(pop_items, "_rev")), na.rm = TRUE),
    hpt_ctx6    = rowMeans(cbind(hpt_pop_rev, hpt_cont),      na.rm = TRUE),
    frlf_tot    = rowMeans(cbind(
      rowMeans(across(all_of(rd_items)), na.rm = TRUE),
      rowMeans(across(all_of(ns_items)), na.rm = TRUE)
    ), na.rm = TRUE),
    ksa3_tot    = rowMeans(across(all_of(ksa_items)), na.rm = TRUE),
    kn_total    = rowSums(across(all_of(kn_items)),   na.rm = TRUE),
    sdr5_tot    = rowMeans(across(all_of(sdr_items)), na.rm = TRUE)
  ) |>
  mutate(
    z_hpt_ctx6  = z(hpt_ctx6),
    z_hpt_cont  = z(hpt_cont),
    z_hpt_pop   = z(hpt_pop_rev),
    z_frlf_tot  = z(frlf_tot),
    z_ksa3_tot  = z(ksa3_tot),
    z_kn_total  = z(kn_total),
    z_sdr5_tot  = z(sdr5_tot)
  ) |>
  drop_na(all_of(c(school_var, "class_id")))

# ---- 1. TOST EQUIVALENCE TESTS -----------------------------------------------
# SESOI = beta = 0.20 (defined as smallest effect of educational interest)
# TOST: two one-sided tests at alpha = .05
#   H_low:  beta > -SESOI  (test that effect is not meaningfully negative)
#   H_high: beta <  SESOI  (test that effect is not meaningfully positive)
# Both must be significant to conclude equivalence within [-SESOI, +SESOI]
#
# For lmer, we use lmerTest z-test statistics and compute one-sided p-values.
# TOST p-value = max(p_low, p_high), compared to alpha = .05

SESOI <- 0.20

cat("\n", strrep("=", 70), "\n")
cat("1. FORMAL TOST EQUIVALENCE TESTS  (SESOI = +/-.20)\n")
cat(strrep("=", 70), "\n")

outcomes <- list(
  "HPT_CTX6" = "z_hpt_ctx6",
  "CONT"     = "z_hpt_cont",
  "POP_rev"  = "z_hpt_pop"
)

tost_results <- list()

for (nm in names(outcomes)) {
  dv <- outcomes[[nm]]
  form <- as.formula(paste0(
    dv, " ~ z_frlf_tot + z_ksa3_tot + z_kn_total + z_sdr5_tot + (1 | ",
    school_var, ") + (1 | class_id)"
  ))
  m   <- lmer(form, data = dat)
  cf  <- summary(m)$coefficients

  beta <- cf["z_frlf_tot", "Estimate"]
  se   <- cf["z_frlf_tot", "Std. Error"]
  df   <- cf["z_frlf_tot", "df"]

  # t-statistics for TOST
  t_low  <- (beta - (-SESOI)) / se   # test beta > -SESOI
  t_high <- (beta -   SESOI)  / se   # test beta <  SESOI

  # one-sided p-values (lower tail for high, upper tail for low)
  p_low  <- pt(t_low,  df = df, lower.tail = FALSE) # H_low:  beta > -SESOI
  p_high <- pt(t_high, df = df, lower.tail = TRUE)  # H_high: beta <  SESOI

  tost_p <- max(p_low, p_high)
  equiv  <- tost_p < .05

  # Standard 95% CI
  ci_lo <- beta - qt(.975, df) * se
  ci_hi <- beta + qt(.975, df) * se

  tost_results[[nm]] <- list(beta = beta, se = se, ci_lo = ci_lo, ci_hi = ci_hi,
                              t_low = t_low, t_high = t_high,
                              p_low = p_low, p_high = p_high,
                              tost_p = tost_p, equiv = equiv)

  cat(sprintf("\n%s:\n", nm))
  cat(sprintf("  beta = %.4f, SE = %.4f, 95%% CI [%.3f, %.3f]\n",
              beta, se, ci_lo, ci_hi))
  cat(sprintf("  TOST t_low = %.3f (p = %.4f),  t_high = %.3f (p = %.4f)\n",
              t_low, p_low, t_high, p_high))
  cat(sprintf("  TOST p_max = %.4f  -->  Equivalence (|beta| < .20): %s\n",
              tost_p, ifelse(equiv, "YES (p < .05)", "NO (p >= .05)")))
}

# ---- 2. MUNDLAK WITHIN/BETWEEN IDEOLOGY DECOMPOSITION ----------------------
cat("\n", strrep("=", 70), "\n")
cat("2. MUNDLAK WITHIN/BETWEEN IDEOLOGY DECOMPOSITION\n")
cat("   (Tests classroom climate contamination pathway)\n")
cat(strrep("=", 70), "\n")

# Compute class-mean ideology and within-class centered ideology
dat <- dat |>
  group_by(class_id) |>
  mutate(
    frlf_class_mean = mean(z_frlf_tot, na.rm = TRUE),
    frlf_within     = z_frlf_tot - frlf_class_mean,
    ksa_class_mean  = mean(z_ksa3_tot, na.rm = TRUE),
    ksa_within      = z_ksa3_tot - ksa_class_mean
  ) |>
  ungroup()

for (nm in names(outcomes)) {
  dv <- outcomes[[nm]]
  form_mundlak <- as.formula(paste0(
    dv,
    " ~ frlf_within + frlf_class_mean + z_ksa3_tot + z_kn_total + z_sdr5_tot",
    " + (1 | ", school_var, ") + (1 | class_id)"
  ))
  m_m <- lmer(form_mundlak, data = dat)
  cf  <- summary(m_m)$coefficients

  beta_within  <- cf["frlf_within",     "Estimate"]
  se_within    <- cf["frlf_within",     "Std. Error"]
  p_within     <- cf["frlf_within",     "Pr(>|t|)"]
  beta_between <- cf["frlf_class_mean", "Estimate"]
  se_between   <- cf["frlf_class_mean", "Std. Error"]
  p_between    <- cf["frlf_class_mean", "Pr(>|t|)"]

  cat(sprintf("\n%s:\n", nm))
  cat(sprintf("  FR-LF within-class:   beta = %+.4f, SE = %.4f, p = %.4f\n",
              beta_within,  se_within,  p_within))
  cat(sprintf("  FR-LF between-class:  beta = %+.4f, SE = %.4f, p = %.4f\n",
              beta_between, se_between, p_between))
}

cat("\nDone.\n")
