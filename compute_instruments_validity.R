# ============================================================
# Instrument Validity & Reliability — α and McDonald’s ω
# Writes: instrument_reliability_summary.csv
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(psych)        # alpha(), omega(), fa()
  library(GPArotation)
  library(lavaan)
  library(tibble)
})

# ---------------- Data ----------------
load("normalised_responses.RData")   # object: normalised_responses
dat <- normalised_responses

# ---------------- Safe helpers ----------------
safe_alpha <- function(x, check_keys = FALSE) {
  out <- try(psych::alpha(x, check.keys = check_keys), silent = TRUE)
  if (inherits(out, "try-error")) {
    list(alpha = NA_real_, ase = NA_real_)
  } else {
    a  <- suppressWarnings(tryCatch(unname(out$total$std.alpha), error = function(e) NA_real_))
    se <- suppressWarnings(tryCatch(unname(out$total$ase),        error = function(e) NA_real_))
    list(alpha = a, ase = se)
  }
}

safe_omega <- function(x, nf = NULL) {
  if (is.null(nf)) nf <- max(1, min(3, ncol(x)))
  out <- try(psych::omega(x, nfactors = nf, plot = FALSE), silent = TRUE)
  if (inherits(out, "try-error")) {
    return(list(ot = NA_real_, oh = NA_real_))
  }
  ot <- suppressWarnings(tryCatch(as.numeric(out$omega.tot), error = function(e) NA_real_))
  oh <- suppressWarnings(tryCatch(as.numeric(out$omega_h),   error = function(e) NA_real_))
  if (is.finite(ot) && is.finite(oh) && oh > ot) oh <- NA_real_  # sanity: ωh ≤ ωt
  list(ot = ot, oh = oh)
}

validate_scale <- function(data, items, name, nfactors_for_omega = NULL, check_keys = FALSE) {
  cat("\n\n============================================================\n")
  cat("VALIDATING:", name, "\n")
  cat("Items:", paste(items, collapse = ", "), "\n")
  cat("============================================================\n")

  sub <- data %>% dplyr::select(dplyr::all_of(items))

  # Print detailed α output (best effort)
  invisible(try(print(psych::alpha(sub, check.keys = check_keys)), silent = TRUE))

  cat("\nDescriptive statistics:\n")
  invisible(try(print(psych::describe(sub)), silent = TRUE))

  # Robust N and k
  n_use <- sum(stats::complete.cases(sub))
  k_use <- ncol(sub)

  # Safe α and ω
  a  <- safe_alpha(sub, check_keys = check_keys)
  om <- safe_omega(sub, nf = nfactors_for_omega)

  cat(sprintf("\nMcDonald's omega (total): %s; omega_h: %s\n",
              ifelse(is.na(om$ot), "NA", sprintf("%.3f", om$ot)),
              ifelse(is.na(om$oh), "NA", sprintf("%.3f", om$oh))))

  # Row to return
  tibble(
    scale       = name,
    items       = paste(items, collapse = " "),
    k           = k_use,
    n           = n_use,
    alpha       = a$alpha,
    alpha_se    = a$ase,
    omega_total = om$ot,
    omega_hier  = om$oh,
    mean        = mean(colMeans(sub, na.rm = TRUE), na.rm = TRUE),
    sd          = mean(apply(sub, 2, sd, na.rm = TRUE), na.rm = TRUE)
  )
}

# ---------------- Container for results ----------------
results <- tibble(
  scale = character(), items = character(), k = integer(), n = integer(),
  alpha = numeric(), alpha_se = numeric(), omega_total = numeric(), omega_hier = numeric(),
  mean = numeric(), sd = numeric()
)

# ---------------- METADATA (inspect only) ----------------
cat("\n\n================ METADATA CHECK ==================\n")
dat %>% select(school_id, school_level, school_type, region,
               class_label, gender, history_grade) %>% summary()

# ---------------- KN (dichotomous) ----------------
KN_items <- paste0("KN", 1:6)
results <- bind_rows(results,
  validate_scale(dat, KN_items, "Knowledge (KN1–KN6)", nfactors_for_omega = 1)
)
invisible(try(psych::fa(dat %>% select(all_of(KN_items)), nfactors = 1, fm = "ml"), silent = TRUE))

# ---------------- HPT ----------------
POP_items  <- paste0("POP", 1:3)
ROA_items  <- paste0("ROA", 1:3)
CONT_items <- paste0("CONT", 1:3)
HPT_items  <- c(POP_items, ROA_items, CONT_items)

results <- bind_rows(results,
  validate_scale(dat, HPT_items, "HPT total (POP+ROA+CONT)", nfactors_for_omega = 2),
  validate_scale(dat, POP_items,  "HPT — POP",  nfactors_for_omega = 1),
  validate_scale(dat, ROA_items,  "HPT — ROA",  nfactors_for_omega = 1),
  validate_scale(dat, CONT_items, "HPT — CONT", nfactors_for_omega = 1)
)
cat("\nEFA for HPT (expect POP vs CONT poles, ROA ambiguous):\n")
invisible(try(psych::fa(dat %>% select(all_of(HPT_items)),
                        nfactors = 2, rotate = "oblimin", fm = "ml"), silent = TRUE))

# ---------------- FR-LF mini ----------------
RD_items <- paste0("RD", 1:3)
NS_items <- paste0("NS", 1:3)
FR_items <- c(RD_items, NS_items)

results <- bind_rows(results,
  validate_scale(dat, RD_items, "FR-LF RD subscale", nfactors_for_omega = 1),
  validate_scale(dat, NS_items, "FR-LF NS subscale", nfactors_for_omega = 1),
  validate_scale(dat, FR_items, "FR-LF total (6 items)", nfactors_for_omega = 1)
)
invisible(try(psych::fa(dat %>% select(all_of(FR_items)), nfactors = 1, fm = "ml"), silent = TRUE))

# ---------------- KSA-3 ----------------
A_items <- paste0("A", 1:3)
U_items <- paste0("U", 1:3)
K_items <- paste0("K", 1:3)
KSA_items <- c(A_items, U_items, K_items)

results <- bind_rows(results,
  validate_scale(dat, A_items, "KSA-3 Aggression (A)",      nfactors_for_omega = 1),
  validate_scale(dat, U_items, "KSA-3 Submission (U)",      nfactors_for_omega = 1),
  validate_scale(dat, K_items, "KSA-3 Conventionalism (K)", nfactors_for_omega = 1),
  validate_scale(dat, KSA_items, "KSA-3 total (9 items)",   nfactors_for_omega = 3)
)
invisible(try(psych::fa(dat %>% select(all_of(KSA_items)),
                        nfactors = 3, rotate = "oblimin", fm = "ml"), silent = TRUE))

# ---------------- SDR-5 ----------------
SDR_items <- paste0("SDR", 1:5)
results <- bind_rows(results,
  validate_scale(dat, SDR_items, "SDR-5 Social Desirability", nfactors_for_omega = 1)
)
invisible(try(psych::fa(dat %>% select(all_of(SDR_items)), nfactors = 1, fm = "ml"), silent = TRUE))

# ---------------- Save CSV ----------------
out <- results %>%
  mutate(
    alpha       = round(alpha, 3),
    alpha_se    = round(alpha_se, 3),
    omega_total = round(omega_total, 3),
    omega_hier  = round(omega_hier, 3),
    mean        = round(mean, 2),
    sd          = round(sd, 2)
  )

utils::write.csv(out, "instrument_reliability_summary.csv", row.names = FALSE, fileEncoding = "UTF-8")
message(sprintf("Saved concise summary to: instrument_reliability_summary.csv (%d rows)", nrow(out)))
