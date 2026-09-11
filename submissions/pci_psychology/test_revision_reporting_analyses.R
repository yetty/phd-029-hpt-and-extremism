source("submissions/pci_psychology/scoring_helpers.R")
source("submissions/pci_psychology/revision_reporting_analyses.R")

x <- data.frame(a = c(1, 2, NA), b = c(3, NA, NA), c = c(5, 6, 7))
stopifnot(identical(scale_mean(x, c("a", "b", "c"), 2), c(3, 4, NA_real_)))

ci <- fisher_ci(0, 100)
stopifnot(abs(ci[["lower"]] + 0.1964181) < 1e-6)
stopifnot(abs(ci[["upper"]] - 0.1964181) < 1e-6)

rel <- mosier_two_component(0.70, 0.80, 0.50)
stopifnot(abs(rel - 0.8333333) < 1e-6)

plot_data <- prepare_relationship_data(
  c(0, 1, NA), c(2, NA, 3), c(1, 2, 3)
)
stopifnot(nrow(plot_data) == 4)
stopifnot(all(stats::complete.cases(plot_data)))

knowledge <- sum_missing_as_zero(data.frame(a = c(1, NA), b = c(0, 1)))
stopifnot(identical(knowledge, c(1, 1)))

alpha_data <- data.frame(
  a = c(1, 2, 3, 4),
  b = c(1, 2, 3, NA),
  c = c(1, 3, 2, 4)
)
complete_alpha <- psych::alpha(
  alpha_data[stats::complete.cases(alpha_data), ], warnings = FALSE
)$total$raw_alpha
alpha_result <- alpha_summary(alpha_data, ordinal = FALSE)
stopifnot(alpha_result[["n"]] == 3)
stopifnot(abs(alpha_result[["alpha_raw"]] - complete_alpha) < 1e-10)

osf_scoring_scripts <- c(
  "01_measurement_checks.Rmd",
  "02_descriptives_and_zero_order_correlations.Rmd",
  "03_multilevel_models_hypothesis_tests.Rmd",
  "04_dif_and_mg_cfa_measurement_bias.Rmd",
  "05_sensitivity_analyses.Rmd",
  "fig02_measurement_invariance_and_dif.R",
  "fig03_score_distributions.R",
  "fig04_coefficient_plot.R",
  "fig05_marginal_effects.R",
  "supplementary_analyses.R",
  "tost_equivalence_tests_and_mundlak.R",
  "revision_reporting_analyses.R"
)
for (script in osf_scoring_scripts) {
  path <- file.path("osf_storage/scripts", script)
  stopifnot(file.exists(path))
  stopifnot(any(grepl(
    'source("scoring_helpers.R")', readLines(path), fixed = TRUE
  )))
  stopifnot(!any(grepl("min_n =", readLines(path), fixed = TRUE)))
}

cat("revision reporting analysis helper tests passed\n")
