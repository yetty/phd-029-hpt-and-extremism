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

test_failures <- character()
expect_true <- function(condition, message) {
  if (!isTRUE(condition)) {
    test_failures <<- c(test_failures, message)
  }
}

extract_calls <- function(text, function_name) {
  pattern <- paste0("\\b", function_name, "\\s*\\(")
  starts <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (identical(starts, -1L)) {
    return(character())
  }

  match_lengths <- attr(starts, "match.length")
  calls <- character(length(starts))
  for (i in seq_along(starts)) {
    open <- starts[[i]] + match_lengths[[i]] - 1L
    depth <- 0L
    close <- NA_integer_
    for (position in seq.int(open, nchar(text))) {
      character <- substr(text, position, position)
      if (identical(character, "(")) depth <- depth + 1L
      if (identical(character, ")")) depth <- depth - 1L
      if (depth == 0L) {
        close <- position
        break
      }
    }
    calls[[i]] <- substr(text, starts[[i]], close)
  }
  calls
}

scoring_scripts <- c(
  "01_measurement-checks.Rmd",
  "02_descriptives-and-zero-order.Rmd",
  "03_multilevel-models-hypothesis-tests.Rmd",
  "04_dif-and-mg-cfa-hpt-bias.Rmd",
  "05_sensitivity-analyses.Rmd",
  "submissions/pci_psychology/revision_reporting_analyses.R",
  "submissions/pci_psychology/verify_statistics.R",
  file.path("osf_storage/scripts", osf_scoring_scripts)
)

for (script in scoring_scripts) {
  calls <- extract_calls(paste(readLines(script, warn = FALSE), collapse = "\n"),
                         "scale_mean")
  missing_threshold <- !grepl("min_answered\\s*=", calls, perl = TRUE)
  expect_true(
    length(calls) > 0L && !any(missing_threshold),
    paste0("Every scale_mean() call in ", script,
           " must declare min_answered explicitly.")
  )
}

cfa_scripts <- c(
  "01_measurement-checks.Rmd",
  "osf_storage/scripts/01_measurement_checks.Rmd"
)
for (script in cfa_scripts) {
  calls <- extract_calls(paste(readLines(script, warn = FALSE), collapse = "\n"),
                         "fitMeasures")
  expect_true(length(calls) > 0L,
              paste0(script, " must extract CFA fit indices."))
  for (index in seq_along(calls)) {
    expect_true(
      all(vapply(c("cfi.scaled", "tli.scaled", "rmsea.scaled"),
                 grepl, logical(1), x = calls[[index]], fixed = TRUE)),
      paste0("CFA fitMeasures() call ", index, " in ", script,
             " must use scaled WLSMV CFI, TLI, and RMSEA indices.")
    )
  }
}

icc_scripts <- c(
  "02_descriptives-and-zero-order.Rmd",
  "osf_storage/scripts/02_descriptives_and_zero_order_correlations.Rmd"
)
for (script in icc_scripts) {
  text <- paste(readLines(script, warn = FALSE), collapse = "\n")
  expect_true(
    grepl("VarCorr\\s*\\(", text, perl = TRUE),
    paste0(script, " must extract ICC components directly from VarCorr().")
  )
  expect_true(
    !grepl("performance::icc\\s*\\(", text, perl = TRUE),
    paste0(script,
           " must not relabel performance::icc() aggregate output as a component ICC.")
  )
  expect_true(
    grepl("ICC_school\\s*=", text, perl = TRUE) &&
      grepl("ICC_class_within_school\\s*=", text, perl = TRUE) &&
      grepl("ICC_total_cluster\\s*=", text, perl = TRUE),
    paste0(script,
           " must report separate school, class-within-school, and total-cluster ICCs.")
  )
}

measurement_scripts <- c(
  "01_measurement-checks.Rmd",
  "osf_storage/scripts/01_measurement_checks.Rmd"
)
for (script in measurement_scripts) {
  text <- paste(readLines(script, warn = FALSE), collapse = "\n")
  expect_true(
    !grepl("mk_icc_3|ICC_school\\s*=|Step 6 -- Class-level ICCs", text,
           perl = TRUE),
    paste0(script,
           " must not duplicate the ICC analysis reported in the 02 script.")
  )
}

verify_text <- paste(readLines("submissions/pci_psychology/verify_statistics.R",
                               warn = FALSE), collapse = "\n")
expect_true(
  grepl("bifactor_fit\\s*<-\\s*cfa\\s*\\(", verify_text, perl = TRUE) &&
    grepl("orthogonal\\s*=\\s*TRUE", verify_text, perl = TRUE) &&
    grepl("bifactor_indices\\s*<-\\s*fitMeasures\\s*\\(", verify_text,
          perl = TRUE) &&
    grepl("cfi.scaled", verify_text, fixed = TRUE) &&
    grepl("rmsea.scaled", verify_text, fixed = TRUE) &&
    grepl("srmr", verify_text, fixed = TRUE) &&
    grepl("standardizedSolution\\s*\\(", verify_text, perl = TRUE) &&
    grepl("lavInspect\\s*\\(.*post.check", verify_text, perl = TRUE),
  "verify_statistics.R must report scaled orthogonal bifactor fit, standardized loadings, and admissibility diagnostics."
)

verify_output <- system2(
  "Rscript",
  c("--vanilla", "submissions/pci_psychology/verify_statistics.R"),
  stdout = TRUE,
  stderr = TRUE
)
expect_true(
  is.null(attr(verify_output, "status")) &&
    any(grepl("Orthogonal bifactor WLSMV CFA fit indices", verify_output,
              fixed = TRUE)) &&
    any(grepl("Bifactor admissibility: converged=TRUE, post.check=TRUE",
              verify_output, fixed = TRUE)),
  "verify_statistics.R must execute and print bifactor fit and admissibility output."
)

psy_arxiv_doi <- "https://doi.org/10.31234/osf.io/hxngm_v2"
manuscript_lines <- readLines("submissions/pci_psychology/manuscript.tex",
                              warn = FALSE)
manuscript_text <- paste(manuscript_lines, collapse = "\n")
expect_true(
  grepl("CFA showed good fit for the correlated three-factor model", manuscript_text,
        fixed = TRUE) &&
    grepl("data are compatible with the correlated three-factor specification",
          manuscript_text, fixed = TRUE) &&
    !grepl("analysis favored a correlated three-factor representation",
           manuscript_text, fixed = TRUE) &&
    !grepl("better fit of the three-factor model", manuscript_text,
           fixed = TRUE),
  "The manuscript must describe the correlated three-factor model as compatible, not favored over the bifactor model."
)
expect_true(
  grepl("bifactor fit indices were higher", manuscript_text, fixed = TRUE) &&
    grepl("model uncertainty limits structural\\s+claims", manuscript_text,
          perl = TRUE),
  "Section 5.2 must distinguish registered-alternative fit from the higher bifactor fit and its model uncertainty."
)

for (script in measurement_scripts) {
  text <- paste(readLines(script, warn = FALSE), collapse = "\n")
  expect_true(
    !grepl("ICC|intraclass|clustering|class_label|school_id", text,
           ignore.case = TRUE, perl = TRUE),
    paste0(script,
           " must not retain ICC-specific subtitle, description, comment, or setup references after ICCs moved to Report 02.")
  )
}

preprint_field <- manuscript_lines[grepl("Preprint DOI or URL", manuscript_lines,
                                          fixed = TRUE)]
expect_true(
  length(preprint_field) == 1L && any(grepl(psy_arxiv_doi, preprint_field,
                                             fixed = TRUE)),
  "The manuscript preprint field must contain the current PsyArXiv DOI."
)

top_lines <- readLines("submissions/pci_psychology/top_disclosure_table.md",
                       warn = FALSE)
preregistration_lines <- top_lines[grepl("Preregistration of", top_lines,
                                         fixed = TRUE)]
expect_true(
  length(preregistration_lines) == 2L &&
    all(grepl("https://osf.io/zsngy", preregistration_lines, fixed = TRUE)),
  "TOP preregistration disclosures must link to immutable OSF registration zsngy."
)

required_replication_files <- c(
  "osf_storage/supplementary_materials.md",
  "osf_storage/scripts/revision_reporting_analyses.R",
  "osf_storage/scripts/scoring_helpers.R"
)
for (path in required_replication_files) {
  expect_true(file.exists(path),
              paste0("Replication package must include ", path, "."))
}

if (length(test_failures)) {
  cat(c("Revision reporting regression checks failed:",
        paste0("- ", test_failures), ""),
      sep = "\n", file = stderr())
  stop("Revision reporting regression checks failed; see failures above.",
       call. = FALSE)
}

cat("revision reporting analysis helper tests passed\n")
