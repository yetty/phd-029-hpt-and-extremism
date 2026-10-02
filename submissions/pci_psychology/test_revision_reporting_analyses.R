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

readme_path <- "osf_storage/README.md"
readme_text <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
readme_compact <- gsub("[[:space:]]+", " ", readme_text)
manuscript_title <- paste(
  "Cross-Cultural Validation and Ideological Fairness of a Historical",
  "Perspective Taking Instrument: Evidence from Czech Secondary Students"
)
expect_true(
  grepl(manuscript_title, readme_compact, fixed = TRUE) &&
    grepl("Institute of History, Faculty of Arts, Charles University",
          readme_text, fixed = TRUE) &&
    grepl("https://doi.org/10.31234/osf.io/hxngm_v2", readme_text,
          fixed = TRUE) &&
    grepl("https://doi.org/10.17605/OSF.IO/YNG37", readme_text,
          fixed = TRUE) &&
    grepl("https://osf.io/zsngy", readme_text, fixed = TRUE),
  "The replication README must contain the current title, affiliation, preprint DOI, OSF project, and immutable registration."
)
expect_true(
  !grepl("approved by|reviewed and approved|parental|parent or legal guardian",
         readme_text, ignore.case = TRUE, perl = TRUE) &&
    grepl("ethics committee approval is not required", readme_text,
          fixed = TRUE) &&
    grepl("Participation was voluntary", readme_text, fixed = TRUE) &&
    grepl("de-identified at the point of data entry", readme_text,
          fixed = TRUE),
  "The replication README ethics statement must match the manuscript without approval or parental-consent claims."
)
expect_true(
  grepl("planned archival migration to Zenodo", readme_text, fixed = TRUE) &&
    !grepl("upload to Zenodo|Zenodo deposit has been", readme_text,
           ignore.case = TRUE, perl = TRUE),
  "The replication README must use repository-neutral, future-tense Zenodo wording."
)

analytic_rmds <- c(
  "01_measurement_checks.Rmd",
  "02_descriptives_and_zero_order_correlations.Rmd",
  "03_multilevel_models_hypothesis_tests.Rmd",
  "04_dif_and_mg_cfa_measurement_bias.Rmd",
  "05_sensitivity_analyses.Rmd"
)
documentation_rmds <- c(
  "06_appendix_tables_and_figures.Rmd",
  "07_reproducibility_report.Rmd"
)
required_scripts <- c(
  analytic_rmds, documentation_rmds,
  "scoring_helpers.R", "revision_reporting_analyses.R",
  "supplementary_analyses.R", "tost_equivalence_tests_and_mundlak.R"
)
for (script in required_scripts) {
  expect_true(
    file.exists(file.path("osf_storage/scripts", script)) &&
      grepl(script, readme_text, fixed = TRUE),
    paste0("The replication README must accurately inventory ", script, ".")
  )
}
expect_true(
  file.exists("osf_storage/supplementary_materials.md") &&
    file.exists("osf_storage/supplementary_materials.pdf") &&
    file.exists("osf_storage/instrument_adaptation_and_deviations.md") &&
    grepl("Table S5", paste(readLines(
      "osf_storage/instrument_adaptation_and_deviations.md", warn = FALSE
    ), collapse = "\n"), fixed = TRUE),
  "The replication package must include the supplement and adaptation/deviation documentation that points to Table S5."
)

for (script in c(analytic_rmds, documentation_rmds)) {
  output <- file.path(
    "osf_storage/outputs",
    sub("\\.Rmd$", ".pdf", script)
  )
  source <- file.path("osf_storage/scripts", script)
  expect_true(
    file.exists(output) && file.info(output)$mtime > file.info(source)$mtime,
    paste0("Replication output ", output,
           " must be newer than its source script.")
  )
}

pdf_text <- function(path) {
  output <- system2("pdftotext", c("-layout", path, "-"), stdout = TRUE,
                    stderr = TRUE)
  expect_true(is.null(attr(output, "status")),
              paste0("pdftotext must read ", path, "."))
  paste(output, collapse = "\n")
}

supplement_pdf_text <- pdf_text("osf_storage/supplementary_materials.pdf")
expect_true(
  grepl("familywise", supplement_pdf_text, ignore.case = TRUE) &&
    grepl(".05", supplement_pdf_text, fixed = TRUE) &&
    grepl("Benjamini-Hochberg", supplement_pdf_text, fixed = TRUE) &&
    grepl("Orthogonal bifactor CFA", supplement_pdf_text, fixed = TRUE) &&
    !grepl("alpha = .01", supplement_pdf_text, fixed = TRUE),
  "The packaged supplement PDF must contain the current familywise-alpha, Benjamini-Hochberg, and orthogonal-bifactor wording."
)

dif_script <- paste(readLines(
  "osf_storage/scripts/04_dif_and_mg_cfa_measurement_bias.Rmd",
  warn = FALSE
), collapse = "\n")
expect_true(
  grepl("familywise alpha = .05", readme_text, fixed = TRUE) &&
    grepl("familywise alpha = .05", dif_script, fixed = TRUE) &&
    grepl("adj_p < .05", dif_script, fixed = TRUE),
  "The README and DIF script must agree on familywise alpha .05 and adjusted-p flags."
)

dif_pdf_text <- pdf_text("osf_storage/outputs/04_dif_and_mg_cfa_measurement_bias.pdf")
dif_table_start <- regexpr("DIF omnibus", dif_pdf_text, fixed = TRUE)[1]
dif_table_text <- if (dif_table_start > 0L) {
  substr(dif_pdf_text, dif_table_start, dif_table_start + 3000L)
} else {
  ""
}
expect_true(
  nzchar(dif_table_text) && !grepl("\\bNA\\b", dif_table_text) &&
    grepl("POP1", dif_table_text, fixed = TRUE) &&
    grepl("0.033", dif_table_text, fixed = TRUE) &&
    grepl("0.300", dif_table_text, fixed = TRUE),
  "The refreshed DIF PDF must show non-missing omnibus p-values, including POP1 raw .033 and adjusted .300."
)

reproducibility_pdf_text <- pdf_text("osf_storage/outputs/07_reproducibility_report.pdf")
expect_true(
  !grepl("tidyverse[[:space:]]+not installed", reproducibility_pdf_text,
         perl = TRUE) &&
    !grepl("semTools[[:space:]]+not installed", reproducibility_pdf_text,
           perl = TRUE),
  "The reproducibility PDF must record installed tidyverse and semTools versions."
)

appendix_script <- paste(readLines(
  "osf_storage/scripts/06_appendix_tables_and_figures.Rmd", warn = FALSE
), collapse = "\n")
expect_true(
  grepl("mod2values\\(mod_base\\)", dif_script, perl = TRUE) &&
    grepl("IRTpars = FALSE", dif_script, fixed = TRUE) &&
    grepl("Table S1", dif_script, fixed = TRUE) &&
    grepl("Table S1", readme_text, fixed = TRUE) &&
    grepl("04_dif_and_mg_cfa_measurement_bias.Rmd", appendix_script,
          fixed = TRUE),
  "The Table S1 source and mappings must identify the constrained GRM extraction script."
)

table_s1_export <- "osf_storage/outputs/table_s1_irt_parameters.csv"
table_s1_reference <- "submissions/pci_psychology/table_s1_irt_parameters.csv"
if (file.exists(table_s1_export) && file.exists(table_s1_reference)) {
  exported <- read.csv(table_s1_export, check.names = FALSE)
  reference <- read.csv(table_s1_reference, check.names = FALSE)
  expect_true(
    identical(exported, reference),
    "The constrained-GRM Table S1 export must match the current supplementary Table S1 CSV."
  )
} else {
  expect_true(FALSE,
              "The package and submission must both contain the Table S1 CSV export.")
}

osf_makefile <- paste(readLines("osf_storage/scripts/Makefile", warn = FALSE),
                     collapse = "\n")
expect_true(
  grepl("output_format='pdf_document'", osf_makefile, fixed = TRUE),
  "The OSF Makefile must render only the tracked PDF report format."
)

osf_generated_artifacts <- list.files(
  "osf_storage/outputs",
  pattern = "\\.(md|tex|log)$|_files$",
  full.names = TRUE
)
expect_true(
  !length(osf_generated_artifacts),
  "The OSF output directory must not retain generated Markdown, TeX, log, or _files artifacts."
)

reproducibility_script <- paste(readLines(
  "osf_storage/scripts/07_reproducibility_report.Rmd", warn = FALSE
), collapse = "\n")
expect_true(
  grepl("File = sub", reproducibility_script, fixed = TRUE) &&
    grepl("SHA256 = sub", reproducibility_script, fixed = TRUE),
  "The checksum report must separate file names from SHA-256 values."
)

current_package_sources <- c(
  "osf_storage/README.md",
  "osf_storage/supplementary_materials.md",
  "osf_storage/instrument_adaptation_and_deviations.md",
  file.path("osf_storage/scripts", c(analytic_rmds, documentation_rmds)),
  file.path("osf_storage/scripts", c(
    "fig02_measurement_invariance_and_dif.R",
    "fig03_score_distributions.R",
    "fig04_coefficient_plot.R",
    "fig05_marginal_effects.R"
  ))
)
current_package_text <- paste(
  unlist(lapply(current_package_sources, readLines, warn = FALSE)),
  collapse = "\n"
)
expect_true(
  !grepl("PCI RR|Registered Report|Stage[[:space:]]+[12]",
         current_package_text, perl = TRUE),
  "Current package sources must not use PCI RR, Registered Report, or Stage 1/2 language."
)

dif_sources <- c(
  "04_dif-and-mg-cfa-hpt-bias.Rmd",
  "osf_storage/scripts/04_dif_and_mg_cfa_measurement_bias.Rmd"
)
for (path in dif_sources) {
  dif_text <- gsub("[[:space:]]+", " ", paste(readLines(
    path, warn = FALSE
  ), collapse = "\n"))
  expect_true(
    grepl("registered H4", dif_text, fixed = TRUE) &&
      grepl("positive ideology-related DIF on CONT items", dif_text,
            fixed = TRUE) &&
      grepl("do not implement the registered continuous-ideology MIMIC analysis",
            dif_text, fixed = TRUE) &&
      !grepl("supports H1", dif_text, fixed = TRUE),
    paste0(path, " must distinguish registered H4 from the current post-registration GRM DIF/MG-CFA analyses.")
  )
}

report03_sources <- c(
  "03_multilevel-models-hypothesis-tests.Rmd",
  "osf_storage/scripts/03_multilevel_models_hypothesis_tests.Rmd"
)
for (path in report03_sources) {
  report03_text <- gsub("[[:space:]]+", " ", paste(readLines(
    path, warn = FALSE
  ), collapse = "\n"))
  expect_true(
    !grepl("main confirmatory", report03_text, fixed = TRUE) &&
      grepl("registered hypotheses H1 and H2", report03_text, fixed = TRUE) &&
      grepl("Table S5", report03_text, fixed = TRUE) &&
      !grepl("H1 supported|H2 supported", report03_text, perl = TRUE),
    paste0(path, " must describe its relationship to registered H1/H2 and Table S5 without a misleading confirmatory label.")
  )
}

fig02_source <- "osf_storage/scripts/fig02_measurement_invariance_and_dif.R"
fig02_text <- paste(readLines(fig02_source, warn = FALSE), collapse = "\n")
expect_true(
  grepl("familywise alpha = .05", fig02_text, fixed = TRUE) &&
    grepl("Bonferroni-adjusted p-values", fig02_text, fixed = TRUE) &&
    !grepl("alpha = .01", fig02_text, fixed = TRUE),
  "The Figure 2 caption must report familywise alpha .05 and Bonferroni-adjusted p-values, not alpha .01."
)

figure_scripts <- c(
  "fig02_measurement_invariance_and_dif.R",
  "fig03_score_distributions.R",
  "fig04_coefficient_plot.R",
  "fig05_marginal_effects.R"
)
for (script in figure_scripts) {
  path <- file.path("osf_storage/scripts", script)
  figure_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  figure_stem <- sub("\\.R$", "", script)
  artifacts <- file.path("osf_storage/figures",
                         paste0(figure_stem, c(".pdf", ".png")))
  expect_true(
    !grepl("trse_outputs", figure_text, fixed = TRUE) &&
      grepl("../figures", figure_text, fixed = TRUE) &&
      all(file.exists(artifacts)) &&
      all(file.info(artifacts)$mtime > file.info(path)$mtime),
    paste0(script, " must document ../figures output paths and have current PDF/PNG artifacts.")
  )
}

stale_development_outputs <- file.path("osf_storage/scripts", c(
  "instrument_reliability_summary.csv", "factor_and_invariance_summary.txt",
  "fig_pca_scree.png"
))
expect_true(
  !any(file.exists(stale_development_outputs)),
  "Uninventoried development-script outputs must be excluded from the public package."
)

codebook_sources <- c(
  "normalised_responses_codebook.tex", "osf_storage/data/codebook_source.tex"
)
for (path in codebook_sources) {
  codebook_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_true(
    grepl("\\texttt{lower\\_secondary}", codebook_text, fixed = TRUE) &&
      grepl("\\texttt{upper\\_secondary}", codebook_text, fixed = TRUE) &&
      grepl("Anonymized class code", codebook_text, fixed = TRUE) &&
      !grepl("gymnasium|gymnázium", codebook_text, ignore.case = TRUE),
    paste0(path, " must describe school_level and class_label using only public anonymized codes.")
  )
}

for (path in c("normalised_responses_codebook.pdf", "osf_storage/data/codebook.pdf")) {
  codebook_pdf_text <- pdf_text(path)
  expect_true(
    !grepl("gymnasium|gymnázium", codebook_pdf_text, ignore.case = TRUE) &&
      grepl("lower.secondary", codebook_pdf_text, perl = TRUE) &&
      grepl("Anonymized class code", codebook_pdf_text, fixed = TRUE),
    paste0(path, " must contain the corrected public school-level and class-label descriptions.")
  )
}

report03_pdf_text <- pdf_text(
  "osf_storage/outputs/03_multilevel_models_hypothesis_tests.pdf"
)
expect_true(
  !grepl("main confirmatory", report03_pdf_text, fixed = TRUE) &&
    grepl("Table S5", report03_pdf_text, fixed = TRUE),
  "The rendered Report 03 PDF must not use the stale main-confirmatory framing."
)

if (length(test_failures)) {
  cat(c("Revision reporting regression checks failed:",
        paste0("- ", test_failures), ""),
      sep = "\n", file = stderr())
  stop("Revision reporting regression checks failed; see failures above.",
       call. = FALSE)
}

cat("revision reporting analysis helper tests passed\n")
