failures <- character()
expect_true <- function(condition, message) {
  if (!isTRUE(condition)) failures <<- c(failures, message)
}

script_path <- "sanitize_class_identifiers.R"
expect_true(
  file.exists(script_path),
  "The tracked class-identifier sanitizer script must exist."
)

if (file.exists(script_path)) {
  sanitizer <- new.env(parent = globalenv())
  source(script_path, local = sanitizer)

  fixture <- data.frame(
    school_id = factor(c("S02", "S01", "S02")),
    class_label = factor(c("legacy_two", "legacy_one", "legacy_two")),
    score = c(1.5, 2.5, 3.5),
    gender = factor(c("F", "M", "F"))
  )
  fixture_sanitized <- sanitizer$sanitize_class_identifiers(fixture)
  fixture_non_id <- setdiff(names(fixture), c("class_label", "class_id"))
  expect_true(
    identical(fixture[fixture_non_id], fixture_sanitized[fixture_non_id]) &&
      identical(as.character(fixture_sanitized$class_label),
                as.character(fixture_sanitized$class_id)) &&
      all(grepl("^C[0-9]{2}$",
                as.character(fixture_sanitized$class_label))) &&
      all(grepl("^C[0-9]{2}$",
                as.character(fixture_sanitized$class_id))),
    "The sanitizer must alter only ID fields and assign matching opaque C-codes."
  )

  root_rds <- readRDS("normalised_responses.RDS")
  rdata_environment <- new.env(parent = emptyenv())
  load("normalised_responses.RData", envir = rdata_environment)
  root_rdata <- rdata_environment$normalised_responses
  osf_rds <- readRDS("osf_storage/data/student_responses.RDS")
  non_id <- setdiff(names(root_rds), c("class_label", "class_id"))

  expect_true(
    identical(root_rds, root_rdata) && identical(root_rds, osf_rds) &&
      identical(root_rds[non_id], root_rdata[non_id]) &&
      identical(root_rds[non_id], osf_rds[non_id]),
    "Root RDS/RData and public RDS must be equivalent with identical non-ID data."
  )
  expect_true(
    nrow(unique(root_rds[c("school_id", "class_label")])) == 20L &&
      identical(sort(unique(as.character(root_rds$class_label))),
                sprintf("C%02d", 1:20)) &&
      identical(as.character(root_rds$class_label),
                as.character(root_rds$class_id)) &&
       all(grepl("^C[0-9]{2}$", as.character(root_rds$class_label))),
    "Tracked data must retain 20 clusters while using only opaque class labels and IDs."
  )

  spreadsheet_matches_rds <- function(path, data) {
    spreadsheet <- as.data.frame(readxl::read_xlsx(path, sheet = 1),
                                 stringsAsFactors = FALSE)
    if (!identical(names(spreadsheet), names(data)) ||
        nrow(spreadsheet) != nrow(data)) {
      return(FALSE)
    }

    all(vapply(names(data), function(name) {
      expected <- data[[name]]
      observed <- spreadsheet[[name]]
      if (is.factor(expected)) {
        return(identical(as.character(expected), as.character(observed)))
      }
      if (is.numeric(expected)) {
        missing_match <- identical(is.na(expected), is.na(observed))
        value_match <- all(abs(expected[!is.na(expected)] -
                               observed[!is.na(observed)]) < 1e-12)
        return(missing_match && value_match)
      }
      identical(expected, observed)
    }, logical(1)))
  }
  expect_true(
    spreadsheet_matches_rds("normalised_responses.xlsx", root_rds) &&
      spreadsheet_matches_rds("osf_storage/data/student_responses.xlsx", root_rds),
    "Root and public XLSX files must contain the same sanitized values as RDS."
  )
}

if (length(failures)) {
  cat(c("Class-identifier sanitization checks failed:",
        paste0("- ", failures), ""), sep = "\n", file = stderr())
  stop("Class-identifier sanitization checks failed; see failures above.",
       call. = FALSE)
}

cat("class-identifier sanitization checks passed\n")
