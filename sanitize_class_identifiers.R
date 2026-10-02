# Replace source class names with deterministic opaque cluster identifiers.
# Run from the project root: Rscript sanitize_class_identifiers.R

required_columns <- c("school_id", "class_label")

class_identifier_mapping <- function(data) {
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns)) {
    stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
  }

  school <- as.character(data$school_id)
  class <- as.character(data$class_label)
  if (anyNA(school) || anyNA(class)) {
    stop("school_id and class_label must be non-missing before sanitization.")
  }

  pairs <- unique(data.frame(school_id = school, class_label = class,
                             stringsAsFactors = FALSE))
  pairs <- pairs[order(pairs$school_id, pairs$class_label), , drop = FALSE]
  pairs$opaque_class <- sprintf("C%02d", seq_len(nrow(pairs)))
  pairs
}

sanitize_class_identifiers <- function(data) {
  pairs <- class_identifier_mapping(data)
  school <- as.character(data$school_id)
  class <- as.character(data$class_label)

  pair_key <- function(school_id, class_label) {
    paste(school_id, class_label, sep = "\r")
  }
  lookup <- stats::setNames(
    pairs$opaque_class,
    pair_key(pairs$school_id, pairs$class_label)
  )
  opaque_class <- unname(lookup[pair_key(school, class)])
  opaque_levels <- pairs$opaque_class

  data$class_label <- factor(opaque_class, levels = opaque_levels)
  data$class_id <- factor(opaque_class, levels = opaque_levels)

  if (nrow(unique(data[c("school_id", "class_label")])) != nrow(pairs) ||
      !identical(as.character(data$class_label), as.character(data$class_id))) {
    stop("Sanitization did not preserve one opaque code per school/class pair.")
  }
  data
}

read_normalised_rdata <- function(path) {
  environment <- new.env(parent = emptyenv())
  load(path, envir = environment)
  if (!exists("normalised_responses", envir = environment, inherits = FALSE)) {
    stop("RData file does not contain normalised_responses: ", path)
  }
  environment$normalised_responses
}

write_sanitized_data <- function(project_root = ".") {
  project_root <- normalizePath(project_root)
  root_rds <- file.path(project_root, "normalised_responses.RDS")
  root_rdata <- file.path(project_root, "normalised_responses.RData")
  root_xlsx <- file.path(project_root, "normalised_responses.xlsx")
  osf_rds <- file.path(project_root, "osf_storage/data/student_responses.RDS")
  osf_xlsx <- file.path(project_root, "osf_storage/data/student_responses.xlsx")

  original_rds <- readRDS(root_rds)
  original_rdata <- read_normalised_rdata(root_rdata)
  rds_without_class_id <- original_rds[setdiff(names(original_rds), "class_id")]
  if (!identical(rds_without_class_id, original_rdata)) {
    stop("Root RDS and RData inputs differ; refusing to sanitize inconsistently.")
  }

  sanitized <- sanitize_class_identifiers(original_rds)
  non_id <- setdiff(names(original_rds), c("class_label", "class_id"))
  if (!identical(original_rds[non_id], sanitized[non_id])) {
    stop("Non-identifier data changed during sanitization.")
  }

  mapping <- class_identifier_mapping(original_rds)
  mapping_path <- tempfile("class-identifier-map-", fileext = ".csv")
  utils::write.csv(mapping, mapping_path, row.names = FALSE)

  temp_path <- function(path) {
    tempfile(paste0(".", basename(path), "-"), tmpdir = dirname(path))
  }
  temporary <- c(
    root_rds = temp_path(root_rds),
    root_rdata = temp_path(root_rdata),
    root_xlsx = temp_path(root_xlsx),
    osf_rds = temp_path(osf_rds),
    osf_xlsx = temp_path(osf_xlsx)
  )
  on.exit(unlink(c(mapping_path, temporary), force = TRUE), add = TRUE)

  normalised_responses <- sanitized
  saveRDS(normalised_responses, temporary[["root_rds"]])
  save(normalised_responses, file = temporary[["root_rdata"]])
  saveRDS(normalised_responses, temporary[["osf_rds"]])

  # Do not normalize this path: resolving the virtualenv interpreter symlink
  # loses its site-packages and therefore openpyxl.
  python <- file.path(project_root, "../../.venv/bin/python")
  xlsx_updater <- file.path(project_root, "update_class_identifier_xlsx.py")
  for (paths in list(c(root_xlsx, temporary[["root_xlsx"]]),
                     c(osf_xlsx, temporary[["osf_xlsx"]]))) {
    output <- system2(python, c(xlsx_updater, paths, mapping_path),
                      stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(output, "status"))) {
      stop("XLSX sanitization failed: ", paste(output, collapse = "\n"))
    }
  }

  destinations <- c(root_rds, root_rdata, root_xlsx, osf_rds, osf_xlsx)
  if (!all(file.rename(temporary, destinations))) {
    stop("Unable to replace one or more sanitized data files.")
  }

  invisible(normalised_responses)
}

if (identical(environment(), globalenv()) && !interactive()) {
  write_sanitized_data()
}
