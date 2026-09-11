# Reviewer-requested reporting analyses for the PCI Psychology revision.
# Run from the osf_storage/scripts directory after copying
# student_responses.RDS into it:
#   Rscript revision_reporting_analyses.R

source("scoring_helpers.R")

fisher_ci <- function(r, n, conf_level = 0.95) {
  if (!is.finite(r) || n <= 3) {
    return(c(lower = NA_real_, upper = NA_real_))
  }
  critical <- stats::qnorm(1 - (1 - conf_level) / 2)
  interval <- atanh(r) + c(-1, 1) * critical / sqrt(n - 3)
  stats::setNames(tanh(interval), c("lower", "upper"))
}

mosier_two_component <- function(reliability_1, reliability_2,
                                 correlation) {
  (reliability_1 + reliability_2 + 2 * correlation) /
    (2 + 2 * correlation)
}

sum_missing_as_zero <- function(items) {
  rowSums(as.data.frame(lapply(items, as.numeric)), na.rm = TRUE)
}

alpha_summary <- function(items, ordinal = TRUE) {
  items <- items[stats::complete.cases(items), , drop = FALSE]
  complete_n <- nrow(items)
  raw <- suppressWarnings(psych::alpha(items, warnings = FALSE))
  alpha <- unname(raw$total$raw_alpha)
  ase <- unname(raw$total$ase)
  ci <- pmax(0, pmin(1, alpha + c(-1, 1) * stats::qnorm(.975) * ase))

  poly_alpha <- NA_real_
  omega_total <- NA_real_
  if (ordinal) {
    rho <- suppressWarnings(psych::polychoric(items)$rho)
    if (min(eigen(rho, symmetric = TRUE)$values) <= 1e-6) {
      rho <- psych::cor.smooth(rho)
    }
    poly_alpha <- suppressWarnings(
      psych::alpha(rho, n.obs = complete_n, warnings = FALSE)$total$raw_alpha
    )
    omega_total <- suppressWarnings(
      psych::omega(rho, n.obs = complete_n, nfactors = 1,
                   plot = FALSE)$omega.tot
    )
  }

  c(
    n = complete_n,
    alpha_raw = alpha,
    alpha_ci_lower = ci[1],
    alpha_ci_upper = ci[2],
    alpha_polychoric = poly_alpha,
    omega_total = omega_total
  )
}

bootstrap_ideology_reliability <- function(data, fr_items, ksa_items,
                                           iterations = 2000, seed = 20260910) {
  estimate <- function(rows) {
    sample <- data[rows, , drop = FALSE]
    fr_rel <- suppressWarnings(
      psych::alpha(sample[fr_items], warnings = FALSE)$total$raw_alpha
    )
    ksa_rel <- suppressWarnings(
      psych::alpha(sample[ksa_items], warnings = FALSE)$total$raw_alpha
    )
    fr_score <- scale_mean(sample, fr_items, 4)
    ksa_score <- scale_mean(sample, ksa_items, 7)
    component_r <- stats::cor(fr_score, ksa_score,
                              use = "pairwise.complete.obs")
    mosier_two_component(fr_rel, ksa_rel, component_r)
  }

  set.seed(seed)
  boot <- replicate(iterations, estimate(sample.int(nrow(data), replace = TRUE)))
  c(
    estimate = estimate(seq_len(nrow(data))),
    lower = stats::quantile(boot, .025, na.rm = TRUE),
    upper = stats::quantile(boot, .975, na.rm = TRUE)
  )
}

score_data <- function(data) {
  pop <- paste0("POP", 1:3)
  for (item in pop) {
    data[[paste0(item, "_rev")]] <- 5 - as.numeric(data[[item]])
  }

  fr_items <- c(paste0("RD", 1:3), paste0("NS", 1:3))
  ksa_items <- c(paste0("A", 1:3), paste0("U", 1:3), paste0("K", 1:3))
  data$HPT_composite <- scale_mean(data, paste0(pop, "_rev"), 2) / 2 +
    scale_mean(data, paste0("CONT", 1:3), 2) / 2
  data$Contextualization <- scale_mean(data, paste0("CONT", 1:3), 2)
  data$Reversed_presentism <- scale_mean(data, paste0(pop, "_rev"), 2)
  data$Role_of_agent <- scale_mean(data, paste0("ROA", 1:3), 2)
  data$Historical_knowledge <- sum_missing_as_zero(data[paste0("KN", 1:6)])
  data$FR_LF <- scale_mean(data, fr_items, 4)
  data$KSA_3 <- scale_mean(data, ksa_items, 7)
  data$Social_desirability <- scale_mean(data, paste0("SDR", 1:5), 4)
  data$Ideology_composite <- rowMeans(
    cbind(as.numeric(scale(data$FR_LF)), as.numeric(scale(data$KSA_3))),
    na.rm = FALSE
  )
  data
}

write_reliability_outputs <- function(data, output_dir) {
  data <- score_data(data)
  scales <- list(
    "HPT: reversed present-oriented perspective" = list(paste0("POP", 1:3, "_rev"), TRUE),
    "HPT: role of the historical agent" = list(paste0("ROA", 1:3), TRUE),
    "HPT: contextualization" = list(paste0("CONT", 1:3), TRUE),
    "HPT composite (6 items)" = list(
      c(paste0("POP", 1:3, "_rev"), paste0("CONT", 1:3)), TRUE
    ),
    "HPT total (9 items)" = list(
      c(paste0("POP", 1:3, "_rev"), paste0("ROA", 1:3),
        paste0("CONT", 1:3)), TRUE
    ),
    "Historical knowledge (KR-20)" = list(paste0("KN", 1:6), FALSE),
    "FR-LF: dictatorship acceptance" = list(paste0("RD", 1:3), TRUE),
    "FR-LF: Nazi-crime relativization" = list(paste0("NS", 1:3), TRUE),
    "FR-LF total" = list(
      c(paste0("RD", 1:3), paste0("NS", 1:3)), TRUE
    ),
    "KSA-3: aggression" = list(paste0("A", 1:3), TRUE),
    "KSA-3: submission" = list(paste0("U", 1:3), TRUE),
    "KSA-3: conventionalism" = list(paste0("K", 1:3), TRUE),
    "KSA-3 total" = list(
      c(paste0("A", 1:3), paste0("U", 1:3), paste0("K", 1:3)), TRUE
    ),
    "Social desirability" = list(paste0("SDR", 1:5), TRUE)
  )

  rows <- lapply(names(scales), function(name) {
    spec <- scales[[name]]
    values <- alpha_summary(data[spec[[1]]], ordinal = spec[[2]])
    data.frame(scale = name, items = length(spec[[1]]),
               t(values), row.names = NULL)
  })
  reliability <- do.call(rbind, rows)
  reliability$method <- ifelse(
    reliability$scale == "Historical knowledge (KR-20)",
    "KR-20 (raw alpha for dichotomous items)",
    "Raw alpha; polychoric alpha and omega for ordinal items"
  )

  fr_items <- c(paste0("RD", 1:3), paste0("NS", 1:3))
  ksa_items <- c(paste0("A", 1:3), paste0("U", 1:3), paste0("K", 1:3))
  ideology <- bootstrap_ideology_reliability(data, fr_items, ksa_items)
  reliability <- rbind(
    reliability,
    data.frame(
      scale = "Combined ideology composite",
      items = 2,
      n = sum(stats::complete.cases(data[c("FR_LF", "KSA_3")])),
      alpha_raw = ideology[["estimate"]],
      alpha_ci_lower = ideology[["lower.2.5%"]],
      alpha_ci_upper = ideology[["upper.97.5%"]],
      alpha_polychoric = NA_real_,
      omega_total = NA_real_,
      method = paste("Mosier reliability for the mean of standardized FR-LF",
                     "and KSA-3 scores; percentile bootstrap CI")
    )
  )

  numeric <- vapply(reliability, is.numeric, logical(1))
  reliability[numeric] <- lapply(reliability[numeric], round, 3)
  utils::write.csv(reliability,
                   file.path(output_dir, "revision_reliability.csv"),
                   row.names = FALSE, na = "")

  facilities <- data.frame(
    item = paste0("KN", 1:6),
    facility = vapply(data[paste0("KN", 1:6)], mean,
                      numeric(1), na.rm = TRUE)
  )
  utils::write.csv(facilities,
                   file.path(output_dir, "revision_knowledge_facilities.csv"),
                   row.names = FALSE)
}

write_correlation_outputs <- function(data, output_dir) {
  scored <- score_data(data)
  variables <- c(
    "HPT composite" = "HPT_composite",
    "Contextualization" = "Contextualization",
    "Present-oriented perspective (reversed)" = "Reversed_presentism",
    "Historical knowledge" = "Historical_knowledge",
    "Right-authoritarian attitudes" = "FR_LF",
    "Authoritarianism" = "KSA_3",
    "Social desirability" = "Social_desirability"
  )

  pairs <- utils::combn(seq_along(variables), 2)
  rows <- apply(pairs, 2, function(indices) {
    x <- scored[[variables[[indices[1]]]]]
    y <- scored[[variables[[indices[2]]]]]
    keep <- stats::complete.cases(x, y)
    r <- stats::cor(x[keep], y[keep])
    ci <- fisher_ci(r, sum(keep))
    data.frame(
      variable_1 = names(variables)[indices[1]],
      variable_2 = names(variables)[indices[2]],
      n = sum(keep), r = r,
      ci_lower = ci[["lower"]], ci_upper = ci[["upper"]]
    )
  })
  correlations <- do.call(rbind, rows)
  correlations[c("r", "ci_lower", "ci_upper")] <- lapply(
    correlations[c("r", "ci_lower", "ci_upper")], round, 3
  )
  utils::write.csv(correlations,
                   file.path(output_dir, "revision_correlation_intervals.csv"),
                   row.names = FALSE)
}

write_descriptive_outputs <- function(data, output_dir) {
  scored <- score_data(data)
  targets <- c(
    "Combined ideology (standardized)" = "Ideology_composite",
    "FR-LF attitudes" = "FR_LF",
    "KSA-3 authoritarianism" = "KSA_3",
    "Historical knowledge" = "Historical_knowledge",
    "History grade" = "history_grade"
  )
  rows <- lapply(names(targets), function(name) {
    x <- as.numeric(scored[[targets[[name]]]])
    q <- stats::quantile(x, c(.25, .5, .75), na.rm = TRUE)
    data.frame(
      variable = name, n = sum(!is.na(x)), mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE), min = min(x, na.rm = TRUE),
      q25 = q[1], median = q[2], q75 = q[3], max = max(x, na.rm = TRUE),
      floor_percent = mean(x == min(x, na.rm = TRUE), na.rm = TRUE) * 100,
      ceiling_percent = mean(x == max(x, na.rm = TRUE), na.rm = TRUE) * 100,
      row.names = NULL
    )
  })
  descriptives <- do.call(rbind, rows)
  numeric <- vapply(descriptives, is.numeric, logical(1))
  descriptives[numeric] <- lapply(descriptives[numeric], round, 3)
  utils::write.csv(descriptives,
                   file.path(output_dir, "revision_participant_descriptives.csv"),
                   row.names = FALSE)

  thresholds <- data.frame(
    measure = c("FR-LF attitudes", "KSA-3 authoritarianism",
                "Mean of raw FR-LF and KSA-3 scores"),
    threshold = 3,
    denominator = c(sum(!is.na(scored$FR_LF)), sum(!is.na(scored$KSA_3)),
                    sum(stats::complete.cases(scored[c("FR_LF", "KSA_3")]))),
    above_threshold = c(sum(scored$FR_LF > 3, na.rm = TRUE),
                        sum(scored$KSA_3 > 3, na.rm = TRUE),
                        sum(rowMeans(scored[c("FR_LF", "KSA_3")]) > 3,
                            na.rm = TRUE))
  )
  thresholds$percent <- round(
    100 * thresholds$above_threshold / thresholds$denominator, 1
  )
  utils::write.csv(thresholds,
                   file.path(output_dir, "revision_ideology_thresholds.csv"),
                   row.names = FALSE)

  levels <- do.call(rbind, lapply(c("school_level", "gender"), function(name) {
    counts <- table(scored[[name]], useNA = "ifany")
    data.frame(variable = name, level = names(counts), n = as.integer(counts),
               percent = round(100 * as.integer(counts) / sum(counts), 1))
  }))
  utils::write.csv(levels,
                   file.path(output_dir, "revision_participant_counts.csv"),
                   row.names = FALSE)
}

prepare_relationship_data <- function(ideology, knowledge, hpt) {
  plot_data <- rbind(
    data.frame(
      panel = "A. Ideology and historical perspective-taking",
      predictor = ideology,
      outcome = hpt
    ),
    data.frame(
      panel = "B. Historical knowledge and historical perspective-taking",
      predictor = knowledge,
      outcome = hpt
    )
  )
  plot_data[stats::complete.cases(plot_data), , drop = FALSE]
}

write_relationship_figure <- function(data, output_dir) {
  scored <- score_data(data)
  plot_data <- prepare_relationship_data(
    scored$Ideology_composite,
    scored$Historical_knowledge,
    scored$HPT_composite
  )
  panel_n <- aggregate(outcome ~ panel, plot_data,
                       function(x) sum(!is.na(x)))
  panel_labels <- setNames(
    paste0(panel_n$panel, " (n = ", panel_n$outcome, ")"), panel_n$panel
  )

  figure <- ggplot2::ggplot(plot_data,
                            ggplot2::aes(x = predictor, y = outcome)) +
    ggplot2::geom_point(
      alpha = .35, size = 1.4,
      position = ggplot2::position_jitter(width = .025, height = .015)
    ) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x,
                         colour = "black", fill = "grey70", linewidth = .7) +
    ggplot2::facet_wrap(
      ~panel, scales = "free_x",
      labeller = ggplot2::labeller(panel = panel_labels)
    ) +
    ggplot2::labs(
      x = "Predictor score",
      y = "Historical perspective-taking composite (1-4)",
      caption = paste("Points are jittered to show overlapping observations;",
                      "lines and 95% confidence bands are unadjusted.")
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(legend.position = "none")

  ggplot2::ggsave(file.path(output_dir, "revision_relationships.pdf"),
                  figure, width = 7.2, height = 3.6, units = "in")
  ggplot2::ggsave(file.path(output_dir, "revision_relationships.png"),
                  figure, width = 7.2, height = 3.6, units = "in", dpi = 300)
}

main <- function() {
  suppressPackageStartupMessages({
    library(psych)
    library(ggplot2)
  })
  normalised_responses <- readRDS("student_responses.RDS")
  output_dir <- "../outputs/revision"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  write_reliability_outputs(normalised_responses, output_dir)
  write_correlation_outputs(normalised_responses, output_dir)
  write_descriptive_outputs(normalised_responses, output_dir)
  write_relationship_figure(normalised_responses, output_dir)
  message("Revision reporting outputs written to ", output_dir)
}

if (sys.nframe() == 0L) {
  main()
}
