# =============================================================================
# FIGURE 3: Distribution of HPT and Ideology Scores
#
# Two-row panel figure:
#   Row 1 — HPT subscale and composite distributions (POP_rev, ROA, CONT, CTX6)
#   Row 2 — Ideology and control distributions (FR-LF, KSA-3, KN, SDR-5)
#
# Output:  trse_outputs/fig03_score_distributions.pdf  (vector, 210 × 150 mm)
#          trse_outputs/fig03_score_distributions.png  (300 DPI raster)
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(patchwork)

dir.create("../figures", showWarnings = FALSE)
# ---- 1. Data preparation (mirrors 02_descriptives_and_zero_order_correlations.Rmd) -

normalised_responses <- readRDS("student_responses.RDS")
stopifnot(exists("normalised_responses"))

dat_raw <- normalised_responses |> clean_names()

school_var <- names(dat_raw)[names(dat_raw) %in% c("school_id", "school")][1]
class_label_var <- names(dat_raw)[names(dat_raw) %in%
  c("classroom_label", "class_label", "class")][1]

dat_raw <- dat_raw |>
  mutate(
    !!school_var := as.factor(.data[[school_var]]),
    !!class_label_var := as.factor(.data[[class_label_var]]),
    class_id = interaction(.data[[school_var]], .data[[class_label_var]],
                           drop = TRUE)
  )

# Item vectors
pop_items  <- paste0("pop", 1:3)
roa_items  <- paste0("roa", 1:3)
cont_items <- paste0("cont", 1:3)
rd_items   <- paste0("rd", 1:3)
ns_items   <- paste0("ns", 1:3)
a_items    <- paste0("a", 1:3)
u_items    <- paste0("u", 1:3)
k_items    <- paste0("k", 1:3)
ksa_items  <- c(a_items, u_items, k_items)
kn_items   <- paste0("kn", 1:6)
sdr_items  <- paste0("sdr", 1:5)

# Reverse POP
dat_raw <- dat_raw |>
  mutate(across(all_of(pop_items), ~ 5 - as.numeric(.), .names = "{.col}_rev"))

# Score constructs
dat <- dat_raw |>
  mutate(
    hpt_pop_rev = rowMeans(across(paste0(pop_items, "_rev")), na.rm = TRUE),
    hpt_roa     = rowMeans(across(all_of(roa_items)),        na.rm = TRUE),
    hpt_cont    = rowMeans(across(all_of(cont_items)),       na.rm = TRUE),
    hpt_ctx6    = rowMeans(cbind(hpt_pop_rev, hpt_cont),     na.rm = TRUE),
    frlf_tot    = rowMeans(cbind(
      rowMeans(across(all_of(rd_items)), na.rm = TRUE),
      rowMeans(across(all_of(ns_items)), na.rm = TRUE)
    ), na.rm = TRUE),
    ksa3_tot    = rowMeans(across(all_of(ksa_items)), na.rm = TRUE),
    kn_total    = rowSums(across(all_of(kn_items)),   na.rm = TRUE),
    sdr5_tot    = rowMeans(across(all_of(sdr_items)), na.rm = TRUE)
  )

# ---- 2. Reshape for plotting ------------------------------------------------

hpt_long <- dat |>
  select(hpt_pop_rev, hpt_roa, hpt_cont, hpt_ctx6) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  mutate(variable = factor(variable,
    levels = c("hpt_pop_rev", "hpt_roa", "hpt_cont", "hpt_ctx6"),
    labels = c("POP (reversed)", "ROA", "CONT", "HPT CTX6 (primary)")
  ))

pred_long <- dat |>
  select(frlf_tot, ksa3_tot, kn_total, sdr5_tot) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  mutate(variable = factor(variable,
    levels = c("frlf_tot", "ksa3_tot", "kn_total", "sdr5_tot"),
    labels = c("FR-LF (1\u20135)", "KSA-3 (1\u20135)",
               "Knowledge (0\u20136)", "SDR-5 (1\u20135)")
  ))

# ---- 3. Build panels --------------------------------------------------------

# Colour-blind-safe palette
col_hpt  <- "#0072B2"
col_pred <- "#D55E00"

panel_hpt <- ggplot(hpt_long, aes(x = value)) +
  geom_histogram(binwidth = 0.25, fill = col_hpt, colour = "white",
                 alpha = 0.75, boundary = 1) +
  facet_wrap(~ variable, scales = "free_y", nrow = 1) +
  scale_x_continuous(breaks = 1:4, limits = c(0.75, 4.25)) +
  labs(
    title = "A. HPT Score Distributions",
    x = "Score (1\u20134 scale)",
    y = "Count"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    strip.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8.5)
  )

panel_pred <- ggplot(pred_long, aes(x = value)) +
  geom_histogram(binwidth = 0.25, fill = col_pred, colour = "white",
                 alpha = 0.75) +
  facet_wrap(~ variable, scales = "free", nrow = 1) +
  labs(
    title = "B. Predictor and Control Distributions",
    x = "Score",
    y = "Count"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    strip.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8.5)
  )

# ---- 4. Compose and save ----------------------------------------------------

fig3 <- panel_hpt / panel_pred +
  plot_annotation(
    caption = paste0(
      "Note. Panel A shows distributions of HPT subscale means (POP items ",
      "reverse-scored) and the primary composite (CTX6 = mean of POP_rev ",
      "and CONT).\nPanel B shows distributions of ideology (FR-LF), ",
      "authoritarianism (KSA-3), historical knowledge (KN, sum 0\u20136), ",
      "and social desirability (SDR-5).\nN = 276\u2013293 depending on ",
      "variable completeness."
    ),
    theme = theme(
      plot.caption = element_text(size = 7.5, hjust = 0, lineheight = 1.2,
                                  margin = margin(t = 8))
    )
  )

ggsave("../figures/fig03_score_distributions.pdf", fig3,
       width = 210, height = 150, units = "mm", device = cairo_pdf)

ggsave("../figures/fig03_score_distributions.png", fig3,
       width = 210, height = 150, units = "mm", dpi = 300)

cat("Figure 3 saved to ../figures/\n")
