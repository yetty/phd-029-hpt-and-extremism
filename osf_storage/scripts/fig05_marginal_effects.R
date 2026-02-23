# =============================================================================
# FIGURE 5: Predicted Marginal Effects — Ideology on HPT Contextualisation
#
# X-axis:  FR-LF ideology score (original 1–5 scale)
# Y-axis:  Predicted HPT CONT (original 1–4 scale)
# Lines:   H1 (uncontrolled) vs. H2 (controlled for KN, SDR, KSA)
# Shading: 95% confidence intervals
#
# Purpose: Visualise effect size magnitude; demonstrate substantive triviality
#          of ideology's relationship with contextualisation.
#
# Output:  trse_outputs/fig05_marginal_effects.pdf  (vector, 160 × 120 mm)
#          trse_outputs/fig05_marginal_effects.png  (300 DPI raster)
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(janitor)

dir.create("../figures", showWarnings = FALSE)
# ---- 1. Data preparation (mirrors 03_multilevel_models_hypothesis_tests.Rmd)

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

# Score constructs on ORIGINAL scales (not z-scored)
dat <- dat_raw |>
  mutate(
    hpt_cont = rowMeans(across(all_of(cont_items)), na.rm = TRUE),
    frlf_tot = rowMeans(cbind(
      rowMeans(across(all_of(rd_items)), na.rm = TRUE),
      rowMeans(across(all_of(ns_items)), na.rm = TRUE)
    ), na.rm = TRUE),
    ksa3_tot = rowMeans(across(all_of(ksa_items)), na.rm = TRUE),
    kn_total = rowSums(across(all_of(kn_items)), na.rm = TRUE),
    sdr5_tot = rowMeans(across(all_of(sdr_items)), na.rm = TRUE)
  ) |>
  drop_na(all_of(c(school_var, "class_id")))

# ---- 2. Fit models on raw scales -------------------------------------------

# H1 (uncontrolled): ideology only
m_h1 <- lmer(
  as.formula(paste0(
    "hpt_cont ~ frlf_tot + (1 | ", school_var, ") + (1 | class_id)"
  )),
  data = dat
)

# H2 (controlled): ideology + knowledge + social desirability + authoritarianism
m_h2 <- lmer(
  as.formula(paste0(
    "hpt_cont ~ frlf_tot + ksa3_tot + kn_total + sdr5_tot + (1 | ",
    school_var, ") + (1 | class_id)"
  )),
  data = dat
)

# ---- 3. Compute marginal predictions ---------------------------------------

# Prediction function: population-level (RE = 0) with fixed-effects CIs
marginal_pred <- function(model, newdata) {
  # Extract fixed-effects-only formula and drop the response for model.matrix
  ff <- formula(model, fixed.only = TRUE)
  rhs <- delete.response(terms(ff))
  X <- model.matrix(rhs, data = newdata)
  beta <- fixef(model)
  fit <- as.numeric(X %*% beta)
  V <- as.matrix(vcov(model))
  se <- sqrt(pmax(diag(X %*% V %*% t(X)), 0))
  data.frame(
    newdata,
    fit    = fit,
    ci_lo  = fit - 1.96 * se,
    ci_hi  = fit + 1.96 * se
  )
}

# Prediction grid: FR-LF from observed range, controls at sample means
frlf_range <- range(dat$frlf_tot, na.rm = TRUE)
frlf_seq   <- seq(frlf_range[1], frlf_range[2], length.out = 100)

# H1 grid (no controls needed)
grid_h1 <- data.frame(frlf_tot = frlf_seq)
pred_h1 <- marginal_pred(m_h1, grid_h1) |>
  mutate(model = "H1: Ideology only")

# H2 grid (controls at their means)
grid_h2 <- data.frame(
  frlf_tot = frlf_seq,
  ksa3_tot = mean(dat$ksa3_tot, na.rm = TRUE),
  kn_total = mean(dat$kn_total, na.rm = TRUE),
  sdr5_tot = mean(dat$sdr5_tot, na.rm = TRUE)
)
pred_h2 <- marginal_pred(m_h2, grid_h2) |>
  mutate(model = "H2: Controlled")

pred_all <- bind_rows(pred_h1, pred_h2)

# ---- 4. Build figure --------------------------------------------------------

# Colour-blind-safe palette
cols <- c("H1: Ideology only" = "#0072B2", "H2: Controlled" = "#D55E00")

fig5 <- ggplot(pred_all, aes(x = frlf_tot, y = fit,
                              colour = model, fill = model)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15,
              colour = NA) +
  geom_line(linewidth = 0.9) +
  # Add rug for observed FR-LF distribution
  geom_rug(data = dat, aes(x = frlf_tot), inherit.aes = FALSE,
           sides = "b", alpha = 0.15, length = unit(0.02, "npc")) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(limits = c(1, 4), breaks = 1:4) +
  labs(
    x = "Right-authoritarian ideology (FR-LF, 1\u20135)",
    y = "Predicted HPT Contextualisation (1\u20134)",
    colour = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -2),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 9.5),
    axis.text = element_text(size = 9),
    plot.caption = element_text(size = 7.5, hjust = 0, lineheight = 1.2,
                                margin = margin(t = 10))
  ) +
  annotate(
    "text", x = frlf_range[2], y = 1.15, hjust = 1, size = 2.8,
    colour = "grey40",
    label = paste0(
      "Controls held at sample means (KN = ",
      round(mean(dat$kn_total, na.rm = TRUE), 1),
      ", SDR = ",
      round(mean(dat$sdr5_tot, na.rm = TRUE), 1),
      ", KSA = ",
      round(mean(dat$ksa3_tot, na.rm = TRUE), 1), ")"
    )
  ) +
  labs(
    caption = paste0(
      "Note. Lines show population-level predicted HPT Contextualisation from ",
      "multilevel models (random intercepts\n",
      "for school and class). Shaded bands = 95% CIs for fixed effects. ",
      "Rug plot shows the observed distribution of\n",
      "FR-LF scores. H1 model includes ideology only; ",
      "H2 adds historical knowledge, social desirability, and KSA-3."
    )
  )

# ---- 5. Save ----------------------------------------------------------------

ggsave("../figures/fig05_marginal_effects.pdf", fig5,
       width = 160, height = 120, units = "mm", device = cairo_pdf)

ggsave("../figures/fig05_marginal_effects.png", fig5,
       width = 160, height = 120, units = "mm", dpi = 300)

cat("Figure 5 saved to ../figures/\n")
