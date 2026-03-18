# =============================================================================
# FIGURE 4: Forest Plot of Standardized Coefficients from Multilevel Models
#
# Shows fixed-effect estimates (β) with 95% CIs from the base multilevel
# models predicting three HPT outcomes (CTX6, CONT, POP_rev).
# Highlights that ideology (FR-LF, KSA-3) coefficients cluster around zero
# while knowledge (KN) is the only significant predictor.
#
# Output:  trse_outputs/fig04_coefficient_plot.pdf  (vector, 180 × 140 mm)
#          trse_outputs/fig04_coefficient_plot.png  (300 DPI raster)
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(janitor)

# ---- 1. Data preparation (mirrors 03_multilevel-models-hypothesis-tests) ----

load("normalised_responses.RData")
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

# Score and standardize
z <- function(x) as.numeric(scale(x))

dat <- dat_raw |>
  mutate(
    hpt_cont    = rowMeans(across(all_of(cont_items)),       na.rm = TRUE),
    hpt_pop_rev = rowMeans(across(paste0(pop_items, "_rev")), na.rm = TRUE),
    hpt_total   = rowMeans(cbind(hpt_pop_rev, hpt_cont),     na.rm = TRUE),
    frlf_tot    = rowMeans(cbind(
      rowMeans(across(all_of(rd_items)), na.rm = TRUE),
      rowMeans(across(all_of(ns_items)), na.rm = TRUE)
    ), na.rm = TRUE),
    ksa3_tot    = rowMeans(across(all_of(ksa_items)), na.rm = TRUE),
    kn_total    = rowSums(across(all_of(kn_items)),   na.rm = TRUE),
    sdr5_tot    = rowMeans(across(all_of(sdr_items)), na.rm = TRUE)
  ) |>
  mutate(
    z_hpt_total = z(hpt_total),
    z_hpt_cont  = z(hpt_cont),
    z_hpt_pop   = z(hpt_pop_rev),
    z_frlf_tot  = z(frlf_tot),
    z_ksa3_tot  = z(ksa3_tot),
    z_kn_total  = z(kn_total),
    z_sdr5_tot  = z(sdr5_tot)
  ) |>
  drop_na(all_of(c(school_var, "class_id")))

# ---- 2. Fit base models -----------------------------------------------------

dv_list <- c("z_hpt_total", "z_hpt_cont", "z_hpt_pop")
dv_labels <- c("HPT CTX6", "CONT", "POP (rev.)")

fits <- list()
for (i in seq_along(dv_list)) {
  dv <- dv_list[i]
  form <- as.formula(paste0(
    dv, " ~ z_frlf_tot + z_ksa3_tot + z_kn_total + z_sdr5_tot + (1 | ",
    school_var, ") + (1 | class_id)"
  ))
  fits[[dv_labels[i]]] <- lmer(form, data = dat)
}

# ---- 3. Tidy coefficients ---------------------------------------------------

tidy_df <- bind_rows(
  lapply(names(fits), function(nm) {
    broom.mixed::tidy(fits[[nm]], effects = "fixed", conf.int = TRUE) |>
      filter(term != "(Intercept)") |>
      mutate(outcome = nm)
  })
)

# Clean predictor labels
tidy_df <- tidy_df |>
  mutate(
    predictor = case_when(
      term == "z_frlf_tot" ~ "FR-LF (ideology)",
      term == "z_ksa3_tot" ~ "KSA-3 (authoritarianism)",
      term == "z_kn_total" ~ "Historical knowledge",
      term == "z_sdr5_tot" ~ "Social desirability",
      TRUE ~ term
    ),
    predictor = factor(predictor, levels = rev(c(
      "FR-LF (ideology)", "KSA-3 (authoritarianism)",
      "Historical knowledge", "Social desirability"
    ))),
    outcome = factor(outcome, levels = dv_labels),
    significant = p.value < .05
  )

# ---- 4. Build figure ---------------------------------------------------------

# Colour: significant vs non-significant
cols <- c("TRUE" = "#D55E00", "FALSE" = "#0072B2")

fig4 <- ggplot(tidy_df, aes(x = estimate, y = predictor,
                             colour = significant, shape = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50",
             linewidth = 0.4) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.6),
                  size = 0.5, linewidth = 0.6) +
  scale_colour_manual(
    values = cols,
    labels = c("TRUE" = "p < .05", "FALSE" = "n.s."),
    name = NULL
  ) +
  scale_shape_manual(
    values = c(16, 17, 15),
    name = "Outcome"
  ) +
  scale_x_continuous(
    limits = c(-0.4, 0.55),
    breaks = seq(-0.4, 0.5, by = 0.1)
  ) +
  labs(
    x = "Standardized coefficient (\u03b2) with 95% CI",
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -2),
    legend.text = element_text(size = 9),
    legend.box = "horizontal",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 9, margin = margin(t = 5, l = -20)),
    axis.title.x = element_text(size = 9.5, margin = margin(t = 5, l = -20)),
    plot.caption = element_text(size = 7.5, hjust = 0, lineheight = 1.2,
                                margin = margin(t = 10, l = -50))
  ) +
  labs(
    caption = paste0(
      "Note. Points show standardized fixed-effect coefficients from ",
      "multilevel models (random intercepts for school and class).\n",
      "Error bars = 95% CIs. All predictors z-standardized. ",
      "Orange = p < .05; blue = non-significant. N = 285."
    )
  )

# ---- 5. Save -----------------------------------------------------------------

ggsave("trse_outputs/fig03_coefficients.pdf", fig4,
       width = 180, height = 140, units = "mm", device = cairo_pdf)

ggsave("trse_outputs/fig03_coefficients.png", fig4,
       width = 180, height = 140, units = "mm", dpi = 300)

cat("Figure 3 saved to trse_outputs/\n")
