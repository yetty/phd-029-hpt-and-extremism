# =============================================================================
# FIGURE 2: Measurement Invariance and DIF Across Ideological Groups
#
# Two-panel figure:
#   Panel A — Standardised factor loadings from configural MG-CFA (Low vs High)
#   Panel B — Item discrimination and threshold parameters from unconstrained
#             multi-group graded IRT model (Low vs High)
#
# Output: trse_outputs/fig02_invariance_and_dif.pdf  (vector, 210 × 130 mm)
#         trse_outputs/fig02_invariance_and_dif.png  (300 DPI raster fallback)
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(lavaan)
library(mirt)
library(patchwork)

# ---- 1. Data preparation (mirrors 04_dif-and-mg-cfa-hpt-bias.Rmd) ----------

load("normalised_responses.RData")
dat_raw <- normalised_responses

hpt_items <- c(paste0("POP", 1:3), paste0("ROA", 1:3), paste0("CONT", 1:3))
frlf_items <- c(paste0("RD", 1:3), paste0("NS", 1:3))
ksa_items  <- c(paste0("A", 1:3), paste0("U", 1:3), paste0("K", 1:3))

dat_raw <- dat_raw %>%
  mutate(
    school_id   = as.factor(school_id),
    class_label = as.factor(class_label),
    class_id    = interaction(school_id, class_label, drop = TRUE),
    across(all_of(c(hpt_items, frlf_items, ksa_items)),
           ~ suppressWarnings(as.numeric(.)))
  )

# Reverse POP items (higher = more contextualised)
dat_raw <- dat_raw %>%
  mutate(across(paste0("POP", 1:3), ~ 5 - ., .names = "{.col}_rev"))

# Ideology composite and tertile split
dat <- dat_raw %>%
  rowwise() %>%
  mutate(
    FRLF_mean = mean(c_across(all_of(frlf_items)), na.rm = TRUE),
    KSA_mean  = mean(c_across(all_of(ksa_items)),  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    FRLF_z = as.numeric(scale(FRLF_mean)),
    KSA_z  = as.numeric(scale(KSA_mean)),
    IDEO_Z = (FRLF_z + KSA_z) / 2
  )

qs <- quantile(dat$IDEO_Z, probs = c(.3334, .6666), na.rm = TRUE)
dat <- dat %>%
  mutate(ideology_group = case_when(
    IDEO_Z <= qs[1] ~ "Low",
    IDEO_Z >= qs[2] ~ "High",
    TRUE ~ "Mid"
  ))

# ---- 2. Panel A: CFA factor loadings by group ------------------------------

# Build CFA data frame with reversed POP
cfad <- dat %>%
  filter(ideology_group %in% c("Low", "High")) %>%
  transmute(
    ideology_group,
    POP1 = POP1_rev, POP2 = POP2_rev, POP3 = POP3_rev,
    ROA1, ROA2, ROA3,
    CONT1, CONT2, CONT3
  )

ord_items <- setdiff(names(cfad), "ideology_group")
cfad <- cfad %>%
  mutate(across(all_of(ord_items), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(across(all_of(ord_items), ~ ifelse(. %in% 1:4, ., NA_real_)))

model_3f <- '
  POP  =~ POP1 + POP2 + POP3
  ROA  =~ ROA1 + ROA2 + ROA3
  CONT =~ CONT1 + CONT2 + CONT3
'

fit_conf <- cfa(model_3f, data = cfad, group = "ideology_group",
                ordered = ord_items, estimator = "WLSMV")

# Extract standardised loadings per group
loadings_df <- standardizedSolution(fit_conf) %>%
  filter(op == "=~") %>%
  transmute(
    subscale = lhs,
    item     = rhs,
    group    = ifelse(group == 1, levels(factor(cfad$ideology_group))[1],
                      levels(factor(cfad$ideology_group))[2]),
    loading  = est.std,
    se       = se,
    ci_lo    = ci.lower,
    ci_hi    = ci.upper
  )

# Ensure consistent item ordering: POP, ROA, CONT (1-3 within each)
item_order <- c(paste0("POP", 1:3), paste0("ROA", 1:3), paste0("CONT", 1:3))
loadings_df <- loadings_df %>%
  mutate(
    item = factor(item, levels = rev(item_order)),
    subscale = factor(subscale, levels = c("POP", "ROA", "CONT"))
  )

# Subscale labels for strip text
subscale_labels <- c(
  POP  = "Presentism (reversed)",
  ROA  = "Role of the Historical Agent",
  CONT = "Contextualisation"
)

panel_a <- ggplot(loadings_df,
                  aes(x = loading, y = item, colour = group, shape = group)) +
  geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi),
                  position = position_dodge(width = 0.5),
                  size = 0.45, linewidth = 0.5) +
  facet_grid(subscale ~ ., scales = "free_y", space = "free_y",
             labeller = labeller(subscale = subscale_labels)) +
  scale_colour_manual(
    values = c("Low" = "#0072B2", "High" = "#D55E00"),
    labels = c("Low" = "Low ideology", "High" = "High ideology")
  ) +
  scale_shape_manual(
    values = c("Low" = 16, "High" = 17),
    labels = c("Low" = "Low ideology", "High" = "High ideology")
  ) +
  labs(
    title = "A. Factor Loadings (Configural MG-CFA)",
    x = "Standardised loading",
    y = NULL,
    colour = NULL, shape = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    legend.position = "bottom",
    legend.margin = margin(t = -4),
    legend.text = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.y = element_text(angle = 0, hjust = 0, size = 8),
    axis.text = element_text(size = 8),
    axis.title.x = element_text(size = 8.5)
  )

# ---- 3. Panel B: IRT item parameters by group (unconstrained model) --------

# Use reversed POP items (same direction as CFA) so all discriminations are
# positive and directly interpretable.
hpt_items_rev <- c(paste0("POP", 1:3, "_rev"), paste0("ROA", 1:3), paste0("CONT", 1:3))
anal <- dat %>% filter(ideology_group %in% c("Low", "High"))
hpt_mat <- anal[, hpt_items_rev, drop = FALSE]
for (j in seq_along(hpt_items_rev)) hpt_mat[[j]] <- suppressWarnings(as.numeric(hpt_mat[[j]]))
# Enforce 1-4 range
hpt_mat <- hpt_mat %>% mutate(across(everything(), ~ ifelse(. %in% 1:4, ., NA_real_)))
# Rename to clean labels for the plot
colnames(hpt_mat) <- c(paste0("POP", 1:3), paste0("ROA", 1:3), paste0("CONT", 1:3))
grp <- factor(anal$ideology_group, levels = c("Low", "High"))
keep_irt <- rowSums(!is.na(hpt_mat)) >= 2
hpt_mat <- hpt_mat[keep_irt, , drop = FALSE]
grp <- droplevels(grp[keep_irt])

# Fit configural model (item parameters free across groups, latent N(0,1) in
# both groups for identification). Any real group differences in latent ability
# are absorbed into item parameters, so equivalent parameters = no DIF.
mod_free <- multipleGroup(
  data     = hpt_mat,
  model    = 1,
  group    = grp,
  itemtype = "graded"
)

# Extract item parameters per group
extract_pars <- function(mod, group_name) {
  coefs <- coef(mod, IRTpars = TRUE, simplify = TRUE)[[group_name]]$items
  tibble(
    item   = rownames(coefs),
    a      = coefs[, "a"],
    b_mean = rowMeans(coefs[, grep("^b", colnames(coefs)), drop = FALSE])
  )
}

pars_low  <- extract_pars(mod_free, "Low")  %>% mutate(group = "Low")
pars_high <- extract_pars(mod_free, "High") %>% mutate(group = "High")

# Pivot to wide for scatter plot (Low on x, High on y)
pars_wide_a <- inner_join(
  pars_low  %>% select(item, a_low = a),
  pars_high %>% select(item, a_high = a),
  by = "item"
)
pars_wide_b <- inner_join(
  pars_low  %>% select(item, b_low = b_mean),
  pars_high %>% select(item, b_high = b_mean),
  by = "item"
)

# Assign subscale for colour
assign_subscale <- function(item) {
  case_when(
    grepl("^POP", item)  ~ "POP",
    grepl("^ROA", item)  ~ "ROA",
    grepl("^CONT", item) ~ "CONT"
  )
}

pars_wide_a$subscale <- assign_subscale(pars_wide_a$item)
pars_wide_b$subscale <- assign_subscale(pars_wide_b$item)

# Combine discrimination and difficulty into one long frame for a 2-facet plot
scatter_df <- bind_rows(
  pars_wide_a %>% transmute(item, subscale, low = a_low, high = a_high,
                            parameter = "Discrimination (a)"),
  pars_wide_b %>% transmute(item, subscale, low = b_low, high = b_high,
                            parameter = "Location (avg. threshold)")
)

scatter_df$subscale <- factor(scatter_df$subscale,
                              levels = c("POP", "ROA", "CONT"))

# Winsorise extreme threshold values (from items with near-zero discrimination)
# to keep the scatter readable; mark any clipped points
clip_at <- 5
scatter_df <- scatter_df %>%
  mutate(
    clipped = abs(low) > clip_at | abs(high) > clip_at,
    low  = pmax(pmin(low,  clip_at), -clip_at),
    high = pmax(pmin(high, clip_at), -clip_at)
  )

panel_b <- ggplot(scatter_df, aes(x = low, y = high,
                                  colour = subscale, shape = subscale)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              colour = "grey50", linewidth = 0.4) +
  geom_point(size = 2.5, stroke = 0.6) +
  geom_text(aes(label = item), size = 2.2, vjust = -1, show.legend = FALSE) +
  facet_wrap(~ parameter, scales = "free") +
  scale_colour_manual(
    values = c("POP" = "#0072B2", "ROA" = "#009E73", "CONT" = "#CC79A7"),
    labels = c(POP = "Presentism", ROA = "Author", CONT = "Context")
  ) +
  scale_shape_manual(
    values = c("POP" = 16, "ROA" = 17, "CONT" = 15),
    labels = c(POP = "Presentism", ROA = "Author", CONT = "Context")
  ) +
  labs(
    title = "B. IRT Item Parameters (Multi-Group GRM)",
    x = "Low ideology group",
    y = "High ideology group",
    colour = "Subscale", shape = "Subscale"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    legend.position = "bottom",
    legend.margin = margin(t = -4),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 8.5),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8.5)
  )

# ---- 4. Compose and save ---------------------------------------------------

fig2 <- panel_a + panel_b +
  plot_layout(widths = c(1, 1.3)) +
  plot_annotation(
    caption = paste0(
      "Note. Panel A shows standardised factor loadings (with 95% CIs) from a configural three-factor MG-CFA\n",
      "(WLSMV estimator; Low vs. High ideology tertiles). Panel B plots item discrimination (a) and average\n",
      "location parameters from unconstrained multi-group graded response models. Points near the identity\n",
      "line indicate equivalent item functioning. No items were flagged for DIF (\u03b1 = .01, Bonferroni-adjusted)."
    ),
    theme = theme(
      plot.caption = element_text(size = 7.5, hjust = 0, lineheight = 1.2,
                                  margin = margin(t = 8))
    )
  )

ggsave("trse_outputs/fig02_invariance_and_dif.pdf", fig2,
       width = 210, height = 130, units = "mm", device = cairo_pdf)

ggsave("trse_outputs/fig02_invariance_and_dif.png", fig2,
       width = 210, height = 130, units = "mm", dpi = 300)

cat("Figure 2 saved to trse_outputs/\n")
