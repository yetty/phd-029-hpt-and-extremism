############################################################
# multilevel_and_DIF.R
# - Multilevel models for HPT outcomes
# - DIF diagnostics for HPT items (by ideology)
#
# Data source: normalised_responses.RData
# Assumes object named `all_data` in the workspace.
############################################################

# --------------------------
# 0) Setup
# --------------------------
pkgs <- c(
  "tidyverse", "lme4", "lmerTest", "performance",
  "ggeffects", "broom.mixed", "parameters",
  "lordif", "mirt"
)
invisible(lapply(pkgs, function(p) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)))
lapply(pkgs, library, character.only = TRUE)

set.seed(123)
options(dplyr.summarise.inform = FALSE)
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/figures", showWarnings = FALSE)
dir.create("outputs/tables", showWarnings = FALSE)

# --------------------------
# 1) Load & quick checks
# --------------------------
load("normalised_responses.RData")  
all_data <- normalised_responses  # adjust if object name differs

# Factor sanity
all_data <- all_data %>%
  mutate(
    class_label = as.factor(class_label),
    gender = factor(gender, levels = c("M","F","O"))
  )

# --------------------------
# 2) Score construction
# --------------------------
# HPT subscales & total (mean scores, 1–4)
hpt_items <- c("POP1","POP2","POP3","ROA1","ROA2","ROA3","CONT1","CONT2","CONT3")
stopifnot(all(hpt_items %in% names(all_data)))

all_data <- all_data %>%
  mutate(
    HPT_POP  = rowMeans(select(., POP1:POP3), na.rm = TRUE),
    HPT_ROA  = rowMeans(select(., ROA1:ROA3), na.rm = TRUE),
    HPT_CONT = rowMeans(select(., CONT1:CONT3), na.rm = TRUE),
    HPT_total = rowMeans(select(., all_of(hpt_items)), na.rm = TRUE)
  )

# Ideology (FR-LF mini) and Authoritarianism (KSA-3), Social desirability
fr_items  <- c("RD1","RD2","RD3","NS1","NS2","NS3")
ksa_items <- c("A1","A2","A3","U1","U2","U3","K1","K2","K3")
sdr_items <- c("SDR1","SDR2","SDR3","SDR4","SDR5")  # SDR2-4 already reversed in dataset

stopifnot(all(fr_items %in% names(all_data)))
stopifnot(all(ksa_items %in% names(all_data)))
stopifnot(all(sdr_items %in% names(all_data)))

all_data <- all_data %>%
  mutate(
    FR_total   = rowSums(select(., all_of(fr_items)), na.rm = TRUE),
    KSA_total  = rowSums(select(., all_of(ksa_items)), na.rm = TRUE),
    SDR_total  = rowSums(select(., all_of(sdr_items)), na.rm = TRUE)
  )

# Knowledge (sum of correct, 0/1)
kn_items <- paste0("KN", 1:6)
stopifnot(all(kn_items %in% names(all_data)))
all_data <- all_data %>%
  mutate(KN_sum = rowSums(select(., all_of(kn_items)), na.rm = TRUE))

# Z-standardize continuous predictors (for interpretability)
scale_z <- function(x) as.numeric(scale(x))
all_data <- all_data %>%
  mutate(
    FR_z  = scale_z(FR_total),
    KSA_z = scale_z(KSA_total),
    SDR_z = scale_z(SDR_total),
    KN_z  = scale_z(KN_sum)
  )

# --------------------------
# 3) Multilevel models
#    Outcomes: HPT_total, HPT_CONT, HPT_POP
#    Random intercept for class_label
# --------------------------
# build RHS as a single string
rhs <- "FR_z + KSA_z + KN_z + SDR_z + gender + (1 | class_label)"

mk_formula <- function(outcome) {
  as.formula(paste(outcome, "~", rhs))
}

models <- list(
  total = lmer(mk_formula("HPT_total"), data = all_data, REML = TRUE, na.action = na.omit),
  cont  = lmer(mk_formula("HPT_CONT"),  data = all_data, REML = TRUE, na.omit),
  pop   = lmer(mk_formula("HPT_POP"),   data = all_data, REML = TRUE, na.omit)
)

# Model summaries
summaries <- lapply(models, function(m) broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE))
write.csv(bind_rows(
  dplyr::bind_rows(summaries$total, .id = NULL) %>% mutate(model = "HPT_total"),
  dplyr::bind_rows(summaries$cont,  .id = NULL) %>% mutate(model = "HPT_CONT"),
  dplyr::bind_rows(summaries$pop,   .id = NULL) %>% mutate(model = "HPT_POP")
), "outputs/tables/multilevel_fixed_effects.csv", row.names = FALSE)

# ICCs and random effects
icc_tbl <- tibble(
  model = names(models),
  ICC   = sapply(models, performance::icc)
)
write.csv(icc_tbl, "outputs/tables/multilevel_icc.csv", row.names = FALSE)

# Marginal effects plots
for (nm in names(models)) {
  m <- models[[nm]]
  for (pred in c("FR_z","KSA_z","KN_z","SDR_z")) {
    g <- ggeffects::ggpredict(m, terms = paste0(pred, " [all]")) %>%
      plot() + ggtitle(paste0("Marginal effect of ", pred, " on ", nm))
    ggplot2::ggsave(filename = paste0("outputs/figures/marginal_", nm, "_", pred, ".png"),
                    plot = g, width = 7, height = 5, dpi = 300)
  }
}

# Save full model objects
saveRDS(models, "outputs/models_multilevel.rds")

# --------------------------
# 4) DIF analysis (primary: lordif)
#    Grouping: ideology_group by median FR_total
#    Matching variable: HPT sum score
# --------------------------
# Prepare HPT item matrix (integers 1–4)
hpt_mat <- all_data %>%
  select(all_of(hpt_items)) %>%
  mutate(across(everything(), as.integer)) %>%
  as.data.frame()

# Group variable (binary)
all_data <- all_data %>%
  mutate(ideology_group = if_else(FR_total >= median(FR_total, na.rm = TRUE), 1L, 0L))

# Matching variable (total raw sum; higher = more agreement)
HPT_rawsum <- rowSums(hpt_mat, na.rm = TRUE)

# Guard against too many missing values for lordif
valid_rows <- complete.cases(hpt_mat, all_data$ideology_group, HPT_rawsum)
hpt_mat_ld <- hpt_mat[valid_rows, , drop = FALSE]
group_ld   <- all_data$ideology_group[valid_rows]
theta_ld   <- HPT_rawsum[valid_rows]

# Run lordif (ordinal logistic DIF)
# - will select anchors iteratively
cat("\n===== Running lordif DIF (ideology high vs low) =====\n")
ld <- lordif::lordif(
  Data = hpt_mat_ld,
  group = group_ld,
  theta = theta_ld,
  criterion = "Chisqr",
  alpha = 0.01,            # stricter criterion
  pseudo.R2 = "McFadden",  # effect size indicator
  minCell = 5
)

sink("outputs/tables/DIF_lordif_summary.txt")
print(ld)
sink()

# Flag items with DIF (non-uniform or uniform)
dif_flags <- data.frame(
  item = rownames(ld$G2.Pvals),
  p_uniform   = ld$G2.Pvals[, "Uniform"],
  p_nonunif   = ld$G2.Pvals[, "Nonuniform"],
  p_chisq_all = ld$G2.Pvals[, "Chisqr"],
  R2_change   = ld$alpha.change # McFadden pseudo-R2 change
) %>%
  mutate(
    DIF_uniform   = p_uniform   < 0.01,
    DIF_nonunif   = p_nonunif   < 0.01,
    DIF_any       = p_chisq_all < 0.01
  )
write.csv(dif_flags, "outputs/tables/DIF_lordif_flags.csv", row.names = FALSE)

# --------------------------
# 5) DIF analysis (robustness: mirt multiple-group GRM)
# --------------------------
# mirt expects ordered factors for polytomous items
hpt_ord <- all_data %>%
  select(all_of(hpt_items), ideology_group) %>%
  drop_na() %>%
  mutate(across(all_of(hpt_items), ~ordered(.x, levels = sort(unique(.x)))))

mgmod <- mirt::multipleGroup(
  data  = hpt_ord %>% select(all_of(hpt_items)),
  model = 1,                # 1-factor graded response model
  group = hpt_ord$ideology_group,
  itemtype = "graded",
  invariance = c("free_means", "free_var")  # basic configural invariance
)

# DIF test for slopes (a) and intercepts (d)
dif_res <- mirt::DIF(mgmod, which.par = c("a1","d"), scheme = "additive", p.adjust = "BH")
capture.output(dif_res, file = "outputs/tables/DIF_mirt_results.txt")

# Also export item parameters by group
itempars <- coef(mgmod, IRTpars = TRUE, simplify = TRUE)
sink("outputs/tables/mirt_item_parameters.txt"); print(itempars); sink()

# --------------------------
# 6) Compact console summary
# --------------------------
cat("\n=== MULTILEVEL MODELS (Fixed effects) ===\n")
lapply(names(models), function(nm) {
  cat("\n--", nm, "--\n"); print(summary(models[[nm]]))
})

cat("\n=== ICCs ===\n")
print(icc_tbl)

cat("\n=== DIF (lordif) flags ===\n")
print(dif_flags)

cat("\nAll outputs written to: outputs/ (tables, figures, models)\n")
