############################################################
# factor_and_invariance.R
# Dimensionality of HPT (PCA, EFA, CFA) and measurement
# invariance across ideological subgroups.
#
# Loads data from: normalised_responses.RData
############################################################

# ----------------------------------------------------------
# 0. Setup
# ----------------------------------------------------------
library(tidyverse)
library(psych)        # PCA, EFA
library(lavaan)       # CFA, invariance
library(semTools)     # measurement invariance utilities

# ----------------------------------------------------------
# 1. Load data
# ----------------------------------------------------------
load("normalised_responses.RData") 
all_data <- normalised_responses

# ----------------------------------------------------------
# 2. Select HPT items
# ----------------------------------------------------------
hpt_items <- all_data %>%
  select(POP1, POP2, POP3,
         ROA1, ROA2, ROA3,
         CONT1, CONT2, CONT3)

# Optional: drop ROA1 if later PCA suggests cross-loading (as in HH 2008)
# hpt_items <- hpt_items %>% select(-ROA1)

# ----------------------------------------------------------
# 3. PCA (following Hartmann & Hasselhorn 2008 patterns)
# ----------------------------------------------------------
cat("\n===== PCA of HPT items =====\n")
pca_res <- principal(hpt_items, nfactors = 3, rotate = "varimax")
print(pca_res, digits = 3)

# Scree plot
png("fig_pca_scree.png", width = 800, height = 600)
plot(pca_res$values, type = "b", main = "HPT Scree Plot",
     ylab = "Eigenvalue", xlab = "Component")
abline(h = 1, col = "red", lty = 2)
dev.off()

# ----------------------------------------------------------
# 4. EFA (example: 2-factor solution)
# ----------------------------------------------------------
cat("\n===== EFA (2-factor) =====\n")
efa_res <- fa(hpt_items, nfactors = 2, rotate = "varimax", fm = "ml")
print(efa_res, digits = 3)

# ----------------------------------------------------------
# 5. CFA models
# ----------------------------------------------------------
# Model 1: Two-dimensional solution consistent with HH:
# Factor1 = POP + CONT
# Factor2 = ROA
cfa_model_2f <- "
  HPT_PC =~ POP1 + POP2 + POP3 + CONT1 + CONT2 + CONT3
  HPT_ROA =~ ROA1 + ROA2 + ROA3
"

cat("\n===== CFA: Two-factor model =====\n")
fit_2f <- cfa(cfa_model_2f, data = all_data, std.lv = TRUE)
summary(fit_2f, fit.measures = TRUE, standardized = TRUE)

# ----------------------------------------------------------
# 6. Ideology split (FR-LF-mini)
# ----------------------------------------------------------
# Create ideology grouping variable (median split)
# (You can change to quantiles or theory-based thresholds)
all_data <- all_data %>%
  mutate(FR_total = RD1 + RD2 + RD3 + NS1 + NS2 + NS3,
         ideology_group = ifelse(FR_total >= median(FR_total, na.rm = TRUE),
                                 "high_ideo", "low_ideo"))

table(all_data$ideology_group)

# ----------------------------------------------------------
# 7. Multi-group CFA invariance
# ----------------------------------------------------------
cat("\n===== Multi-group CFA by ideology_group =====\n")

# Configural (baseline)
fit_config <- cfa(cfa_model_2f, data = all_data,
                  group = "ideology_group", std.lv = TRUE)

# Metric invariance
fit_metric <- cfa(cfa_model_2f, data = all_data,
                  group = "ideology_group",
                  group.equal = "loadings", std.lv = TRUE)

# Scalar invariance
fit_scalar <- cfa(cfa_model_2f, data = all_data,
                  group = "ideology_group",
                  group.equal = c("loadings", "intercepts"),
                  std.lv = TRUE)

cat("\n--- Configural Fit ---\n")
fitMeasures(fit_config, c("cfi","tli","rmsea","srmr"))

cat("\n--- Metric Fit ---\n")
fitMeasures(fit_metric, c("cfi","tli","rmsea","srmr"))

cat("\n--- Scalar Fit ---\n")
fitMeasures(fit_scalar, c("cfi","tli","rmsea","srmr"))

# Compare models
cat("\n--- Model Comparisons ---\n")
anova(fit_config, fit_metric, fit_scalar)

# Automatic invariance testing (optional)
mi <- measurementInvariance(model = cfa_model_2f,
                            data = all_data,
                            group = "ideology_group")
print(mi)

# ----------------------------------------------------------
# 8. Export summary
# ----------------------------------------------------------
sink("factor_and_invariance_summary.txt")
cat("===== PCA =====\n")
print(pca_res)

cat("\n===== EFA =====\n")
print(efa_res)

cat("\n===== CFA (2-factor) =====\n")
summary(fit_2f, fit.measures = TRUE, standardized = TRUE)

cat("\n===== Measurement Invariance =====\n")
print(mi)
sink()

cat("\nDone: factor_and_invariance.R completed.\n")
