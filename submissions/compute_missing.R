#!/usr/bin/env Rscript
# Compute all missing statistics for Paper A (SPE) and Paper B (AME)

suppressPackageStartupMessages({
  library(lavaan)
  library(mirt)
  library(lme4)
  library(lmerTest)
  library(psych)
})

dat <- readRDS("normalised_responses.RDS")

# --- Compute composite scores ---
dat$FR_LF <- rowMeans(dat[, c("RD1","RD2","RD3","NS1","NS2","NS3")], na.rm=FALSE)
dat$KSA <- rowMeans(dat[, c("A1","A2","A3","U1","U2","U3","K1","K2","K3")], na.rm=FALSE)
dat$KN <- rowSums(dat[, paste0("KN", 1:6)], na.rm=FALSE)
# SDR: items 2-4 reversed (5-point scale)
dat$SDR2r <- 6 - dat$SDR2
dat$SDR3r <- 6 - dat$SDR3
dat$SDR4r <- 6 - dat$SDR4
dat$SDR <- rowMeans(dat[, c("SDR1","SDR2r","SDR3r","SDR4r","SDR5")], na.rm=FALSE)
dat$FR_RD <- rowMeans(dat[, c("RD1","RD2","RD3")], na.rm=FALSE)
dat$FR_NS <- rowMeans(dat[, c("NS1","NS2","NS3")], na.rm=FALSE)

# Z-standardize predictors
dat$FR_z <- scale(dat$FR_LF)[,1]
dat$KSA_z <- scale(dat$KSA)[,1]
dat$KN_z <- scale(dat$KN)[,1]
dat$SDR_z <- scale(dat$SDR)[,1]

cat("\n========================================\n")
cat("PAPER B: CFA ANALYSES\n")
cat("========================================\n\n")

# --- 1. CFA with chi-square and df ---
hpt_items <- c("POP1_rev","POP2_rev","POP3_rev","ROA1","ROA2","ROA3","CONT1","CONT2","CONT3")
dat_cfa <- dat[complete.cases(dat[, hpt_items]), ]

# 1-factor model
mod1 <- 'HPT =~ POP1_rev + POP2_rev + POP3_rev + ROA1 + ROA2 + ROA3 + CONT1 + CONT2 + CONT3'
fit1 <- cfa(mod1, data=dat_cfa, ordered=hpt_items, estimator="WLSMV")

# 2-factor model (POP+CONT vs ROA)
mod2 <- 'F1 =~ POP1_rev + POP2_rev + POP3_rev + CONT1 + CONT2 + CONT3
          F2 =~ ROA1 + ROA2 + ROA3'
fit2 <- cfa(mod2, data=dat_cfa, ordered=hpt_items, estimator="WLSMV")

# 3-factor model
mod3 <- 'POP =~ POP1_rev + POP2_rev + POP3_rev
          ROA =~ ROA1 + ROA2 + ROA3
          CONT =~ CONT1 + CONT2 + CONT3'
fit3 <- cfa(mod3, data=dat_cfa, ordered=hpt_items, estimator="WLSMV")

cat("--- CFA Model Comparison (chi-square, df) ---\n")
for (nm in c("1-Factor", "2-Factor", "3-Factor")) {
  fit <- switch(nm, "1-Factor"=fit1, "2-Factor"=fit2, "3-Factor"=fit3)
  fi <- fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr"))
  cat(sprintf("%s: chi2=%.2f, df=%d, p=%.4f, CFI=%.3f, TLI=%.3f, RMSEA=%.3f [%.3f,%.3f], SRMR=%.3f\n",
      nm, fi["chisq"], fi["df"], fi["pvalue"], fi["cfi"], fi["tli"],
      fi["rmsea"], fi["rmsea.ci.lower"], fi["rmsea.ci.upper"], fi["srmr"]))
}

cat("\n--- 3-Factor Inter-factor Correlations (Phi matrix) ---\n")
vc <- lavInspect(fit3, "cor.lv")
print(round(vc, 3))

cat("\n--- 3-Factor Standardized Loadings ---\n")
sl <- standardizedSolution(fit3)
sl_load <- sl[sl$op == "=~", c("lhs","rhs","est.std","se","ci.lower","ci.upper","pvalue")]
print(sl_load, digits=3)

cat("\n--- Mean Inter-item Correlations per Subscale ---\n")
pop_items <- c("POP1_rev","POP2_rev","POP3_rev")
roa_items <- c("ROA1","ROA2","ROA3")
cont_items <- c("CONT1","CONT2","CONT3")

mean_r <- function(items, data) {
  cm <- cor(data[, items], use="pairwise.complete.obs")
  mean(cm[lower.tri(cm)])
}

cat(sprintf("POP mean r_ii = %.3f\n", mean_r(pop_items, dat_cfa)))
cat(sprintf("ROA mean r_ii = %.3f\n", mean_r(roa_items, dat_cfa)))
cat(sprintf("CONT mean r_ii = %.3f\n", mean_r(cont_items, dat_cfa)))

cat("\n--- Bifactor Model ---\n")
mod_bi <- 'G =~ POP1_rev + POP2_rev + POP3_rev + ROA1 + ROA2 + ROA3 + CONT1 + CONT2 + CONT3
            POP =~ POP1_rev + POP2_rev + POP3_rev
            ROA =~ ROA1 + ROA2 + ROA3
            CONT =~ CONT1 + CONT2 + CONT3'
tryCatch({
  fit_bi <- cfa(mod_bi, data=dat_cfa, ordered=hpt_items, estimator="WLSMV",
                orthogonal=TRUE)  # bifactor = orthogonal specific factors
  fi_bi <- fitMeasures(fit_bi, c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr"))
  cat(sprintf("Bifactor: chi2=%.2f, df=%d, p=%.4f, CFI=%.3f, TLI=%.3f, RMSEA=%.3f [%.3f,%.3f], SRMR=%.3f\n",
      fi_bi["chisq"], fi_bi["df"], fi_bi["pvalue"], fi_bi["cfi"], fi_bi["tli"],
      fi_bi["rmsea"], fi_bi["rmsea.ci.lower"], fi_bi["rmsea.ci.upper"], fi_bi["srmr"]))
  cat("\nBifactor standardized loadings:\n")
  sl_bi <- standardizedSolution(fit_bi)
  print(sl_bi[sl_bi$op == "=~", c("lhs","rhs","est.std","se","pvalue")], digits=3)
}, error = function(e) {
  cat("Bifactor model failed:", conditionMessage(e), "\n")
})

cat("\n========================================\n")
cat("PAPER B: MG-CFA SRMR (metric/scalar)\n")
cat("========================================\n\n")

# Ideology grouping (tertile split)
ideology_composite <- scale(dat$FR_LF)[,1]/2 + scale(dat$KSA)[,1]/2
dat$ideology_group <- cut(ideology_composite,
                          breaks=quantile(ideology_composite, c(0, 1/3, 2/3, 1), na.rm=TRUE),
                          labels=c("Low","Mid","High"), include.lowest=TRUE)
dat_mg <- dat[dat$ideology_group %in% c("Low","High") & complete.cases(dat[, hpt_items]), ]
dat_mg$ideology_group <- droplevels(dat_mg$ideology_group)
cat("MG-CFA sample: Low n=", sum(dat_mg$ideology_group=="Low"),
    ", High n=", sum(dat_mg$ideology_group=="High"), "\n\n")

# Configural
fit_config <- cfa(mod3, data=dat_mg, ordered=hpt_items, estimator="WLSMV",
                  group="ideology_group")
# Metric
fit_metric <- cfa(mod3, data=dat_mg, ordered=hpt_items, estimator="WLSMV",
                  group="ideology_group", group.equal="loadings")
# Scalar
fit_scalar <- cfa(mod3, data=dat_mg, ordered=hpt_items, estimator="WLSMV",
                  group="ideology_group", group.equal=c("loadings","thresholds"))

for (nm in c("Configural", "Metric", "Scalar")) {
  fit <- switch(nm, "Configural"=fit_config, "Metric"=fit_metric, "Scalar"=fit_scalar)
  fi <- fitMeasures(fit, c("chisq","df","cfi","rmsea","srmr"))
  cat(sprintf("%s: chi2=%.2f, df=%d, CFI=%.3f, RMSEA=%.3f, SRMR=%.3f\n",
      nm, fi["chisq"], fi["df"], fi["cfi"], fi["rmsea"], fi["srmr"]))
}

cat("\n========================================\n")
cat("PAPER B: DIF EXACT STATISTICS\n")
cat("========================================\n\n")

# DIF via mirt (constrained multi-group GRM)
dat_dif <- dat_mg[complete.cases(dat_mg[, hpt_items]), ]

tryCatch({
  # Fit multi-group GRM
  mg_mod <- multipleGroup(dat_dif[, hpt_items], model=1,
                          group=dat_dif$ideology_group,
                          itemtype='graded', SE=TRUE, verbose=FALSE)

  # DIF test per item
  cat("Item-level DIF (likelihood ratio tests):\n")
  cat(sprintf("%-10s %10s %5s %10s %10s\n", "Item", "chi2", "df", "p_raw", "p_adj"))
  for (i in 1:length(hpt_items)) {
    dif_test <- DIF(mg_mod, which.par=c('a1', 'd1', 'd2', 'd3'),
                    items2test=i, scheme='drop', seq_stat='LR',
                    p.adjust='none', verbose=FALSE)
    chi2 <- dif_test$X2[1]
    df_val <- dif_test$df[1]
    p_val <- dif_test$p[1]
    p_adj <- p.adjust(p_val, method="bonferroni", n=9)
    cat(sprintf("%-10s %10.3f %5d %10.4f %10.4f\n",
        hpt_items[i], chi2, df_val, p_val, p_adj))
  }
}, error = function(e) {
  cat("DIF analysis error:", conditionMessage(e), "\n")
})

cat("\n========================================\n")
cat("PAPER B: HISTORY GRADE CORRELATIONS\n")
cat("========================================\n\n")

# Note: Czech scale 1=excellent, 5=failing, so negative correlation with HPT = expected
cor_vars <- c("HPT_CTX6","HPT_CONT","HPT_POP_rev","HPT_ROA","HPT_TOT9","history_grade")
dat_cor <- dat[complete.cases(dat[, cor_vars]), ]
cat("N for history grade correlations:", nrow(dat_cor), "\n")
for (v in c("HPT_CTX6","HPT_CONT","HPT_POP_rev","HPT_ROA","HPT_TOT9")) {
  r <- cor(dat_cor[[v]], dat_cor$history_grade, use="complete.obs")
  ct <- cor.test(dat_cor[[v]], dat_cor$history_grade)
  cat(sprintf("%s ~ history_grade: r = %.3f, p = %.4f, 95%% CI [%.3f, %.3f]\n",
      v, r, ct$p.value, ct$conf.int[1], ct$conf.int[2]))
}

cat("\n========================================\n")
cat("PAPER A: GENDER SENSITIVITY CHECK\n")
cat("========================================\n\n")

# Add gender dummy (male=1, female=0, other=NA for sensitivity)
dat$gender_binary <- ifelse(dat$gender == "M", 0,
                     ifelse(dat$gender == "F", 1, NA))
cat("Gender: M=", sum(dat$gender=="M", na.rm=TRUE),
    ", F=", sum(dat$gender=="F", na.rm=TRUE),
    ", Other=", sum(dat$gender=="O" | dat$gender=="other", na.rm=TRUE), "\n")

dat_gender <- dat[!is.na(dat$gender_binary) & !is.na(dat$FR_z) & !is.na(dat$KSA_z) &
                  !is.na(dat$KN_z) & !is.na(dat$SDR_z) & !is.na(dat$HPT_CTX6) &
                  !is.na(dat$school_id) & !is.na(dat$class_id), ]
cat("N for gender models:", nrow(dat_gender), "\n\n")

# Original model (no gender)
m_orig <- lmer(HPT_CTX6 ~ FR_z + KSA_z + KN_z + SDR_z + (1|school_id) + (1|class_id),
               data=dat_gender, REML=TRUE)

# Model with gender
m_gender <- lmer(HPT_CTX6 ~ FR_z + KSA_z + KN_z + SDR_z + gender_binary +
                 (1|school_id) + (1|class_id),
                 data=dat_gender, REML=TRUE)

cat("--- Without gender ---\n")
print(summary(m_orig)$coefficients, digits=3)
cat("\n--- With gender ---\n")
print(summary(m_gender)$coefficients, digits=3)

cat("\n\nFR-LF coefficient change: ",
    sprintf("%.4f -> %.4f", fixef(m_orig)["FR_z"], fixef(m_gender)["FR_z"]), "\n")
cat("KSA coefficient change: ",
    sprintf("%.4f -> %.4f", fixef(m_orig)["KSA_z"], fixef(m_gender)["KSA_z"]), "\n")

cat("\n========================================\n")
cat("ALL ANALYSES COMPLETE\n")
cat("========================================\n")
