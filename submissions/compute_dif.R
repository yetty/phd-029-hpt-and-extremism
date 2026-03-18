#!/usr/bin/env Rscript
# DIF exact statistics via mirt — constrained baseline with per-item freeing
suppressPackageStartupMessages(library(mirt))

dat <- readRDS("normalised_responses.RDS")
dat$FR_LF <- rowMeans(dat[, c("RD1","RD2","RD3","NS1","NS2","NS3")], na.rm=FALSE)
dat$KSA <- rowMeans(dat[, c("A1","A2","A3","U1","U2","U3","K1","K2","K3")], na.rm=FALSE)

ideology_composite <- (scale(dat$FR_LF)[,1] + scale(dat$KSA)[,1]) / 2
dat$ideology_group <- cut(ideology_composite,
                          breaks=quantile(ideology_composite, c(0, 1/3, 2/3, 1), na.rm=TRUE),
                          labels=c("Low","Mid","High"), include.lowest=TRUE)

hpt_items <- c("POP1_rev","POP2_rev","POP3_rev","ROA1","ROA2","ROA3","CONT1","CONT2","CONT3")
dat_dif <- dat[dat$ideology_group %in% c("Low","High") & complete.cases(dat[, hpt_items]), ]
dat_dif$ideology_group <- droplevels(dat_dif$ideology_group)

cat("DIF sample: Low n=", sum(dat_dif$ideology_group=="Low"),
    ", High n=", sum(dat_dif$ideology_group=="High"), "\n\n")

# Constrained baseline: all items equal, mean/variance free
mg_constr <- multipleGroup(dat_dif[, hpt_items], model=1,
                           group=dat_dif$ideology_group,
                           itemtype='graded', SE=TRUE, verbose=FALSE,
                           invariance=c('slopes','intercepts','free_means','free_var'))

# Per-item DIF: free one item at a time (drop from constraints)
cat("=== Per-item DIF (LR test, freeing each item) ===\n")
cat(sprintf("%-12s %10s %5s %10s %12s %8s\n", "Item", "chi2", "df", "p_raw", "p_bonf", "Flagged"))
cat(paste(rep("-", 60), collapse=""), "\n")

for (i in 1:length(hpt_items)) {
  # Free item i parameters in the focal group
  free_pars <- c('a1','d1','d2','d3')

  # Fit model with item i freed
  inv_items <- hpt_items[-i]  # constrain all except item i
  mg_free_i <- multipleGroup(dat_dif[, hpt_items], model=1,
                             group=dat_dif$ideology_group,
                             itemtype='graded', verbose=FALSE,
                             invariance=c(inv_items, 'free_means','free_var'))

  lr <- anova(mg_constr, mg_free_i, verbose=FALSE)
  chi2 <- lr$X2[2]
  df_val <- lr$df[2]
  p_val <- lr$p[2]
  p_bonf <- min(p_val * 9, 1.0)
  flagged <- ifelse(p_bonf < 0.01, "YES", "No")
  cat(sprintf("%-12s %10.3f %5d %10.4f %12.4f %8s\n",
      hpt_items[i], chi2, df_val, p_val, p_bonf, flagged))
}

cat("\nDIF analysis complete.\n")
