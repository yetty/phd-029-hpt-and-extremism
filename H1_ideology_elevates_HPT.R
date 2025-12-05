############################################################
# H1_ideology_elevates_HPT.R
# Hypothesis 1: Ideology → higher HPT (total, CONT, POP)
# Multilevel (students nested in class), no extra controls.
############################################################

# 0) Setup
library(tidyverse)
library(nlme)
library(broom)
library(broom.mixed)   # gives tidy() for lme objects
library(ggeffects)
library(lordif)
library(mirt)
library(parameters)
library(performance)


set.seed(123); dir.create("outputs", showWarnings=FALSE); dir.create("outputs/figures", showWarnings=FALSE); dir.create("outputs/tables", showWarnings=FALSE)

# 1) Load
load("normalised_responses.RData")  
all_data <- normalised_responses  # adjust if object name differs

# 2) Scoring (per codebook)
hpt_items <- c("POP1","POP2","POP3","ROA1","ROA2","ROA3","CONT1","CONT2","CONT3")
fr_items  <- c("RD1","RD2","RD3","NS1","NS2","NS3")
ksa_items <- c("A1","A2","A3","U1","U2","U3","K1","K2","K3")

stopifnot(all(hpt_items %in% names(all_data)))
stopifnot(all(fr_items  %in% names(all_data)))
stopifnot(all(ksa_items %in% names(all_data)))

all_data <- all_data %>%
  mutate(
    class_label = factor(class_label),
    HPT_total = rowMeans(dplyr::select(., all_of(hpt_items)), na.rm=TRUE),
    HPT_CONT  = rowMeans(dplyr::select(., CONT1,CONT2,CONT3), na.rm=TRUE),
    HPT_POP   = rowMeans(dplyr::select(., POP1,POP2,POP3),   na.rm=TRUE),
    FR_total  = rowSums(dplyr::select(., all_of(fr_items)),   na.rm=TRUE),
    KSA_total = rowSums(dplyr::select(., all_of(ksa_items)),  na.rm=TRUE)
  )

# Standardize predictors for interpretability
scale_z <- function(x) as.numeric(scale(x))
all_data <- all_data %>% mutate(FR_z = scale_z(FR_total), KSA_z = scale_z(KSA_total))

# 3) Multilevel models (random intercept: class)
fit_lme <- function(outcome){
  nlme::lme(
    fixed  = as.formula(paste0(outcome, " ~ FR_z + KSA_z")),
    random = ~ 1 | class_label,
    data   = all_data,
    na.action = na.omit,
    method = "REML",
    control = nlme::lmeControl(opt="optim")
  )
}
mods <- list(
  HPT_total = fit_lme("HPT_total"),
  HPT_CONT  = fit_lme("HPT_CONT"),
  HPT_POP   = fit_lme("HPT_POP")
)

# 4) Export results
tb <- purrr::imap(mods, ~ broom::tidy(.x) %>% mutate(model=.y)) %>% bind_rows()
readr::write_csv(tb, "outputs/tables/H1_fixed_effects.csv")

# Quick ICC
icc_from_lme <- function(m){
  re <- as.numeric(nlme::VarCorr(m)["class_label","Variance"])
  rs <- as.numeric(nlme::VarCorr(m)["Residual","Variance"])
  re/(re+rs)
}
icc_tbl <- tibble::tibble(model=names(mods), ICC=sapply(mods, icc_from_lme))
readr::write_csv(icc_tbl, "outputs/tables/H1_icc.csv")

# Marginal effects plots
for(nm in names(mods)){
  for(pred in c("FR_z","KSA_z")){
    p <- ggeffects::ggpredict(mods[[nm]], terms=paste0(pred," [all]")) %>% plot() +
      ggplot2::ggtitle(paste0("H1: Effect of ", pred, " on ", nm))
    ggplot2::ggsave(paste0("outputs/figures/H1_marginal_", nm, "_", pred, ".png"), p, width=7, height=5, dpi=300)
  }
}

cat("H1 done. Tables in outputs/tables/, figures in outputs/figures/\n")
