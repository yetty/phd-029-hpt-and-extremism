# What this document does

This report checks whether our **Historical Perspective-Taking (HPT)**
instrument behaves well **before** we run any hypothesis tests.

We do three things:

1.  **Reliability:** Are the HPT subscales internally consistent? We
    report **Cronbach's alpha ($\alpha$)** and **McDonald's omega
    ($\omega$)** for **POP**, **ROA**, **CONT** (three items each; 1-4).
2.  **Dimensionality (CFA/EFA):** Does the **factor structure** match
    prior research (e.g., **POP+CONT** vs **ROA**, or three correlated
    factors)?
3.  **Presentism-contextualization contrast:** Do **POP** (presentist)
    and **CONT** (contextualization) show the expected contrast? \>
    **Composite scores used** (for descriptives and later files): \> -
    **POP_rev = 5 − POP_raw** (so higher = more contextualized). \> -
    **HPT_CTX6 = mean(POP_rev, CONT)** (default composite). \> -
    **HPT_TOT9 = mean(POP_rev, CONT, ROA)** (includes ROA; robustness).

# Setup and data loading

``` r
options(width = 120)

# Data handling & plots
library(tidyverse)

# Psychometrics
library(psych)        # alpha, omega, polychoric, EFA helpers
library(lavaan)       # CFA
library(semTools)     # model comparisons & extras

# Tables
library(knitr)

# Make kableExtra use longtable/booktabs and avoid loading tabu
options(kableExtra.latex.load_packages = FALSE)
library(kableExtra)
source("submissions/pci_psychology/scoring_helpers.R")

# Load the dataset created in 00_data-preparation
load("normalised_responses.RData")
stopifnot(exists("normalised_responses"))
dat <- normalised_responses

# POP reversed item-wise and subscale helper
POP_rev_items <- paste0("POP", 1:3)

# Reverse POP items (1-4)
dat <- dat %>%
  mutate(
    across(
      all_of(POP_rev_items),
      ~ 5 - suppressWarnings(as.numeric(.)),
      .names = "{.col}_rev"     # <<< THIS was the culprit
    )
  ) %>%
  mutate(
    HPT_POP_raw = scale_mean(., POP_rev_items, min_answered = 2),
    HPT_POP_rev = scale_mean(., paste0(POP_rev_items, "_rev"), min_answered = 2),
    HPT_CONT    = scale_mean(., paste0("CONT", 1:3), min_answered = 2),
    HPT_ROA     = scale_mean(., paste0("ROA", 1:3), min_answered = 2),
    HPT_CTX6    = rowMeans(cbind(HPT_POP_rev, HPT_CONT), na.rm = FALSE),
    HPT_TOT9    = rowMeans(cbind(HPT_POP_rev, HPT_CONT, HPT_ROA),
                           na.rm = FALSE)
  )

print_tbl <- function(df, caption, digits = 3, escape = TRUE) {
  kbl(df, booktabs = TRUE, longtable = TRUE, caption = caption, digits = digits, escape = escape) |>
    kable_styling(full_width = FALSE, latex_options = c("hold_position"))
}
```

We verify that **HPT items** exist. If something is missing, we stop
with a clear message.

``` r
## -- check-columns ----------------------------
hpt_cols <- c(paste0("POP", 1:3), paste0("ROA", 1:3), paste0("CONT", 1:3))
need   <- hpt_cols
miss   <- setdiff(need, names(dat))
if (length(miss)) stop("Missing variables: ", paste(miss, collapse = ", "))

keep <- complete.cases(dat[, hpt_cols])

analysis_df <- dat[keep, hpt_cols] |>
  as_tibble()

nrow_all   <- nrow(dat)
nrow_keep  <- nrow(analysis_df)
cat("Rows in full data: ", nrow_all,  "\n",
    "Rows kept (complete HPT items): ", nrow_keep, "\n", sep = "")
```

    ## Rows in full data: 293
    ## Rows kept (complete HPT items): 276

# Step 1 -- Descriptives and scale construction

**Why:** Simple summaries catch obvious data problems and help readers
develop intuition.

``` r
hpt_items <- analysis_df %>% select(all_of(hpt_cols))  # 9 HPT items

# Subscales and composites with POP reversed for composites
hpt_scores <- hpt_items %>%
  mutate(
    POP_raw = rowMeans(select(., starts_with("POP")),  na.rm = TRUE),
    ROA     = rowMeans(select(., starts_with("ROA")),  na.rm = TRUE),
    CONT    = rowMeans(select(., starts_with("CONT")), na.rm = TRUE)
  ) %>%
  mutate(
    POP_rev   = 5 - POP_raw,
    HPT_CTX6  = rowMeans(cbind(POP_rev, CONT), na.rm = TRUE),
    HPT_TOT9  = rowMeans(cbind(POP_rev, CONT, ROA), na.rm = TRUE)
  )

summary(select(hpt_scores, POP_raw, POP_rev, ROA, CONT, HPT_CTX6, HPT_TOT9))
```

    ##     POP_raw         POP_rev           ROA             CONT          HPT_CTX6        HPT_TOT9
    ##  Min.   :1.000   Min.   :1.333   Min.   :1.000   Min.   :1.000   Min.   :1.333   Min.   :1.667
    ##  1st Qu.:1.583   1st Qu.:2.333   1st Qu.:2.333   1st Qu.:2.333   1st Qu.:2.500   1st Qu.:2.444
    ##  Median :2.000   Median :3.000   Median :3.000   Median :2.667   Median :2.833   Median :2.889
    ##  Mean   :2.029   Mean   :2.971   Mean   :2.793   Mean   :2.713   Mean   :2.842   Mean   :2.826
    ##  3rd Qu.:2.667   3rd Qu.:3.417   3rd Qu.:3.333   3rd Qu.:3.333   3rd Qu.:3.333   3rd Qu.:3.222
    ##  Max.   :3.667   Max.   :4.000   Max.   :4.000   Max.   :4.000   Max.   :4.000   Max.   :3.889

``` r
cor(select(hpt_scores, POP_raw, ROA, CONT, HPT_CTX6, HPT_TOT9), use = "pairwise.complete.obs")
```

    ##             POP_raw        ROA       CONT   HPT_CTX6   HPT_TOT9
    ## POP_raw   1.0000000 -0.1530998 -0.2763074 -0.7695631 -0.6452167
    ## ROA      -0.1530998  1.0000000  0.3694237  0.3351715  0.7105332
    ## CONT     -0.2763074  0.3694237  1.0000000  0.8263467  0.7871798
    ## HPT_CTX6 -0.7695631  0.3351715  0.8263467  1.0000000  0.9011123
    ## HPT_TOT9 -0.6452167  0.7105332  0.7871798  0.9011123  1.0000000

# Step 2 -- Reliability: $\alpha$ and $\omega$ for POP-ROA-CONT

``` r
alpha_poly <- function(x) {
  pc <- psych::polychoric(x)$rho
  psych::alpha(pc, n.obs = nrow(x))
}
omega_poly <- function(x) {
  pc <- psych::polychoric(x)$rho
  psych::omega(pc, n.obs = nrow(x), nfactors = 1, plot = FALSE)
}

subsets <- list(
  POP  = hpt_items %>% select(starts_with("POP")),
  ROA  = hpt_items %>% select(starts_with("ROA")),
  CONT = hpt_items %>% select(starts_with("CONT"))
)

rel_table <- purrr::imap_dfr(subsets, function(df, nm){
  a_raw  <- psych::alpha(df)
  a_poly <- alpha_poly(df)
  om     <- omega_poly(df)
  tibble(
    scale = nm,
    k_items = ncol(df),
    alpha_raw  = unname(a_raw$total$raw_alpha),
    alpha_poly = unname(a_poly$total$raw_alpha),
    omega_total = unname(om$omega.tot),
    omega_hier  = unname(om$omega.h)
  )
})
```

    ## Omega_h for 1 factor is not meaningful, just omega_t
    ## Omega_h for 1 factor is not meaningful, just omega_t
    ## Omega_h for 1 factor is not meaningful, just omega_t

``` r
print_tbl(rel_table, digits = 3, caption = "Reliability of HPT subscales (alpha and omega).")
```

```{=html}
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
```
```{=html}
<caption>
```
Reliability of HPT subscales (alpha and omega).
```{=html}
</caption>
```
```{=html}
<thead>
```
```{=html}
<tr>
```
```{=html}
<th style="text-align:left;">
```
scale
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
k_items
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
alpha_raw
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
alpha_poly
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
omega_total
```{=html}
</th>
```
```{=html}
</tr>
```
```{=html}
</thead>
```
```{=html}
<tbody>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
POP
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
3
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.466
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.525
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.546
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
ROA
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
3
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.502
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.525
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.558
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
CONT
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
3
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.636
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.689
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.693
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
</tbody>
```
```{=html}
</table>
```
# Step 3 -- Dimensionality (CFA/EFA)

``` r
hpt_ord <- hpt_items  # treat items as ordered

m1_2factor <- '
F1 =~ POP1 + POP2 + POP3 + CONT1 + CONT2 + CONT3
F2 =~ ROA1 + ROA2 + ROA3
F1 ~~ F2
'
m2_3factor <- '
POP  =~ POP1 + POP2 + POP3
CONT =~ CONT1 + CONT2 + CONT3
ROA  =~ ROA1 + ROA2 + ROA3
POP ~~ CONT + ROA
CONT ~~ ROA
'
m3_1factor <- '
G =~ POP1 + POP2 + POP3 + ROA1 + ROA2 + ROA3 + CONT1 + CONT2 + CONT3
'

fit_2 <- cfa(m1_2factor, data = hpt_ord, ordered = hpt_cols, estimator = "WLSMV")
fit_3 <- cfa(m2_3factor, data = hpt_ord, ordered = hpt_cols, estimator = "WLSMV")
fit_1 <- cfa(m3_1factor, data = hpt_ord, ordered = hpt_cols, estimator = "WLSMV")

# Compare fits
semTools::compareFit(fit_2, fit_3, fit_1)
```

    ## The following lavaan models were compared:
    ##     fit_3
    ##     fit_2
    ##     fit_1
    ## To view results, assign the compareFit() output to an object and  use the summary() method; see the class?FitDiff help page.

``` r
report_fit <- function(fit) {
  list(
    indices = fitMeasures(fit, c(
      "chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled",
      "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled",
      "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled", "srmr"
    )),
    loadings = standardizedSolution(fit) %>% as_tibble() %>% filter(op == "=~")
  )
}

cfa_summary <- list(
  `2-factor (POP+CONT vs ROA)` = report_fit(fit_2),
  `3-factor (POP/CONT/ROA)`    = report_fit(fit_3),
  `1-factor (general)`         = report_fit(fit_1)
)

purrr::iwalk(cfa_summary, function(x, nm){
  cat("\n###", nm, "\n")
  print(x$indices)
  print(kable(x$loadings, digits = 3))
})
```

    ##
    ## ### 2-factor (POP+CONT vs ROA)
    ##          chisq.scaled             df.scaled         pvalue.scaled            cfi.scaled            tli.scaled
    ##                61.297                26.000                 0.000                 0.913                 0.880
    ##          rmsea.scaled rmsea.ci.lower.scaled rmsea.ci.upper.scaled   rmsea.pvalue.scaled                  srmr
    ##                 0.070                 0.048                 0.093                 0.069                 0.066
    ##
    ##
    ## |lhs |op |rhs   | est.std|    se|      z| pvalue| ci.lower| ci.upper|
    ## |:---|:--|:-----|-------:|-----:|------:|------:|--------:|--------:|
    ## |F1  |=~ |POP1  |  -0.489| 0.066| -7.385|  0.000|   -0.619|   -0.359|
    ## |F1  |=~ |POP2  |  -0.191| 0.068| -2.822|  0.005|   -0.324|   -0.058|
    ## |F1  |=~ |POP3  |  -0.358| 0.061| -5.907|  0.000|   -0.477|   -0.239|
    ## |F1  |=~ |CONT1 |   0.690| 0.055| 12.630|  0.000|    0.583|    0.797|
    ## |F1  |=~ |CONT2 |   0.595| 0.058| 10.258|  0.000|    0.481|    0.709|
    ## |F1  |=~ |CONT3 |   0.618| 0.054| 11.416|  0.000|    0.512|    0.724|
    ## |F2  |=~ |ROA1  |   0.604| 0.072|  8.410|  0.000|    0.463|    0.745|
    ## |F2  |=~ |ROA2  |   0.293| 0.075|  3.916|  0.000|    0.146|    0.439|
    ## |F2  |=~ |ROA3  |   0.676| 0.075|  9.016|  0.000|    0.529|    0.822|
    ##
    ## ### 3-factor (POP/CONT/ROA)
    ##          chisq.scaled             df.scaled         pvalue.scaled            cfi.scaled            tli.scaled
    ##                34.258                24.000                 0.080                 0.975                 0.962
    ##          rmsea.scaled rmsea.ci.lower.scaled rmsea.ci.upper.scaled   rmsea.pvalue.scaled                  srmr
    ##                 0.039                 0.000                 0.067                 0.702                 0.048
    ##
    ##
    ## |lhs  |op |rhs   | est.std|    se|      z| pvalue| ci.lower| ci.upper|
    ## |:----|:--|:-----|-------:|-----:|------:|------:|--------:|--------:|
    ## |POP  |=~ |POP1  |   0.735| 0.099|  7.445|      0|    0.542|    0.929|
    ## |POP  |=~ |POP2  |   0.295| 0.076|  3.865|      0|    0.145|    0.444|
    ## |POP  |=~ |POP3  |   0.501| 0.068|  7.414|      0|    0.368|    0.633|
    ## |CONT |=~ |CONT1 |   0.716| 0.055| 13.084|      0|    0.608|    0.823|
    ## |CONT |=~ |CONT2 |   0.609| 0.059| 10.395|      0|    0.494|    0.724|
    ## |CONT |=~ |CONT3 |   0.637| 0.054| 11.867|      0|    0.532|    0.743|
    ## |ROA  |=~ |ROA1  |   0.606| 0.071|  8.524|      0|    0.467|    0.745|
    ## |ROA  |=~ |ROA2  |   0.296| 0.074|  3.972|      0|    0.150|    0.442|
    ## |ROA  |=~ |ROA3  |   0.672| 0.074|  9.070|      0|    0.527|    0.817|
    ##
    ## ### 1-factor (general)
    ##          chisq.scaled             df.scaled         pvalue.scaled            cfi.scaled            tli.scaled
    ##                81.538                27.000                 0.000                 0.866                 0.821
    ##          rmsea.scaled rmsea.ci.lower.scaled rmsea.ci.upper.scaled   rmsea.pvalue.scaled                  srmr
    ##                 0.086                 0.065                 0.107                 0.003                 0.075
    ##
    ##
    ## |lhs |op |rhs   | est.std|    se|       z| pvalue| ci.lower| ci.upper|
    ## |:---|:--|:-----|-------:|-----:|-------:|------:|--------:|--------:|
    ## |G   |=~ |POP1  |   0.479| 0.066|   7.262|  0.000|    0.349|    0.608|
    ## |G   |=~ |POP2  |   0.177| 0.067|   2.649|  0.008|    0.046|    0.308|
    ## |G   |=~ |POP3  |   0.344| 0.061|   5.679|  0.000|    0.225|    0.462|
    ## |G   |=~ |ROA1  |  -0.467| 0.061|  -7.595|  0.000|   -0.587|   -0.346|
    ## |G   |=~ |ROA2  |  -0.227| 0.067|  -3.397|  0.001|   -0.358|   -0.096|
    ## |G   |=~ |ROA3  |  -0.515| 0.059|  -8.782|  0.000|   -0.630|   -0.400|
    ## |G   |=~ |CONT1 |  -0.672| 0.053| -12.761|  0.000|   -0.775|   -0.569|
    ## |G   |=~ |CONT2 |  -0.583| 0.057| -10.319|  0.000|   -0.694|   -0.472|
    ## |G   |=~ |CONT3 |  -0.596| 0.052| -11.471|  0.000|   -0.697|   -0.494|

### Optional: EFA (polychoric)

``` r
pc <- psych::polychoric(hpt_ord)$rho
efa2 <- psych::fa(pc, nfactors = 2, fm = "pa", rotate = "oblimin")
efa3 <- psych::fa(pc, nfactors = 3, fm = "pa", rotate = "oblimin")

cat("\nEFA (2 factors):\n")
```

    ##
    ## EFA (2 factors):

``` r
print(efa2$loadings, cutoff = 0.25)
```

    ##
    ## Loadings:
    ##       PA1    PA2
    ## POP1  -0.288  0.371
    ## POP2          0.448
    ## POP3          0.635
    ## ROA1   0.563
    ## ROA2   0.356
    ## ROA3   0.579
    ## CONT1  0.631
    ## CONT2  0.527
    ## CONT3  0.521
    ##
    ##                  PA1   PA2
    ## SS loadings    1.811 0.833
    ## Proportion Var 0.201 0.093
    ## Cumulative Var 0.201 0.294

``` r
cat("\nEFA (3 factors):\n")
```

    ##
    ## EFA (3 factors):

``` r
print(efa3$loadings, cutoff = 0.25)
```

    ##
    ## Loadings:
    ##       PA3    PA1    PA2
    ## POP1                 0.397
    ## POP2                 0.382
    ## POP3                 0.763
    ## ROA1          0.577
    ## ROA2          0.456
    ## ROA3          0.585
    ## CONT1  0.507
    ## CONT2  0.396
    ## CONT3  0.816
    ##
    ##                  PA3   PA1   PA2
    ## SS loadings    1.109 1.035 0.912
    ## Proportion Var 0.123 0.115 0.101
    ## Cumulative Var 0.123 0.238 0.340

# Step 4 -- Presentism-contextualization contrast (POP vs CONT)

``` r
contrast_tbl <- hpt_scores %>%
  summarise(
    mean_POP   = mean(POP_raw,  na.rm = TRUE),  sd_POP   = sd(POP_raw,  na.rm = TRUE),
    mean_CONT  = mean(CONT,     na.rm = TRUE),  sd_CONT  = sd(CONT,     na.rm = TRUE),
    r_POP_CONT = cor(POP_raw, CONT, use = "pairwise.complete.obs")
  )

print_tbl(contrast_tbl, digits = 3, caption = "POP (raw) vs CONT: means, SDs, and correlation.")
```

```{=html}
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
```
```{=html}
<caption>
```
POP (raw) vs CONT: means, SDs, and correlation.
```{=html}
</caption>
```
```{=html}
<thead>
```
```{=html}
<tr>
```
```{=html}
<th style="text-align:right;">
```
mean_POP
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
sd_POP
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
mean_CONT
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
sd_CONT
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
r_POP_CONT
```{=html}
</th>
```
```{=html}
</tr>
```
```{=html}
</thead>
```
```{=html}
<tbody>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:right;">
```
2.029
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.646
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
2.713
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.732
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.276
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
</tbody>
```
```{=html}
</table>
```
``` r
t_test <- t.test(hpt_scores$POP_raw, hpt_scores$CONT, paired = TRUE)
t_test
```

    ##
    ##  Paired t-test
    ##
    ## data:  hpt_scores$POP_raw and hpt_scores$CONT
    ## t = -10.306, df = 275, p-value < 2.2e-16
    ## alternative hypothesis: true mean difference is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.8141513 -0.5529985
    ## sample estimates:
    ## mean difference
    ##      -0.6835749

# Step 5 -- Distribution checks

``` r
# Item distributions
long_items <- hpt_items %>%
  pivot_longer(cols = everything(), names_to = "item", values_to = "score")

ggplot(long_items, aes(score)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "left") +
  facet_wrap(~ item, ncol = 3) +
  labs(title = "HPT item score distributions", x = "Score (1-4)", y = "Count")
```

![](/home/yetty/PhD/projects/phd-029-hpt-and-extremism/outputs/01_measurement-checks_files/figure-markdown/distributions-1.png)

``` r
# Scale/composite distributions
long_scales <- hpt_scores %>%
  select(POP_raw, ROA, CONT, HPT_CTX6, HPT_TOT9) %>%
  pivot_longer(everything(), names_to = "scale", values_to = "score")

ggplot(long_scales, aes(x = score)) +
  geom_histogram(binwidth = 0.25) +
  facet_wrap(~ scale, scales = "free") +
  labs(title = "Subscales and composites", x = "Mean score", y = "Count")
```

![](/home/yetty/PhD/projects/phd-029-hpt-and-extremism/outputs/01_measurement-checks_files/figure-markdown/distributions-2.png)

# Step 6 -- Knowledge mini-test (KN1-KN6)

``` r
kn_cols <- paste0("KN", 1:6)
has_kn  <- all(kn_cols %in% names(dat))

if (!has_kn) {
  cat("\n**Knowledge section skipped:** KN1-KN6 not found in data.\n")
} else {
  kn_items <- dat[keep, kn_cols]  # align to analysis_df rows via 'keep'
  # Basic sanity: coerce to numeric 0/1
  kn_items <- kn_items %>% mutate(across(everything(), ~ as.numeric(.)))

  # Total score, difficulty (p), discrimination (point-biserial)
  kn_total <- rowSums(kn_items, na.rm = TRUE)

  item_stats <- tibble(
    item = kn_cols,
    difficulty_p = sapply(kn_items, function(x) mean(x, na.rm = TRUE)),
    discr_pb = sapply(kn_items, function(x) cor(x, kn_total - x, use = "pairwise.complete.obs"))
  )

  # KR-20 (alpha on dichotomous items)
  kn_alpha <- psych::alpha(kn_items)

  print_tbl(item_stats, digits = 3, caption = "KN items: difficulty (p) and point-biserial discrimination.")

  print_tbl(tibble(
    k_items = ncol(kn_items),
    total_mean = mean(kn_total, na.rm = TRUE),
    total_sd   = sd(kn_total, na.rm = TRUE),
    alpha_KR20 = unname(kn_alpha$total$raw_alpha)
  ), digits = 3, caption = "KN total: summary and KR-20 (alpha for dichotomous items).")
}
```

```{=html}
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
```
```{=html}
<caption>
```
KN total: summary and KR-20 (alpha for dichotomous items).
```{=html}
</caption>
```
```{=html}
<thead>
```
```{=html}
<tr>
```
```{=html}
<th style="text-align:right;">
```
k_items
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
total_mean
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
total_sd
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
alpha_KR20
```{=html}
</th>
```
```{=html}
</tr>
```
```{=html}
</thead>
```
```{=html}
<tbody>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:right;">
```
6
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
3.029
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.622
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.544
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
</tbody>
```
```{=html}
</table>
```
# Step 8 -- Ideology batteries (FR-LF mini, KSA-3) and Social Desirability (SDR-5)

``` r
# Helper: reliability table for Likert batteries (polychoric + omega total)
alpha_poly_likert <- function(x) {
  pc <- psych::polychoric(x)$rho
  psych::alpha(pc, n.obs = nrow(x))
}
omega_total_poly_likert <- function(x) {
  pc <- psych::polychoric(x)$rho
  if (!all(eigen(pc, symmetric = TRUE)$values > 1e-6)) pc <- psych::cor.smooth(pc)
  suppressWarnings(psych::omega(pc, n.obs = nrow(x), nfactors = 1, plot = FALSE)$omega.tot)
}
```

## FR-LF mini (RD1-RD3, NS1-NS3)

``` r
fr_cols <- c(paste0("RD", 1:3), paste0("NS", 1:3))
has_fr  <- all(fr_cols %in% names(dat))

if (!has_fr) {
  cat("\n**FR-LF mini section skipped:** RD1-3 and/or NS1-3 not found.\n")
} else {
  fr_df <- dat[keep, fr_cols] %>% as_tibble()
  RD <- fr_df %>% select(starts_with("RD"))
  NS <- fr_df %>% select(starts_with("NS"))

  fr_rel <- bind_rows(
    {
      a <- psych::alpha(RD); ap <- alpha_poly_likert(RD); wt <- omega_total_poly_likert(RD)
      tibble(scale = "FR-LF: RD", k_items = ncol(RD),
             alpha_raw = a$total$raw_alpha, alpha_poly = ap$total$raw_alpha, omega_total = wt)
    },
    {
      a <- psych::alpha(NS); ap <- alpha_poly_likert(NS); wt <- omega_total_poly_likert(NS)
      tibble(scale = "FR-LF: NS", k_items = ncol(NS),
             alpha_raw = a$total$raw_alpha, alpha_poly = ap$total$raw_alpha, omega_total = wt)
    },
    {
      a <- psych::alpha(fr_df); ap <- alpha_poly_likert(fr_df); wt <- omega_total_poly_likert(fr_df)
      tibble(scale = "FR-LF: total (RD+NS)", k_items = ncol(fr_df),
             alpha_raw = a$total$raw_alpha, alpha_poly = ap$total$raw_alpha, omega_total = wt)
    }
  )

  print_tbl(fr_rel, digits = 3, caption = "FR-LF mini reliability (alpha, polychoric alpha, omega total).")

  # Optional CFA: 2 correlated factors (RD, NS), ordered WLSMV
  fr_model <- '
  RD =~ RD1 + RD2 + RD3
  NS =~ NS1 + NS2 + NS3
  RD ~~ NS
  '
  fr_fit <- try(lavaan::cfa(fr_model, data = fr_df, ordered = colnames(fr_df), estimator = "WLSMV"), silent = TRUE)
  if (!inherits(fr_fit, "try-error")) {
    print(fitMeasures(fr_fit, c(
      "chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled",
      "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled",
      "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled", "srmr"
    )))
  } else {
    cat("\nFR-LF CFA skipped (model failed to converge).\n")
  }
}
```

    ## Omega_h for 1 factor is not meaningful, just omega_t
    ## Omega_h for 1 factor is not meaningful, just omega_t
    ## Omega_h for 1 factor is not meaningful, just omega_t

    ##          chisq.scaled             df.scaled         pvalue.scaled            cfi.scaled            tli.scaled
    ##                29.384                 8.000                 0.000                 0.939                 0.886
    ##          rmsea.scaled rmsea.ci.lower.scaled rmsea.ci.upper.scaled   rmsea.pvalue.scaled                  srmr
    ##                 0.101                 0.063                 0.141                 0.015                 0.055

## KSA-3 (A1-A3, U1-U3, K1-K3)

``` r
ksa_cols <- c(paste0("A",1:3), paste0("U",1:3), paste0("K",1:3))
has_ksa  <- all(ksa_cols %in% names(dat))

if (!has_ksa) {
  cat("\n**KSA-3 section skipped:** A1-A3, U1-U3, and/or K1-K3 not found.\n")
} else {
  ksa_df <- dat[keep, ksa_cols] %>% as_tibble()
  A <- ksa_df %>% select(starts_with("A"))
  U <- ksa_df %>% select(starts_with("U"))
  K <- ksa_df %>% select(starts_with("K"))

  ksa_rel <- bind_rows(
    {
      a <- psych::alpha(A); ap <- alpha_poly_likert(A); wt <- omega_total_poly_likert(A)
      tibble(scale = "KSA-3: Aggression (A)", k_items = 3,
             alpha_raw = a$total$raw_alpha, alpha_poly = ap$total$raw_alpha, omega_total = wt)
    },
    {
      a <- psych::alpha(U); ap <- alpha_poly_likert(U); wt <- omega_total_poly_likert(U)
      tibble(scale = "KSA-3: Submission (U)", k_items = 3,
             alpha_raw = a$total$raw_alpha, alpha_poly = ap$total$raw_alpha, omega_total = wt)
    },
    {
      a <- psych::alpha(K); ap <- alpha_poly_likert(K); wt <- omega_total_poly_likert(K)
      tibble(scale = "KSA-3: Conventionalism (K)", k_items = 3,
             alpha_raw = a$total$raw_alpha, alpha_poly = ap$total$raw_alpha, omega_total = wt)
    },
    {
      a <- psych::alpha(ksa_df); ap <- alpha_poly_likert(ksa_df); wt <- omega_total_poly_likert(ksa_df)
      tibble(scale = "KSA-3: total", k_items = 9,
             alpha_raw = a$total$raw_alpha, alpha_poly = ap$total$raw_alpha, omega_total = wt)
    }
  )

  print_tbl(ksa_rel, digits = 3, caption = "KSA-3 reliability (alpha, polychoric alpha, omega total).")

  # Optional CFA: 3 correlated factors (A, U, K)
  ksa_model <- '
  A =~ A1 + A2 + A3
  U =~ U1 + U2 + U3
  K =~ K1 + K2 + K3
  A ~~ U + K
  U ~~ K
  '
  ksa_fit <- try(lavaan::cfa(ksa_model, data = ksa_df, ordered = colnames(ksa_df), estimator = "WLSMV"), silent = TRUE)
  if (!inherits(ksa_fit, "try-error")) {
    print(fitMeasures(ksa_fit, c(
      "chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled",
      "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled",
      "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled", "srmr"
    )))
  } else {
    cat("\nKSA-3 CFA skipped (model failed to converge).\n")
  }
}
```

    ## Omega_h for 1 factor is not meaningful, just omega_t
    ## Omega_h for 1 factor is not meaningful, just omega_t
    ## Omega_h for 1 factor is not meaningful, just omega_t
    ## Omega_h for 1 factor is not meaningful, just omega_t

    ##          chisq.scaled             df.scaled         pvalue.scaled            cfi.scaled            tli.scaled
    ##                75.977                24.000                 0.000                 0.898                 0.847
    ##          rmsea.scaled rmsea.ci.lower.scaled rmsea.ci.upper.scaled   rmsea.pvalue.scaled                  srmr
    ##                 0.092                 0.069                 0.116                 0.002                 0.064

## SDR-5 (SDR1-SDR5)

``` r
sdr_cols <- paste0("SDR", 1:5)
has_sdr  <- all(sdr_cols %in% names(dat))

if (!has_sdr) {
  cat("\n**SDR-5 section skipped:** SDR1-SDR5 not found.\n")
} else {
  sdr_df <- dat[keep, sdr_cols] %>% as_tibble()
  a_sdr  <- psych::alpha(sdr_df)
  ap_sdr <- alpha_poly_likert(sdr_df)
  wt_sdr <- omega_total_poly_likert(sdr_df)

  print_tbl(tibble(
    scale = "SDR-5",
    k_items = 5,
    alpha_raw = a_sdr$total$raw_alpha,
    alpha_poly = ap_sdr$total$raw_alpha,
    omega_total = wt_sdr
  ), digits = 3, caption = "SDR-5 reliability (alpha, polychoric alpha, omega total).")

  # Optional CFA: 1 factor
  sdr_model <- 'SDR =~ SDR1 + SDR2 + SDR3 + SDR4 + SDR5'
  sdr_fit <- try(lavaan::cfa(sdr_model, data = sdr_df, ordered = colnames(sdr_df), estimator = "WLSMV"), silent = TRUE)
  if (!inherits(sdr_fit, "try-error")) {
    print(fitMeasures(sdr_fit, c(
      "chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled",
      "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled",
      "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled", "srmr"
    )))
  } else {
    cat("\nSDR-5 CFA skipped (model failed to converge).\n")
  }
}
```

    ## Some items ( SDR1 SDR5 ) were negatively correlated with the first principal component and
    ## probably should be reversed.
    ## To do this, run the function again with the 'check.keys=TRUE' option

    ## Some items ( SDR1 SDR5 ) were negatively correlated with the first principal component and
    ## probably should be reversed.
    ## To do this, run the function again with the 'check.keys=TRUE' option

    ## Omega_h for 1 factor is not meaningful, just omega_t

    ##          chisq.scaled             df.scaled         pvalue.scaled            cfi.scaled            tli.scaled
    ##                61.401                 5.000                 0.000                 0.640                 0.279
    ##          rmsea.scaled rmsea.ci.lower.scaled rmsea.ci.upper.scaled   rmsea.pvalue.scaled                  srmr
    ##                 0.206                 0.162                 0.254                 0.000                 0.103

# Step 9 -- Cross-construct correlations (HPT, KN, FR-LF, KSA-3, SDR-5)

``` r
# Build scale scores that exist in your data (gracefully skipping any missing block)
scales_list <- list(
  HPT_CTX6  = hpt_scores$HPT_CTX6,
  HPT_TOT9  = hpt_scores$HPT_TOT9,
  HPT_POP   = hpt_scores$POP_raw,  # presentism foil (higher = worse)
  HPT_ROA   = hpt_scores$ROA,
  HPT_CONT  = hpt_scores$CONT
)

# optional blocks
kn_cols <- paste0("KN", 1:6); has_kn <- all(kn_cols %in% names(dat))
fr_cols <- c(paste0("RD", 1:3), paste0("NS", 1:3)); has_fr <- all(fr_cols %in% names(dat))
ksa_cols <- c(paste0("A",1:3), paste0("U",1:3), paste0("K",1:3)); has_ksa <- all(ksa_cols %in% names(dat))
sdr_cols <- paste0("SDR", 1:5); has_sdr <- all(sdr_cols %in% names(dat))

if (has_kn) {
  scales_list$KN_total <- rowSums(dat[keep, kn_cols], na.rm = TRUE)
}

if (has_fr) {
  fr_df <- dat[keep, fr_cols]
  scales_list$FR_RD     <- scale_mean(fr_df, paste0("RD", 1:3), min_answered = 2)
  scales_list$FR_NS     <- scale_mean(fr_df, paste0("NS", 1:3), min_answered = 2)
  scales_list$FR_total  <- scale_mean(fr_df, fr_cols, min_answered = 4)
}

if (has_ksa) {
  ksa_df <- dat[keep, ksa_cols]
  scales_list$KSA_A     <- scale_mean(ksa_df, paste0("A", 1:3), min_answered = 2)
  scales_list$KSA_U     <- scale_mean(ksa_df, paste0("U", 1:3), min_answered = 2)
  scales_list$KSA_K     <- scale_mean(ksa_df, paste0("K", 1:3), min_answered = 2)
  scales_list$KSA_total <- scale_mean(ksa_df, ksa_cols, min_answered = 7)
}

if (has_sdr) {
  sdr_df <- dat[keep, sdr_cols]
  scales_list$SDR_total <- scale_mean(sdr_df, sdr_cols, min_answered = 4)
}

scales_df <- as_tibble(scales_list)

# Pairwise complete correlations
cors <- cor(scales_df, use = "pairwise.complete.obs")

print_tbl(round(cors, 3), caption = "Cross-construct correlations (pairwise complete).")
```

```{=html}
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
```
```{=html}
<caption>
```
Cross-construct correlations (pairwise complete).
```{=html}
</caption>
```
```{=html}
<thead>
```
```{=html}
<tr>
```
```{=html}
<th style="text-align:left;">
```
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
HPT_CTX6
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
HPT_TOT9
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
HPT_POP
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
HPT_ROA
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
HPT_CONT
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
KN_total
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
FR_RD
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
FR_NS
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
FR_total
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
KSA_A
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
KSA_U
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
KSA_K
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
KSA_total
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
SDR_total
```{=html}
</th>
```
```{=html}
</tr>
```
```{=html}
</thead>
```
```{=html}
<tbody>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
HPT_CTX6
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.000
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.901
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.770
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.335
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.826
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.340
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.062
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.003
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.037
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.008
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.072
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.011
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.031
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.027
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
HPT_TOT9
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.901
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.000
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.645
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.711
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.787
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.378
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.036
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.003
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.024
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.038
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.040
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.026
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.012
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.017
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
HPT_POP
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.770
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.645
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.000
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.153
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.276
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.307
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.091
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.063
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.093
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.065
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.141
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.115
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.139
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.041
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
HPT_ROA
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.335
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.711
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.153
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.000
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.369
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.268
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.022
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.010
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.007
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.070
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.030
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.074
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.077
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.007
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
HPT_CONT
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.826
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.787
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.276
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.369
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.000
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.241
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.012
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.060
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.027
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.070
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.017
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.086
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.076
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.004
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
KN_total
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.340
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.378
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.307
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.268
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.241
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.000
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.062
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.106
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.095
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.037
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.012
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.082
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.056
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.004
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
FR_RD
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.062
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.036
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.091
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.022
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.012
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.062
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.000
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.423
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.839
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.379
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.415
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.359
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.499
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.068
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
FR_NS
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.003
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.003
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.063
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.010
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.060
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.106
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.423
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.000
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.847
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.353
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.371
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.228
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.414
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.242
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
FR_total
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.037
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.024
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.093
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.007
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.027
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.095
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.839
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.847
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.000
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.433
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.467
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.348
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.540
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.185
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
KSA_A
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.008
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.038
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.065
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.070
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.070
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.037
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.379
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.353
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.433
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.000
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.339
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.423
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.790
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.149
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
KSA_U
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.072
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.040
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.141
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.030
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.017
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.012
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.415
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.371
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.467
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.339
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.000
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.387
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.723
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.174
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
KSA_K
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.011
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.026
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.115
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.074
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.086
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.082
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.359
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.228
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.348
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.423
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.387
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.000
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.787
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.012
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
KSA_total
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.031
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.012
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.139
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.077
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.076
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.056
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.499
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.414
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.540
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.790
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.723
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.787
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.000
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.144
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
<tr>
```
```{=html}
<td style="text-align:left;">
```
SDR_total
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.027
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.017
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.041
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.007
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.004
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.004
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.068
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.242
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.185
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.149
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.174
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.012
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.144
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.000
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
</tbody>
```
```{=html}
</table>
```
# Reproducibility appendix

``` r
sessionInfo()
```

    ## R version 4.6.1 (2026-06-24)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.5 LTS
    ##
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.12.0
    ## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.12.0  LAPACK version 3.12.0
    ##
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=cs_CZ.UTF-8        LC_COLLATE=en_US.UTF-8
    ##  [5] LC_MONETARY=cs_CZ.UTF-8    LC_MESSAGES=en_US.UTF-8    LC_PAPER=cs_CZ.UTF-8       LC_NAME=C
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=cs_CZ.UTF-8 LC_IDENTIFICATION=C
    ##
    ## time zone: Europe/Prague
    ## tzcode source: system (glibc)
    ##
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base
    ##
    ## other attached packages:
    ##  [1] kableExtra_1.4.1 knitr_1.51       semTools_0.5-9   lavaan_0.7-2     psych_2.6.5      lubridate_1.9.5
    ##  [7] forcats_1.0.1    stringr_1.6.0    dplyr_1.2.1      purrr_1.2.2      readr_2.2.0      tidyr_1.3.2
    ## [13] tibble_3.3.1     ggplot2_4.0.3    tidyverse_2.0.0
    ##
    ## loaded via a namespace (and not attached):
    ##  [1] GPArotation_2026.8-2 generics_0.1.4       xml2_1.6.0           stringi_1.8.7        lattice_0.23-1
    ##  [6] hms_1.1.4            digest_0.6.39        magrittr_2.0.5       evaluate_1.0.5       grid_4.6.1
    ## [11] timechange_0.4.0     RColorBrewer_1.1-3   fastmap_1.2.0        tinytex_0.61         viridisLite_0.4.3
    ## [16] scales_1.4.0         pbivnorm_0.6.0       textshaping_1.0.5    mnormt_2.1.2         cli_3.6.6
    ## [21] rlang_1.3.0          withr_3.0.3          yaml_2.3.12          otel_0.2.0           tools_4.6.1
    ## [26] parallel_4.6.1       tzdb_0.5.0           vctrs_0.7.3          R6_2.6.1             stats4_4.6.1
    ## [31] lifecycle_1.0.5      MASS_7.3-66          pkgconfig_2.0.3      pillar_1.11.1        gtable_0.3.6
    ## [36] glue_1.8.1           systemfonts_1.3.2    xfun_0.60            tidyselect_1.2.1     rstudioapi_0.19.0
    ## [41] farver_2.1.2         htmltools_0.5.9      nlme_3.1-171         labeling_0.4.3       svglite_2.2.2
    ## [46] rmarkdown_2.32       compiler_4.6.1       S7_0.2.2             quadprog_1.5-8
