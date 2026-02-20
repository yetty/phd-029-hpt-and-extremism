# What this document does

This report checks whether our **Historical Perspective-Taking (HPT)**
instrument behaves well **before** we run any hypothesis tests.

We do four things:

1.  **Reliability:** Are the HPT subscales internally consistent? We
    report **Cronbach's alpha ($\alpha$)** and **McDonald's omega
    ($\omega$)** for **POP**, **ROA**, **CONT** (three items each;
    1--4).
2.  **Dimensionality (CFA/EFA):** Does the **factor structure** match
    prior research (e.g., **POP+CONT** vs **ROA**, or three correlated
    factors)?
3.  **Presentism--contextualization contrast:** Do **POP** (presentist)
    and **CONT** (contextualization) show the expected contrast?
4.  **Clustering (ICCs):** Are scores clustered by **school** and
    **class-within-school**?

> **Composite scores used** (for descriptives/ICCs and later files): -
> **POP_rev = 5 − POP_raw** (so higher = more contextualized). -
> **HPT_CTX6 = mean(POP_rev, CONT)** (default composite). - **HPT_TOT9 =
> mean(POP_rev, CONT, ROA)** (includes ROA; robustness).

# Setup and data loading

``` r
options(width = 120)

# Data handling & plots
library(tidyverse)

# Psychometrics
library(psych)        # alpha, omega, polychoric, EFA helpers
library(lavaan)       # CFA
library(semTools)     # model comparisons & extras

# Multilevel ICCs
library(lme4)
library(performance)

# Tables
library(knitr)

# Make kableExtra use longtable/booktabs and avoid loading tabu
options(kableExtra.latex.load_packages = FALSE)
library(kableExtra)

# Load the dataset created in 00_data-preparation
load("normalised_responses.RData")
stopifnot(exists("normalised_responses"))
dat <- normalised_responses

# Ensure clustering identifiers and unique class id
stopifnot(all(c("school_id","class_label") %in% names(dat)))
dat <- dat %>%
  mutate(
    school_id   = as.factor(school_id),
    class_label = as.factor(class_label),
    class_id    = interaction(school_id, class_label, drop = TRUE)
  )

# POP reversed item-wise and subscale helper
POP_rev_items <- paste0("POP", 1:3)

# Reverse POP items (1–4)
dat <- dat %>%
  mutate(
    across(
      all_of(POP_rev_items),
      ~ 5 - suppressWarnings(as.numeric(.)),
      .names = "{.col}_rev"     # <<< THIS was the culprit
    )
  ) %>%
  mutate(
    HPT_POP_raw = rowMeans(across(all_of(POP_rev_items)), na.rm = TRUE),
    HPT_POP_rev = rowMeans(across(all_of(paste0(POP_rev_items, "_rev"))), na.rm = TRUE),
    HPT_CONT    = rowMeans(across(CONT1:CONT3), na.rm = TRUE),
    HPT_ROA     = rowMeans(across(ROA1:ROA3),   na.rm = TRUE),
    HPT_CTX6    = rowMeans(cbind(HPT_POP_rev, HPT_CONT), na.rm = TRUE),
    HPT_TOT9    = rowMeans(cbind(HPT_POP_rev, HPT_CONT, HPT_ROA), na.rm = TRUE)
  )

print_tbl <- function(df, caption, digits = 3, escape = TRUE) {
  kbl(df, booktabs = TRUE, longtable = TRUE, caption = caption, digits = digits, escape = escape) |>
    kable_styling(full_width = FALSE, latex_options = c("hold_position"))
}
```

We verify that **HPT items** and **class labels** exist. If something is
missing, we stop with a clear message.

``` r
## -- check-columns ----------------------------
hpt_cols <- c(paste0("POP", 1:3), paste0("ROA", 1:3), paste0("CONT", 1:3))
need   <- c(hpt_cols, "class_label", "school_id")
miss   <- setdiff(need, names(dat))
if (length(miss)) stop("Missing variables: ", paste(miss, collapse = ", "))

# Keep only rows that are COMPLETE on all HPT items AND have class_label and school_id
keep <- complete.cases(dat[, hpt_cols]) & !is.na(dat$class_label) & !is.na(dat$school_id)

analysis_df <- dat[keep, c(hpt_cols, "class_label", "school_id", "class_id")] |> 
  as_tibble()

nrow_all   <- nrow(dat)
nrow_keep  <- nrow(analysis_df)
cat("Rows in full data: ", nrow_all,  "\n",
    "Rows kept (complete HPT + class_label + school_id): ", nrow_keep, "\n", sep = "")
```

    ## Rows in full data: 293
    ## Rows kept (complete HPT + class_label + school_id): 276

# Step 1 --- Descriptives and scale construction

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

# Step 2 --- Reliability: $\alpha$ and $\omega$ for POP--ROA--CONT

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
# Step 3 --- Dimensionality (CFA/EFA)

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
    indices = fitMeasures(fit, c("cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr")),
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
    ##            cfi            tli          rmsea rmsea.ci.lower rmsea.ci.upper           srmr 
    ##          0.956          0.940          0.057          0.032          0.081          0.066 
    ## 
    ## 
    ## |lhs |op |rhs   | est.std|    se|       z| pvalue| ci.lower| ci.upper|
    ## |:---|:--|:-----|-------:|-----:|-------:|------:|--------:|--------:|
    ## |F1  |=~ |POP1  |   0.489| 0.066|   7.385|  0.000|    0.359|    0.619|
    ## |F1  |=~ |POP2  |   0.191| 0.068|   2.822|  0.005|    0.058|    0.324|
    ## |F1  |=~ |POP3  |   0.358| 0.061|   5.907|  0.000|    0.239|    0.477|
    ## |F1  |=~ |CONT1 |  -0.690| 0.055| -12.630|  0.000|   -0.797|   -0.583|
    ## |F1  |=~ |CONT2 |  -0.595| 0.058| -10.258|  0.000|   -0.709|   -0.481|
    ## |F1  |=~ |CONT3 |  -0.618| 0.054| -11.416|  0.000|   -0.724|   -0.512|
    ## |F2  |=~ |ROA1  |   0.604| 0.072|   8.410|  0.000|    0.463|    0.745|
    ## |F2  |=~ |ROA2  |   0.293| 0.075|   3.916|  0.000|    0.146|    0.439|
    ## |F2  |=~ |ROA3  |   0.676| 0.075|   9.016|  0.000|    0.529|    0.822|
    ## 
    ## ### 3-factor (POP/CONT/ROA) 
    ##            cfi            tli          rmsea rmsea.ci.lower rmsea.ci.upper           srmr 
    ##          0.999          0.998          0.011          0.000          0.051          0.048 
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
    ##            cfi            tli          rmsea rmsea.ci.lower rmsea.ci.upper           srmr 
    ##          0.926          0.902          0.073          0.051          0.095          0.075 
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
    ##       PA1    PA3    PA2   
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
    ##                  PA1   PA3   PA2
    ## SS loadings    1.109 1.035 0.912
    ## Proportion Var 0.123 0.115 0.101
    ## Cumulative Var 0.123 0.238 0.340

# Step 4 --- Presentism--contextualization contrast (POP vs CONT)

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

# Step 5 --- Distribution checks

``` r
# Item distributions
long_items <- hpt_items %>%
  pivot_longer(cols = everything(), names_to = "item", values_to = "score")

ggplot(long_items, aes(score)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "left") +
  facet_wrap(~ item, ncol = 3) +
  labs(title = "HPT item score distributions", x = "Score (1-4)", y = "Count")
```

![](/home/yetty/Projects/phd-029-hpt-and-extremism/outputs/01_measurement-checks_files/figure-markdown/distributions-1.png)

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

![](/home/yetty/Projects/phd-029-hpt-and-extremism/outputs/01_measurement-checks_files/figure-markdown/distributions-2.png)

# Step 6 --- Class-level ICCs (multilevel warranted?)

``` r
# Build the DV frame from analysis_df (items) and compute subscales/composites here
icc_data <- analysis_df %>%
  mutate(
    POP_raw = rowMeans(select(., starts_with("POP")),  na.rm = TRUE),
    ROA     = rowMeans(select(., starts_with("ROA")),  na.rm = TRUE),
    CONT    = rowMeans(select(., starts_with("CONT")), na.rm = TRUE)
  ) %>%
  mutate(
    POP_rev  = 5 - POP_raw,
    HPT_CTX6 = rowMeans(cbind(POP_rev, CONT), na.rm = TRUE),
    HPT_TOT9 = rowMeans(cbind(POP_rev, CONT, ROA), na.rm = TRUE)
  ) %>%
  select(school_id, class_label, class_id, POP_raw, ROA, CONT, HPT_CTX6, HPT_TOT9)

mk_icc_3 <- function(dv){
  f <- reformulate("1 + (1|school_id) + (1|school_id:class_label)", response = dv)
  fit <- lmer(f, data = icc_data, REML = TRUE)

  vc <- as.data.frame(VarCorr(fit))
  v_school <- vc$vcov[vc$grp == "school_id"];             v_school <- if (length(v_school)) v_school[1] else 0
  v_classW <- vc$vcov[vc$grp == "school_id:class_label"]; v_classW <- if (length(v_classW)) v_classW[1] else 0
  v_resid  <- vc$vcov[vc$grp == "Residual"][1]
  v_tot    <- v_school + v_classW + v_resid

  tibble(
    DV = dv,
    ICC_school = v_school / v_tot,
    ICC_class_within_school = v_classW / v_tot,
    ICC_total_cluster = (v_school + v_classW) / v_tot,
    singular = isSingular(fit)
  )
}

icc_tbl <- purrr::map_dfr(c("HPT_CTX6","HPT_TOT9","POP_raw","ROA","CONT"), mk_icc_3)
```

    ## boundary (singular) fit: see help('isSingular')
    ## boundary (singular) fit: see help('isSingular')
    ## boundary (singular) fit: see help('isSingular')

``` r
print_tbl(icc_tbl, digits = 3, caption = "ICCs: school and class-within-school (HPT composites and subscales).")
```

```{=html}
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
```
```{=html}
<caption>
```
ICCs: school and class-within-school (HPT composites and subscales).
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
DV
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
ICC_school
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
ICC_class_within_school
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
ICC_total_cluster
```{=html}
</th>
```
```{=html}
<th style="text-align:left;">
```
singular
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
0.041
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.000
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
<td style="text-align:left;">
```
TRUE
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
0.089
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.000
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.089
```{=html}
</td>
```
```{=html}
<td style="text-align:left;">
```
TRUE
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
POP_raw
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
0.000
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
<td style="text-align:left;">
```
TRUE
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
0.099
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
0.107
```{=html}
</td>
```
```{=html}
<td style="text-align:left;">
```
FALSE
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
0.023
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.013
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.036
```{=html}
</td>
```
```{=html}
<td style="text-align:left;">
```
FALSE
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
# Step 7 --- Knowledge mini-test (KN1--KN6)

``` r
kn_cols <- paste0("KN", 1:6)
has_kn  <- all(kn_cols %in% names(dat))

if (!has_kn) {
  cat("\n**Knowledge section skipped:** KN1–KN6 not found in data.\n")
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
# Step 8 --- Ideology batteries (FR-LF mini, KSA-3) and Social Desirability (SDR-5)

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

## FR-LF mini (RD1--RD3, NS1--NS3)

``` r
fr_cols <- c(paste0("RD", 1:3), paste0("NS", 1:3))
has_fr  <- all(fr_cols %in% names(dat))

if (!has_fr) {
  cat("\n**FR-LF mini section skipped:** RD1–3 and/or NS1–3 not found.\n")
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
    print(fitMeasures(fr_fit, c("cfi","tli","rmsea","srmr")))
  } else {
    cat("\nFR-LF CFA skipped (model failed to converge).\n")
  }
}
```

    ## Omega_h for 1 factor is not meaningful, just omega_t
    ## Omega_h for 1 factor is not meaningful, just omega_t
    ## Omega_h for 1 factor is not meaningful, just omega_t

    ##   cfi   tli rmsea  srmr 
    ## 0.977 0.956 0.072 0.055

## KSA-3 (A1--A3, U1--U3, K1--K3)

``` r
ksa_cols <- c(paste0("A",1:3), paste0("U",1:3), paste0("K",1:3))
has_ksa  <- all(ksa_cols %in% names(dat))

if (!has_ksa) {
  cat("\n**KSA-3 section skipped:** A1–A3, U1–U3, and/or K1–K3 not found.\n")
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
    print(fitMeasures(ksa_fit, c("cfi","tli","rmsea","srmr")))
  } else {
    cat("\nKSA-3 CFA skipped (model failed to converge).\n")
  }
}
```

    ## Omega_h for 1 factor is not meaningful, just omega_t
    ## Omega_h for 1 factor is not meaningful, just omega_t
    ## Omega_h for 1 factor is not meaningful, just omega_t
    ## Omega_h for 1 factor is not meaningful, just omega_t

    ##   cfi   tli rmsea  srmr 
    ## 0.964 0.947 0.068 0.064

## SDR-5 (SDR1--SDR5)

``` r
sdr_cols <- paste0("SDR", 1:5)
has_sdr  <- all(sdr_cols %in% names(dat))

if (!has_sdr) {
  cat("\n**SDR-5 section skipped:** SDR1–SDR5 not found.\n")
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
    print(fitMeasures(sdr_fit, c("cfi","tli","rmsea","srmr")))
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

    ##   cfi   tli rmsea  srmr 
    ## 0.768 0.536 0.184 0.103

# Step 9 --- Cross-construct correlations (HPT, KN, FR-LF, KSA-3, SDR-5)

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
  scales_list$FR_RD     <- rowMeans(fr_df[, paste0("RD",1:3)], na.rm = TRUE)
  scales_list$FR_NS     <- rowMeans(fr_df[, paste0("NS",1:3)], na.rm = TRUE)
  scales_list$FR_total  <- rowMeans(fr_df, na.rm = TRUE)
}

if (has_ksa) {
  ksa_df <- dat[keep, ksa_cols]
  scales_list$KSA_A     <- rowMeans(ksa_df[, paste0("A",1:3)], na.rm = TRUE)
  scales_list$KSA_U     <- rowMeans(ksa_df[, paste0("U",1:3)], na.rm = TRUE)
  scales_list$KSA_K     <- rowMeans(ksa_df[, paste0("K",1:3)], na.rm = TRUE)
  scales_list$KSA_total <- rowMeans(ksa_df, na.rm = TRUE)
}

if (has_sdr) {
  sdr_df <- dat[keep, sdr_cols]
  scales_list$SDR_total <- rowMeans(sdr_df, na.rm = TRUE)
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
0.002
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.035
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
-0.084
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
-0.035
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
-0.022
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
-0.057
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
0.002
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.020
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
0.064
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
0.065
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.155
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.119
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.145
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.043
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
-0.011
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.009
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
0.012
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.067
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.062
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
0.061
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
0.010
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.088
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.075
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.009
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
-0.109
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.105
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
0.009
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.069
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.050
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.005
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
0.424
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.840
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
0.409
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.356
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.488
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.069
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
0.002
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
0.064
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
0.061
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.109
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.424
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
0.848
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
0.349
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.231
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.400
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.241
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
-0.035
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.022
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
0.009
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
-0.105
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.840
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.848
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
0.431
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.436
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.346
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.517
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.184
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
0.431
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
0.336
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
-0.150
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
-0.084
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.057
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.155
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
0.010
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.009
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.409
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.349
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.436
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.336
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
0.399
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.735
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.164
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
-0.012
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
0.119
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.067
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.088
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.069
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.356
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.231
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.346
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
0.399
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
-0.013
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
-0.035
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.002
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.145
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.062
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.075
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.050
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.488
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.400
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.517
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
0.735
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
-0.140
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
-0.031
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.020
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.043
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
-0.009
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.005
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.069
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.241
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.184
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.150
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.164
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.013
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.140
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

    ## R version 4.4.2 (2024-10-31)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.3 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.12.0 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.12.0
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
    ##  [1] kableExtra_1.4.0   knitr_1.50         performance_0.15.1 lme4_1.1-38        Matrix_1.7-1       semTools_0.5-7    
    ##  [7] lavaan_0.6-20      psych_2.4.12       lubridate_1.9.4    forcats_1.0.0      stringr_1.5.1      dplyr_1.1.4       
    ## [13] purrr_1.1.0        readr_2.1.5        tidyr_1.3.1        tibble_3.2.1       ggplot2_4.0.1      tidyverse_2.0.0   
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.6         xfun_0.54            insight_1.4.2        lattice_0.22-5       tzdb_0.5.0          
    ##  [6] quadprog_1.5-8       vctrs_0.6.5          tools_4.4.2          Rdpack_2.6.4         generics_0.1.3      
    ## [11] stats4_4.4.2         parallel_4.4.2       sandwich_3.1-1       pkgconfig_2.0.3      lavaan.mi_0.1-0     
    ## [16] RColorBrewer_1.1-3   S7_0.2.1             lifecycle_1.0.4      GPArotation_2024.3-1 compiler_4.4.2      
    ## [21] farver_2.1.2         textshaping_0.4.1    tinytex_0.54         mnormt_2.1.1         codetools_0.2-20    
    ## [26] htmltools_0.5.8.1    yaml_2.3.10          pillar_1.10.0        nloptr_2.2.1         MASS_7.3-61         
    ## [31] reformulas_0.4.1     boot_1.3-31          multcomp_1.4-28      nlme_3.1-166         tidyselect_1.2.1    
    ## [36] digest_0.6.37        mvtnorm_1.3-2        stringi_1.8.4        labeling_0.4.3       splines_4.4.2       
    ## [41] fastmap_1.2.0        grid_4.4.2           cli_3.6.5            magrittr_2.0.3       survival_3.7-0      
    ## [46] pbivnorm_0.6.0       TH.data_1.1-4        withr_3.0.2          scales_1.4.0         estimability_1.5.1  
    ## [51] timechange_0.3.0     rmarkdown_2.29       emmeans_1.10.6       zoo_1.8-14           hms_1.1.3           
    ## [56] coda_0.19-4.1        evaluate_1.0.5       rbibutils_2.3        viridisLite_0.4.2    rlang_1.1.6         
    ## [61] Rcpp_1.0.13-1        xtable_1.8-4         glue_1.8.0           xml2_1.3.6           svglite_2.2.2       
    ## [66] rstudioapi_0.17.1    minqa_1.2.8          R6_2.6.1             systemfonts_1.3.1
