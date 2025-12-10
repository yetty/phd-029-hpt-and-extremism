# 1. Purpose and hypotheses

This document runs the **main confirmatory analyses** for:

-   **H1.** Higher right‐authoritarian / pro-Nazi attitudes predict
    **higher HPT scores** on the original instrument (risk of
    ideological contamination). Predictors: FR-LF-mini (total or RD/NS
    facets) and KSA-3.
-   **H2.** The H1 effect **persists controlling** for prior knowledge
    (KN total) and social desirability (SDR-5).

Notes on constructs and scoring:

-   HPT instrument and subscores (POP, ROA, CONT) follow Hartmann &
    Hasselhorn / Huijgen et al. For this file we use **HPT total, CONT,
    POP** as DVs. (In our dataset, higher is coded as "fits Hannes's
    situation better" across items; thus **higher CONT** means more
    contextualized alignment; **higher POP** reflects stronger
    endorsement of the presentism-trigger statements after our
    recoding.)
-   FR-LF-mini uses **RD1--RD3** (right-dictatorship) and **NS1--NS3**
    (Nazi relativization) with robust reliability and a validated
    6-dimension parent scale. We analyze **total** and the **RD/NS
    facets**.
-   KSA-3 (9 items; aggression, submission, conventionalism) is included
    as a convergent authoritarian predictor. (Registered.)

# 2. Data, variables, and preprocessing

We expect either `normalised_responses_<DATE>.RData` or `.xlsx` in the
project root. Variable names match the codebook.

``` r
# Core
library(tidyverse)
library(readxl)
library(janitor)
# Models + tables
library(lme4)
library(lmerTest)
library(performance)
library(effectsize)
library(broom.mixed)
library(modelsummary)
library(glue)

library(kableExtra)
options(
    modelsummary_format = "latex",
    modelsummary_factory_latex = "kableExtra"
)
```

``` r
# Load the dataset created in 00_data-preparation
load("normalised_responses.RData")
stopifnot(exists("normalised_responses"))

dat_raw <- normalised_responses

dat_raw <- janitor::clean_names(dat_raw)
```

``` r
# ---- Knowledge ----
kn_items <- paste0("kn", 1:6)

dat <- dat_raw |>
  mutate(
    # across() returns a data frame of the selected columns; rowSums works on that
    kn_total = rowSums(across(all_of(kn_items)), na.rm = TRUE)
  )

# ---- HPT ----
pop_items   <- paste0("pop", 1:3)
cont_items  <- paste0("cont", 1:3)
roa_items   <- paste0("roa", 1:3)

dat <- dat |>
  mutate(
    hpt_pop   = rowMeans(across(all_of(pop_items)),  na.rm = TRUE),
    hpt_cont  = rowMeans(across(all_of(cont_items)), na.rm = TRUE),
    hpt_roa   = rowMeans(across(all_of(roa_items)),  na.rm = TRUE),
    hpt_total = rowMeans(cbind(hpt_pop, hpt_cont, hpt_roa), na.rm = TRUE)
  )

# ---- FR-LF mini ----
rd_items <- paste0("rd", 1:3)
ns_items <- paste0("ns", 1:3)

dat <- dat |>
  mutate(
    frlf_rd  = rowMeans(across(all_of(rd_items)), na.rm = TRUE),
    frlf_ns  = rowMeans(across(all_of(ns_items)), na.rm = TRUE),
    frlf_tot = rowMeans(cbind(frlf_rd, frlf_ns),  na.rm = TRUE)
  )

# ---- KSA-3 ----
a_items   <- paste0("a", 1:3)
u_items   <- paste0("u", 1:3)
k_items   <- paste0("k", 1:3)
ksa_items <- c(a_items, u_items, k_items)

dat <- dat |>
  mutate(
    ksa3_a   = rowMeans(across(all_of(a_items)),   na.rm = TRUE),
    ksa3_u   = rowMeans(across(all_of(u_items)),   na.rm = TRUE),
    ksa3_k   = rowMeans(across(all_of(k_items)),   na.rm = TRUE),
    ksa3_tot = rowMeans(across(all_of(ksa_items)), na.rm = TRUE)
  )

# ---- SDR-5 ----
# (SDR2–SDR4 already reversed upstream)
sdr_items <- paste0("sdr", 1:5)

dat <- dat |>
  mutate(
    sdr5_tot = rowMeans(across(all_of(sdr_items)), na.rm = TRUE)
  )

# ---- Clustering factor ----
cluster_var <- dplyr::case_when(
  "class_label" %in% names(dat) ~ "class_label",
  "class"       %in% names(dat) ~ "class",
  TRUE ~ NA_character_
)
if (is.na(cluster_var)) stop("No class cluster variable found (expected `class_label` or `class`).")
dat[[cluster_var]] <- as.factor(dat[[cluster_var]])
```

``` r
# Z-standardise continuous predictors (for comparability) and outcomes (optional)
z <- function(x) as.numeric(scale(x))

dat <- dat |>
  mutate(
    z_hpt_total = z(hpt_total),
    z_hpt_cont  = z(hpt_cont),
    z_hpt_pop   = z(hpt_pop),

    z_frlf_tot = z(frlf_tot),
    z_frlf_rd  = z(frlf_rd),
    z_frlf_ns  = z(frlf_ns),

    z_ksa3_tot = z(ksa3_tot),

    z_kn_total = z(kn_total),
    z_sdr5_tot = z(sdr5_tot)
  ) |>
  drop_na(!!sym(cluster_var))   # must have cluster id
```

# 3. Model plan

We estimate **random‐intercept multilevel models** (students nested in
classes). For each DV:

-   **Base (FR-LF total):**
    `DV ~ z_frlf_tot + z_ksa3_tot + z_kn_total + z_sdr5_tot + (1 | class)`
-   **Facet (RD/NS):**
    `DV ~ z_frlf_rd + z_frlf_ns + z_ksa3_tot + z_kn_total + z_sdr5_tot + (1 | class)`
-   **Interaction (if preregistered):**
    `DV ~ z_frlf_tot * z_kn_total + z_ksa3_tot + z_sdr5_tot + (1 | class)`

DVs: `z_hpt_total`, `z_hpt_cont`, `z_hpt_pop`.

Interpretation (fixed effects): positive $\beta$ means **higher
predictor → higher DV** (in SD units). For **H1--H2**, the key test is
**FR-LF coefficients** (or RD/NS) remaining positive and significant
**after controls**.

``` r
dv_list <- c("z_hpt_total","z_hpt_cont","z_hpt_pop")

fits <- list()

for (dv in dv_list) {

  # Build formulas INSIDE the loop (so {dv} exists when glue runs)
  form_base  <- as.formula(
    glue("{dv} ~ z_frlf_tot + z_ksa3_tot + z_kn_total + z_sdr5_tot + (1 | {cluster_var})")
  )

  form_facet <- as.formula(
    glue("{dv} ~ z_frlf_rd + z_frlf_ns + z_ksa3_tot + z_kn_total + z_sdr5_tot + (1 | {cluster_var})")
  )

  form_int   <- as.formula(
    glue("{dv} ~ z_frlf_tot * z_kn_total + z_ksa3_tot + z_sdr5_tot + (1 | {cluster_var})")
  )

  # Fit models
  m_base  <- lmer(form_base,  data = dat)
  m_facet <- lmer(form_facet, data = dat)
  m_int   <- lmer(form_int,   data = dat)

  fits[[dv]] <- list(
    base  = m_base,
    facet = m_facet,
    int   = m_int
  )
}
```

``` r
# Show standard errors in parentheses; alternatives include {t}, {p.value}, etc.
msummary(
  list(
    "HPT total — Base"  = fits$z_hpt_total$base,
    "HPT total — Facet" = fits$z_hpt_total$facet,
    "HPT total — Int."  = fits$z_hpt_total$int
  ),
  statistic = "({std.error})",
  gof_omit = "IC|Log|AIC|BIC",
  stars = TRUE
)
```

+----------------------+--------------+---------------+--------------+
|                      | HPT total    | HPT total --- | HPT total    |
|                      | --- Base     | Facet         | --- Int.     |
+======================+==============+===============+==============+
| (Intercept)          | 0.001        | -0.004        | -0.014       |
+----------------------+--------------+---------------+--------------+
|                      | (0.080)      | (0.083)       | (0.082)      |
+----------------------+--------------+---------------+--------------+
| z_frlf_tot           | -0.078       |               | -0.082       |
+----------------------+--------------+---------------+--------------+
|                      | (0.092)      |               | (0.092)      |
+----------------------+--------------+---------------+--------------+
| z_ksa3_tot           | 0.204\*      | 0.216\*       | 0.205\*      |
+----------------------+--------------+---------------+--------------+
|                      | (0.091)      | (0.092)       | (0.091)      |
+----------------------+--------------+---------------+--------------+
| z_kn_total           | 0.124        | 0.129         | 0.110        |
+----------------------+--------------+---------------+--------------+
|                      | (0.080)      | (0.080)       | (0.081)      |
+----------------------+--------------+---------------+--------------+
| z_sdr5_tot           | 0.103        | 0.123         | 0.109        |
+----------------------+--------------+---------------+--------------+
|                      | (0.080)      | (0.081)       | (0.080)      |
+----------------------+--------------+---------------+--------------+
| z_frlf_rd            |              | -0.136        |              |
+----------------------+--------------+---------------+--------------+
|                      |              | (0.093)       |              |
+----------------------+--------------+---------------+--------------+
| z_frlf_ns            |              | 0.042         |              |
+----------------------+--------------+---------------+--------------+
|                      |              | (0.090)       |              |
+----------------------+--------------+---------------+--------------+
| z_frlf_tot ×         |              |               | -0.075       |
| z_kn_total           |              |               |              |
+----------------------+--------------+---------------+--------------+
|                      |              |               | (0.087)      |
+----------------------+--------------+---------------+--------------+
| SD (Intercept        | 0.058        | 0.082         | 0.058        |
| class_label)         |              |               |              |
+----------------------+--------------+---------------+--------------+
| SD (Observations)    | 0.984        | 0.984         | 0.985        |
+----------------------+--------------+---------------+--------------+
| Num.Obs.             | 159          | 158           | 159          |
+----------------------+--------------+---------------+--------------+
| R2 Marg.             | 0.057        | 0.065         | 0.061        |
+----------------------+--------------+---------------+--------------+
| R2 Cond.             | 0.060        | 0.071         | 0.064        |
+----------------------+--------------+---------------+--------------+
| RMSE                 | 0.97         | 0.96          | 0.96         |
+----------------------+--------------+---------------+--------------+
| -   p \< 0.1, \* p   |              |               |              |
|     \< 0.05, \*\* p  |              |               |              |
|     \< 0.01, \*\*\*  |              |               |              |
|     p \< 0.001       |              |               |              |
+----------------------+--------------+---------------+--------------+

``` r
msummary(
  list(
    "CONT — Base"  = fits$z_hpt_cont$base,
    "CONT — Facet" = fits$z_hpt_cont$facet,
    "CONT — Int."  = fits$z_hpt_cont$int
  ),
  statistic = "({std.error})",
  gof_omit = "IC|Log|AIC|BIC",
  stars = TRUE
)
```

+---------------------------+------------+-------------+------------+
|                           | CONT ---   | CONT ---    | CONT ---   |
|                           | Base       | Facet       | Int.       |
+===========================+============+=============+============+
| (Intercept)               | -0.019     | -0.025      | -0.034     |
+---------------------------+------------+-------------+------------+
|                           | (0.096)    | (0.096)     | (0.099)    |
+---------------------------+------------+-------------+------------+
| z_frlf_tot                | -0.039     |             | -0.043     |
+---------------------------+------------+-------------+------------+
|                           | (0.091)    |             | (0.091)    |
+---------------------------+------------+-------------+------------+
| z_ksa3_tot                | 0.075      | 0.088       | 0.076      |
+---------------------------+------------+-------------+------------+
|                           | (0.091)    | (0.091)     | (0.091)    |
+---------------------------+------------+-------------+------------+
| z_kn_total                | 0.231\*\*  | 0.236\*\*   | 0.218\*\*  |
+---------------------------+------------+-------------+------------+
|                           | (0.079)    | (0.078)     | (0.080)    |
+---------------------------+------------+-------------+------------+
| z_sdr5_tot                | 0.060      | 0.086       | 0.065      |
+---------------------------+------------+-------------+------------+
|                           | (0.079)    | (0.080)     | (0.079)    |
+---------------------------+------------+-------------+------------+
| z_frlf_rd                 |            | -0.130      |            |
+---------------------------+------------+-------------+------------+
|                           |            | (0.091)     |            |
+---------------------------+------------+-------------+------------+
| z_frlf_ns                 |            | 0.086       |            |
+---------------------------+------------+-------------+------------+
|                           |            | (0.088)     |            |
+---------------------------+------------+-------------+------------+
| z_frlf_tot × z_kn_total   |            |             | -0.068     |
+---------------------------+------------+-------------+------------+
|                           |            |             | (0.086)    |
+---------------------------+------------+-------------+------------+
| SD (Intercept             | 0.187      | 0.188       | 0.193      |
| class_label)              |            |             |            |
+---------------------------+------------+-------------+------------+
| SD (Observations)         | 0.963      | 0.958       | 0.963      |
+---------------------------+------------+-------------+------------+
| Num.Obs.                  | 159        | 158         | 159        |
+---------------------------+------------+-------------+------------+
| R2 Marg.                  | 0.064      | 0.076       | 0.067      |
+---------------------------+------------+-------------+------------+
| R2 Cond.                  | 0.098      | 0.110       | 0.103      |
+---------------------------+------------+-------------+------------+
| RMSE                      | 0.94       | 0.93        | 0.93       |
+---------------------------+------------+-------------+------------+
| -   p \< 0.1, \* p \<     |            |             |            |
|     0.05, \*\* p \< 0.01, |            |             |            |
|     \*\*\* p \< 0.001     |            |             |            |
+---------------------------+------------+-------------+------------+

``` r
msummary(
  list(
    "POP — Base"  = fits$z_hpt_pop$base,
    "POP — Facet" = fits$z_hpt_pop$facet,
    "POP — Int."  = fits$z_hpt_pop$int
  ),
  statistic = "({std.error})",
  gof_omit = "IC|Log|AIC|BIC",
  stars = TRUE
)
```

+----------------------------+------------+-------------+------------+
|                            | POP ---    | POP ---     | POP ---    |
|                            | Base       | Facet       | Int.       |
+============================+============+=============+============+
| (Intercept)                | -0.005     | -0.000      | -0.014     |
+----------------------------+------------+-------------+------------+
|                            | (0.075)    | (0.075)     | (0.077)    |
+----------------------------+------------+-------------+------------+
| z_frlf_tot                 | -0.039     |             | -0.042     |
+----------------------------+------------+-------------+------------+
|                            | (0.088)    |             | (0.088)    |
+----------------------------+------------+-------------+------------+
| z_ksa3_tot                 | 0.173\*    | 0.176\*     | 0.174\*    |
+----------------------------+------------+-------------+------------+
|                            | (0.087)    | (0.088)     | (0.087)    |
+----------------------------+------------+-------------+------------+
| z_kn_total                 | -0         | -           | -0         |
|                            | .329\*\*\* | 0.328\*\*\* | .337\*\*\* |
+----------------------------+------------+-------------+------------+
|                            | (0.076)    | (0.077)     | (0.078)    |
+----------------------------+------------+-------------+------------+
| z_sdr5_tot                 | 0.096      | 0.092       | 0.100      |
+----------------------------+------------+-------------+------------+
|                            | (0.076)    | (0.078)     | (0.077)    |
+----------------------------+------------+-------------+------------+
| z_frlf_rd                  |            | -0.031      |            |
+----------------------------+------------+-------------+------------+
|                            |            | (0.089)     |            |
+----------------------------+------------+-------------+------------+
| z_frlf_ns                  |            | -0.022      |            |
+----------------------------+------------+-------------+------------+
|                            |            | (0.087)     |            |
+----------------------------+------------+-------------+------------+
| z_frlf_tot × z_kn_total    |            |             | -0.044     |
+----------------------------+------------+-------------+------------+
|                            |            |             | (0.084)    |
+----------------------------+------------+-------------+------------+
| SD (Intercept class_label) | 0.000      | 0.000       | 0.000      |
+----------------------------+------------+-------------+------------+
| SD (Observations)          | 0.942      | 0.946       | 0.944      |
+----------------------------+------------+-------------+------------+
| Num.Obs.                   | 159        | 158         | 159        |
+----------------------------+------------+-------------+------------+
| R2 Marg.                   | 0.132      | 0.131       | 0.133      |
+----------------------------+------------+-------------+------------+
| RMSE                       | 0.93       | 0.93        | 0.93       |
+----------------------------+------------+-------------+------------+
| -   p \< 0.1, \* p \<      |            |             |            |
|     0.05, \*\* p \< 0.01,  |            |             |            |
|     \*\*\* p \< 0.001      |            |             |            |
+----------------------------+------------+-------------+------------+

``` r
# Robust extractors so we ALWAYS return a single-row data.frame
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

collect_metrics <- function(m) {
  # ICC (allow for API differences)
  icc_val <- tryCatch({
    ic <- performance::icc(m)
    as.numeric(
      ic$ICC_adjusted %||% ic$ICC %||% ic$ICC_conditional %||% NA_real_
    )
  }, error = function(e) NA_real_)

  # R2 (prefer Nakagawa; fallbacks to older names)
  r2m <- r2c <- NA_real_
  try({
    r2o <- performance::r2_nakagawa(m)
    r2m <- as.numeric(r2o$R2_marginal %||% r2o$R2m %||% NA_real_)
    r2c <- as.numeric(r2o$R2_conditional %||% r2o$R2c %||% NA_real_)
  }, silent = TRUE)

  data.frame(ICC = icc_val, R2_m = r2m, R2_c = r2c, check.names = FALSE)
}

metrics <- dplyr::bind_rows(
  list(
    `HPT total — Base`  = collect_metrics(fits$z_hpt_total$base),
    `HPT total — Facet` = collect_metrics(fits$z_hpt_total$facet),
    `HPT total — Int.`  = collect_metrics(fits$z_hpt_total$int),
    `CONT — Base`       = collect_metrics(fits$z_hpt_cont$base),
    `CONT — Facet`      = collect_metrics(fits$z_hpt_cont$facet),
    `CONT — Int.`       = collect_metrics(fits$z_hpt_cont$int),
    `POP — Base`        = collect_metrics(fits$z_hpt_pop$base),
    `POP — Facet`       = collect_metrics(fits$z_hpt_pop$facet),
    `POP — Int.`        = collect_metrics(fits$z_hpt_pop$int)
  ),
  .id = "Model"
)
```

    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Random effect variances not available. Returned R2 does not account for random effects.

``` r
knitr::kable(metrics, digits = 3, caption = "Model fit and clustering (ICC, $R^2$).")
```

  Model                     ICC    R2_m    R2_c
  --------------------- ------- ------- -------
  HPT total --- Base      0.004   0.057   0.060
  HPT total --- Facet     0.007   0.065   0.071
  HPT total --- Int.      0.004   0.061   0.064
  CONT --- Base           0.036   0.064   0.098
  CONT --- Facet          0.037   0.076   0.110
  CONT --- Int.           0.039   0.067   0.103
  POP --- Base               NA   0.132      NA
  POP --- Facet              NA   0.131      NA
  POP --- Int.               NA   0.133      NA

  : Model fit and clustering (ICC, $R^2$).

``` r
# Extract tidy tables for inference and interpretation sections
tidy_all <- function(lst, label) {
  bind_rows(
    broom.mixed::tidy(lst$base,  effects="fixed", conf.int=TRUE) |> mutate(spec="Base"),
    broom.mixed::tidy(lst$facet, effects="fixed", conf.int=TRUE) |> mutate(spec="Facet"),
    broom.mixed::tidy(lst$int,   effects="fixed", conf.int=TRUE) |> mutate(spec="Interaction")
  ) |>
    filter(term != "(Intercept)") |>
    mutate(dv = label)
}

tidy_tbl <- bind_rows(
  tidy_all(fits$z_hpt_total, "HPT total"),
  tidy_all(fits$z_hpt_cont,  "CONT"),
  tidy_all(fits$z_hpt_pop,   "POP")
)

knitr::kable(
  tidy_tbl |> select(dv, spec, term, estimate, conf.low, conf.high, p.value),
  digits = 3,
  caption = "Fixed effects (standardized coefficients)."
)
```

  ------------------------------------------------------------------------------------------
  dv       spec          term                      estimate   conf.low   conf.high   p.value
  -------- ------------- ----------------------- ---------- ---------- ----------- ---------
  HPT      Base          z_frlf_tot                  -0.078     -0.260       0.103     0.396
  total                                                                            

  HPT      Base          z_ksa3_tot                   0.204      0.024       0.383     0.027
  total                                                                            

  HPT      Base          z_kn_total                   0.124     -0.033       0.281     0.121
  total                                                                            

  HPT      Base          z_sdr5_tot                   0.103     -0.054       0.261     0.197
  total                                                                            

  HPT      Facet         z_frlf_rd                   -0.136     -0.320       0.048     0.145
  total                                                                            

  HPT      Facet         z_frlf_ns                    0.042     -0.137       0.220     0.645
  total                                                                            

  HPT      Facet         z_ksa3_tot                   0.216      0.034       0.397     0.020
  total                                                                            

  HPT      Facet         z_kn_total                   0.129     -0.029       0.287     0.109
  total                                                                            

  HPT      Facet         z_sdr5_tot                   0.123     -0.038       0.283     0.133
  total                                                                            

  HPT      Interaction   z_frlf_tot                  -0.082     -0.264       0.100     0.374
  total                                                                            

  HPT      Interaction   z_kn_total                   0.110     -0.050       0.271     0.176
  total                                                                            

  HPT      Interaction   z_ksa3_tot                   0.205      0.025       0.385     0.026
  total                                                                            

  HPT      Interaction   z_sdr5_tot                   0.109     -0.049       0.268     0.174
  total                                                                            

  HPT      Interaction   z_frlf_tot:z_kn_total       -0.075     -0.248       0.097     0.389
  total                                                                            

  CONT     Base          z_frlf_tot                  -0.039     -0.219       0.141     0.669

  CONT     Base          z_ksa3_tot                   0.075     -0.104       0.255     0.408

  CONT     Base          z_kn_total                   0.231      0.076       0.386     0.004

  CONT     Base          z_sdr5_tot                   0.060     -0.096       0.215     0.451

  CONT     Facet         z_frlf_rd                   -0.130     -0.310       0.050     0.156

  CONT     Facet         z_frlf_ns                    0.086     -0.088       0.260     0.331

  CONT     Facet         z_ksa3_tot                   0.088     -0.092       0.268     0.337

  CONT     Facet         z_kn_total                   0.236      0.082       0.391     0.003

  CONT     Facet         z_sdr5_tot                   0.086     -0.072       0.244     0.283

  CONT     Interaction   z_frlf_tot                  -0.043     -0.223       0.137     0.639

  CONT     Interaction   z_kn_total                   0.218      0.060       0.377     0.007

  CONT     Interaction   z_ksa3_tot                   0.076     -0.104       0.256     0.404

  CONT     Interaction   z_sdr5_tot                   0.065     -0.091       0.222     0.413

  CONT     Interaction   z_frlf_tot:z_kn_total       -0.068     -0.238       0.102     0.432

  POP      Base          z_frlf_tot                  -0.039     -0.213       0.134     0.654

  POP      Base          z_ksa3_tot                   0.173      0.002       0.344     0.048

  POP      Base          z_kn_total                  -0.329     -0.479      -0.179     0.000

  POP      Base          z_sdr5_tot                   0.096     -0.054       0.247     0.208

  POP      Facet         z_frlf_rd                   -0.031     -0.207       0.145     0.731

  POP      Facet         z_frlf_ns                   -0.022     -0.193       0.149     0.802

  POP      Facet         z_ksa3_tot                   0.176      0.003       0.349     0.046

  POP      Facet         z_kn_total                  -0.328     -0.479      -0.176     0.000

  POP      Facet         z_sdr5_tot                   0.092     -0.061       0.246     0.237

  POP      Interaction   z_frlf_tot                  -0.042     -0.216       0.133     0.638

  POP      Interaction   z_kn_total                  -0.337     -0.491      -0.184     0.000

  POP      Interaction   z_ksa3_tot                   0.174      0.002       0.345     0.047

  POP      Interaction   z_sdr5_tot                   0.100     -0.051       0.251     0.194

  POP      Interaction   z_frlf_tot:z_kn_total       -0.044     -0.209       0.121     0.596
  ------------------------------------------------------------------------------------------

  : Fixed effects (standardized coefficients).

# 4. Results --- decision rules

Interpret **only the preregistered tests**:

-   **H1 supported** if the coefficient for **FR-LF** (either
    `z_frlf_tot` in Base/Int. or `z_frlf_rd`/`z_frlf_ns` in Facet) is
    **$>$ 0 and $p < .05$** for **HPT total** and/or **CONT**.
-   **H2 supported** if the same holds **after** adding controls
    (**KN**, **SDR-5**) and **KSA-3** (already included), and --- if
    preregistered --- the **FR-LF × KN** interaction is **not
    necessary** for the main effect to persist (or, if hypothesized, is
    significant in the expected direction).

**Reading POP.** Given our recoding (1--4 "fit"), higher **POP** here
reflects stronger agreement that the presentism-trigger statements "fit"
Hannes. In the original instrument, POP and CONT came out as a single
factor vs. ROA in CFA, and item wording can trip respondents; interpret
POP cautiously and triangulate with CONT.

# 5. Brief interpretation guide (for the write-up)

-   **Effect size:** Coefficients are **standardized** ($\beta$). Values
    around 0.10 are small, 0.20--0.30 moderate for individual-level
    predictors in multilevel models; report 95% CIs.
-   **Clustering:** Report **ICC** to show class-level variance.
-   **Model fit:** Report marginal and conditional $R^2$ and compare
    Base vs. Facet vs. Interaction.
-   **Substantive meaning:** A **positive FR-LF** effect on **HPT total
    / CONT** suggests that ideological affinity **elevates apparent
    contextualization**, consistent with the contamination concern. Cite
    the HPT literature and the FR-LF validation when interpreting.
-   **Controls:** If FR-LF remains significant after **KN** and
    **SDR-5**, state that results are **not explained** by prior
    knowledge or social desirability (per H2).

# 6. Transparency and provenance

-   **Instrument provenance.** HPT instrument and subscale logic follow
    Hartmann & Hasselhorn / Huijgen et al. (contextualization
    vs. presentism; known POP/CONT factor behavior; potential item-level
    ambiguities).
-   **FR-LF-mini.** Items RD1--3 and NS1--3 originate from the Leipzig
    FR-LF, which shows a stable 6-factor structure and excellent
    internal consistency in representative samples.
-   **Analysis plan.** This file implements the Stage 1 snapshot plan
    (multilevel regressions; DVs: HPT total, CONT, POP; predictors:
    FR-LF, KSA-3; controls: KN, SDR-5; class clustering).
-   **Variable names and coding** are taken from the project codebook to
    ensure reproducibility.

# 7. Session info

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
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=cs_CZ.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=cs_CZ.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=cs_CZ.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=cs_CZ.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## time zone: Europe/Prague
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] kableExtra_1.4.0    glue_1.8.0          modelsummary_2.5.0 
    ##  [4] broom.mixed_0.2.9.6 effectsize_1.0.1    performance_0.15.1 
    ##  [7] lmerTest_3.1-3      lme4_1.1-38         Matrix_1.7-1       
    ## [10] janitor_2.2.1       readxl_1.4.3        lubridate_1.9.4    
    ## [13] forcats_1.0.0       stringr_1.5.1       dplyr_1.1.4        
    ## [16] purrr_1.1.0         readr_2.1.5         tidyr_1.3.1        
    ## [19] tibble_3.2.1        ggplot2_4.0.1       tidyverse_2.0.0    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1    viridisLite_0.4.2   farver_2.1.2       
    ##  [4] S7_0.2.1            fastmap_1.2.0       TH.data_1.1-4      
    ##  [7] bayestestR_0.17.0   digest_0.6.37       estimability_1.5.1 
    ## [10] timechange_0.3.0    lifecycle_1.0.4     survival_3.7-0     
    ## [13] magrittr_2.0.3      compiler_4.4.2      rlang_1.1.6        
    ## [16] tools_4.4.2         yaml_2.3.10         data.table_1.17.8  
    ## [19] knitr_1.50          xml2_1.3.6          RColorBrewer_1.1-3 
    ## [22] multcomp_1.4-28     tinytable_0.15.1    withr_3.0.2        
    ## [25] numDeriv_2016.8-1.1 grid_4.4.2          datawizard_1.2.0   
    ## [28] xtable_1.8-4        future_1.68.0       globals_0.18.0     
    ## [31] emmeans_1.10.6      scales_1.4.0        MASS_7.3-61        
    ## [34] tinytex_0.54        insight_1.4.2       cli_3.6.5          
    ## [37] mvtnorm_1.3-2       rmarkdown_2.29      reformulas_0.4.1   
    ## [40] generics_0.1.3      future.apply_1.20.0 rstudioapi_0.17.1  
    ## [43] tzdb_0.5.0          parameters_0.28.1   minqa_1.2.8        
    ## [46] splines_4.4.2       parallel_4.4.2      cellranger_1.1.0   
    ## [49] vctrs_0.6.5         boot_1.3-31         sandwich_3.1-1     
    ## [52] hms_1.1.3           listenv_0.10.0      systemfonts_1.3.1  
    ## [55] parallelly_1.45.1   nloptr_2.2.1        codetools_0.2-20   
    ## [58] stringi_1.8.4       gtable_0.3.6        tables_0.9.31      
    ## [61] pillar_1.10.0       furrr_0.3.1         htmltools_0.5.8.1  
    ## [64] R6_2.6.1            textshaping_0.4.1   Rdpack_2.6.4       
    ## [67] evaluate_1.0.5      lattice_0.22-5      rbibutils_2.3      
    ## [70] backports_1.5.0     broom_1.0.7         snakecase_0.11.1   
    ## [73] Rcpp_1.0.13-1       checkmate_2.3.3     svglite_2.2.2      
    ## [76] coda_0.19-4.1       nlme_3.1-166        xfun_0.54          
    ## [79] zoo_1.8-14          pkgconfig_2.0.3

------------------------------------------------------------------------

### Minimal reporting template (paste into manuscript)

-   **Model:** Random-intercept LMM (classes).
-   **DV:** HPT total (z); robustness for CONT (z) and POP (z).
-   **Predictors:** FR-LF total (z) or RD/NS facets (z); KSA-3 total
    (z); controls KN (z), SDR-5 (z).
-   **Key result:** $\beta_{\text{FR-LF}}=\ldots$, 95% CI
    $[\ldots,\ldots]$, $p=\ldots$; ICC = $\ldots$; $R^2_m=\ldots$,
    $R^2_c=\ldots$.
-   **Decision:** H1 ... / H2 ... (per criteria above).

*References for context*: Huijgen et al. on HPT structure and presentism
risks; FR-LF validation and dimensionality; Stage 1 snapshot for
analytic plan; project codebook for variables.
