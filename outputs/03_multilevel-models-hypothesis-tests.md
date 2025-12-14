# 1. Purpose and hypotheses

This document runs the **main confirmatory analyses** for:

-   **H1.** Higher right‐authoritarian / pro-Nazi attitudes predict
    **higher HPT scores** on the original instrument (risk of
    ideological contamination). Predictors: FR-LF-mini (total or RD/NS
    facets) and KSA-3.
-   **H2.** The H1 effect **persists controlling** for prior knowledge
    (KN total) and social desirability (SDR-5).

Notes on constructs and scoring:

-   HPT subscores (POP, ROA, CONT) follow Hartmann & Hasselhorn /
    Huijgen et al. We treat **POP items as presentist** and therefore
    **reverse-score POP** so that **higher = more contextualised
    reasoning**. DVs used here are **HPT total (CTX6: POP_rev+CONT)**,
    **CONT**, and **POP_rev**.
-   FR-LF-mini uses **RD1--RD3** and **NS1--NS3**; we analyse **total**
    and **RD/NS facets**.
-   KSA-3 (9 items; aggression, submission, conventionalism) is included
    as a convergent authoritarian predictor. (Registered.)

# 2. Data, variables, and preprocessing

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

# Clean names to lower_snake so items are pop1/roa1/cont1 etc.
dat_raw <- normalised_responses |> janitor::clean_names()

# ------------------------------------------------------------------
# Build a UNIQUE class identifier = school_id x class label
# We support multiple plausible column names from the codebook.
# ------------------------------------------------------------------
# Detect school id column
school_var <- names(dat_raw)[names(dat_raw) %in% c("school_id","school")]
# Detect class label column (human-readable class label)
class_label_var <- names(dat_raw)[names(dat_raw) %in% c("classroom_label","class_label","class")]

if (length(school_var) == 0) stop("No school id column found (tried: school_id, school).")
if (length(class_label_var) == 0) stop("No class label column found (tried: classroom_label, class_label, class).")

school_var <- school_var[1]
class_label_var <- class_label_var[1]

# Force factors and create class_id
dat_raw <- dat_raw |>
  mutate(
    !!school_var := as.factor(.data[[school_var]]),
    !!class_label_var := as.factor(.data[[class_label_var]]),
    class_id = interaction(.data[[school_var]], .data[[class_label_var]], drop = TRUE)
  )

# ------------------
# HPT item vectors (lowercase after clean_names())
# ------------------
pop_items  <- paste0("pop", 1:3)
roa_items  <- paste0("roa", 1:3)
cont_items <- paste0("cont", 1:3)

# Reverse POP items so higher = more contextualised (1-4 scale assumed)
dat_raw <- dat_raw %>%
  mutate(across(all_of(pop_items), ~ 5 - as.numeric(.), .names = "{.col}_rev"))
```

``` r
# ---- Knowledge ----
kn_items <- paste0("kn", 1:6)

dat <- dat_raw |>
  mutate(
    kn_total = rowSums(across(all_of(kn_items)), na.rm = TRUE)
  )

# ---- HPT (use reversed POP) ----
dat <- dat |>
  mutate(
    hpt_pop_rev = rowMeans(across(paste0(pop_items, "_rev")), na.rm = TRUE),
    hpt_cont    = rowMeans(across(all_of(cont_items)),       na.rm = TRUE),
    hpt_roa     = rowMeans(across(all_of(roa_items)),        na.rm = TRUE),
    # Primary total (CTX6 = POP_rev + CONT); keep 9-item as sensitivity if needed
    hpt_total   = rowMeans(cbind(hpt_pop_rev, hpt_cont), na.rm = TRUE),
    hpt_total9  = rowMeans(cbind(hpt_pop_rev, hpt_cont, hpt_roa), na.rm = TRUE)
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
sdr_items <- paste0("sdr", 1:5)

dat <- dat |>
  mutate(
    sdr5_tot = rowMeans(across(all_of(sdr_items)), na.rm = TRUE)
  )
```

``` r
# Z-standardise continuous predictors (for comparability)
z <- function(x) as.numeric(scale(x))

# Ensure clustering vars present for every analysed row

dat <- dat |>
  mutate(
    z_hpt_total = z(hpt_total),
    z_hpt_cont  = z(hpt_cont),
    z_hpt_pop   = z(hpt_pop_rev),

    z_frlf_tot = z(frlf_tot),
    z_frlf_rd  = z(frlf_rd),
    z_frlf_ns  = z(frlf_ns),

    z_ksa3_tot = z(ksa3_tot),

    z_kn_total = z(kn_total),
    z_sdr5_tot = z(sdr5_tot)
  ) |>
  drop_na(all_of(c(school_var, "class_id")))
```

# 3. Model plan

We estimate **random‐intercept multilevel models** with **two clustering
terms** (students nested in classes within schools):

-   Base (FR-LF total):
    `DV ~ z_frlf_tot + z_ksa3_tot + z_kn_total + z_sdr5_tot + (1 | school_id) + (1 | class_id)`
-   Facet (RD/NS):
    `DV ~ z_frlf_rd + z_frlf_ns + z_ksa3_tot + z_kn_total + z_sdr5_tot + (1 | school_id) + (1 | class_id)`
-   Interaction (if preregistered):
    `DV ~ z_frlf_tot * z_kn_total + z_ksa3_tot + z_sdr5_tot + (1 | school_id) + (1 | class_id)`

DVs: `z_hpt_total` (CTX6), `z_hpt_cont`, `z_hpt_pop` (POP_rev).

``` r
dv_list <- c("z_hpt_total","z_hpt_cont","z_hpt_pop")

fits <- list()

for (dv in dv_list) {
  form_base  <- as.formula(
    glue("{dv} ~ z_frlf_tot + z_ksa3_tot + z_kn_total + z_sdr5_tot + (1 | {school_var}) + (1 | class_id)")
  )
  form_facet <- as.formula(
    glue("{dv} ~ z_frlf_rd + z_frlf_ns + z_ksa3_tot + z_kn_total + z_sdr5_tot + (1 | {school_var}) + (1 | class_id)")
  )
  form_int   <- as.formula(
    glue("{dv} ~ z_frlf_tot * z_kn_total + z_ksa3_tot + z_sdr5_tot + (1 | {school_var}) + (1 | class_id)")
  )

  m_base  <- lmer(form_base,  data = dat)
  m_facet <- lmer(form_facet, data = dat)
  m_int   <- lmer(form_int,   data = dat)

  fits[[dv]] <- list(base=m_base, facet=m_facet, int=m_int)
}
```

``` r
msummary(
  list(
    "HPT total (CTX6) — Base"  = fits$z_hpt_total$base,
    "HPT total (CTX6) — Facet" = fits$z_hpt_total$facet,
    "HPT total (CTX6) — Int."  = fits$z_hpt_total$int
  ),
  statistic = "({std.error})",
  gof_omit = "IC|Log|AIC|BIC",
  stars = TRUE
)
```

+-----------------+----------------+-----------------+----------------+
|                 | HPT total      | HPT total       | HPT total      |
|                 | (CTX6) ---     | (CTX6) ---      | (CTX6) ---     |
|                 | Base           | Facet           | Int.           |
+=================+================+=================+================+
| (Intercept)     | -0.027         | -0.031          | -0.029         |
+-----------------+----------------+-----------------+----------------+
|                 | (0.090)        | (0.088)         | (0.090)        |
+-----------------+----------------+-----------------+----------------+
| z_frlf_tot      | 0.036          |                 | 0.037          |
+-----------------+----------------+-----------------+----------------+
|                 | (0.075)        |                 | (0.075)        |
+-----------------+----------------+-----------------+----------------+
| z_ksa3_tot      | -0.081         | -0.079          | -0.080         |
+-----------------+----------------+-----------------+----------------+
|                 | (0.075)        | (0.075)         | (0.075)        |
+-----------------+----------------+-----------------+----------------+
| z_kn_total      | 0.320\*\*\*    | 0.322\*\*\*     | 0.318\*\*\*    |
+-----------------+----------------+-----------------+----------------+
|                 | (0.064)        | (0.064)         | (0.065)        |
+-----------------+----------------+-----------------+----------------+
| z_sdr5_tot      | -0.048         | -0.034          | -0.047         |
+-----------------+----------------+-----------------+----------------+
|                 | (0.065)        | (0.067)         | (0.065)        |
+-----------------+----------------+-----------------+----------------+
| z_frlf_rd       |                | -0.019          |                |
+-----------------+----------------+-----------------+----------------+
|                 |                | (0.075)         |                |
+-----------------+----------------+-----------------+----------------+
| z_frlf_ns       |                | 0.066           |                |
+-----------------+----------------+-----------------+----------------+
|                 |                | (0.074)         |                |
+-----------------+----------------+-----------------+----------------+
| z_frlf_tot ×    |                |                 | -0.019         |
| z_kn_total      |                |                 |                |
+-----------------+----------------+-----------------+----------------+
|                 |                |                 | (0.068)        |
+-----------------+----------------+-----------------+----------------+
| SD (Intercept   | 0.000          | 0.000           | 0.000          |
| class_id)       |                |                 |                |
+-----------------+----------------+-----------------+----------------+
| SD (Intercept   | 0.158          | 0.150           | 0.155          |
| school_id)      |                |                 |                |
+-----------------+----------------+-----------------+----------------+
| SD              | 0.943          | 0.943           | 0.945          |
| (Observations)  |                |                 |                |
+-----------------+----------------+-----------------+----------------+
| Num.Obs.        | 228            | 227             | 228            |
+-----------------+----------------+-----------------+----------------+
| R2 Marg.        | 0.105          | 0.106           | 0.105          |
+-----------------+----------------+-----------------+----------------+
| RMSE            | 0.93           | 0.93            | 0.93           |
+-----------------+----------------+-----------------+----------------+
| -   p \< 0.1,   |                |                 |                |
|     \* p \<     |                |                 |                |
|     0.05, \*\*  |                |                 |                |
|     p \< 0.01,  |                |                 |                |
|     \*\*\* p \< |                |                 |                |
|     0.001       |                |                 |                |
+-----------------+----------------+-----------------+----------------+

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

+--------------------------+-------------+--------------+-------------+
|                          | CONT ---    | CONT ---     | CONT ---    |
|                          | Base        | Facet        | Int.        |
+==========================+=============+==============+=============+
| (Intercept)              | -0.048      | -0.052       | -0.062      |
+--------------------------+-------------+--------------+-------------+
|                          | (0.099)     | (0.098)      | (0.098)     |
+--------------------------+-------------+--------------+-------------+
| z_frlf_tot               | 0.044       |              | 0.046       |
+--------------------------+-------------+--------------+-------------+
|                          | (0.078)     |              | (0.077)     |
+--------------------------+-------------+--------------+-------------+
| z_ksa3_tot               | 0.025       | 0.032        | 0.031       |
+--------------------------+-------------+--------------+-------------+
|                          | (0.078)     | (0.078)      | (0.077)     |
+--------------------------+-------------+--------------+-------------+
| z_kn_total               | 0.199\*\*   | 0.202\*\*    | 0.184\*\*   |
+--------------------------+-------------+--------------+-------------+
|                          | (0.066)     | (0.066)      | (0.066)     |
+--------------------------+-------------+--------------+-------------+
| z_sdr5_tot               | -0.015      | 0.005        | -0.007      |
+--------------------------+-------------+--------------+-------------+
|                          | (0.067)     | (0.069)      | (0.067)     |
+--------------------------+-------------+--------------+-------------+
| z_frlf_rd                |             | -0.045       |             |
+--------------------------+-------------+--------------+-------------+
|                          |             | (0.078)      |             |
+--------------------------+-------------+--------------+-------------+
| z_frlf_ns                |             | 0.099        |             |
+--------------------------+-------------+--------------+-------------+
|                          |             | (0.077)      |             |
+--------------------------+-------------+--------------+-------------+
| z_frlf_tot × z_kn_total  |             |              | -0.119+     |
+--------------------------+-------------+--------------+-------------+
|                          |             |              | (0.070)     |
+--------------------------+-------------+--------------+-------------+
| SD (Intercept class_id)  | 0.092       | 0.101        | 0.107       |
+--------------------------+-------------+--------------+-------------+
| SD (Intercept school_id) | 0.176       | 0.168        | 0.165       |
+--------------------------+-------------+--------------+-------------+
| SD (Observations)        | 0.972       | 0.970        | 0.967       |
+--------------------------+-------------+--------------+-------------+
| Num.Obs.                 | 228         | 227          | 228         |
+--------------------------+-------------+--------------+-------------+
| R2 Marg.                 | 0.041       | 0.046        | 0.053       |
+--------------------------+-------------+--------------+-------------+
| R2 Cond.                 | 0.079       | 0.084        | 0.090       |
+--------------------------+-------------+--------------+-------------+
| RMSE                     | 0.95        | 0.95         | 0.95        |
+--------------------------+-------------+--------------+-------------+
| -   p \< 0.1, \* p \<    |             |              |             |
|     0.05, \*\* p \<      |             |              |             |
|     0.01, \*\*\* p \<    |             |              |             |
|     0.001                |             |              |             |
+--------------------------+-------------+--------------+-------------+

``` r
msummary(
  list(
    "POP_rev — Base"  = fits$z_hpt_pop$base,
    "POP_rev — Facet" = fits$z_hpt_pop$facet,
    "POP_rev — Int."  = fits$z_hpt_pop$int
  ),
  statistic = "({std.error})",
  gof_omit = "IC|Log|AIC|BIC",
  stars = TRUE
)
```

+-----------------------+--------------+---------------+--------------+
|                       | POP_rev ---  | POP_rev ---   | POP_rev ---  |
|                       | Base         | Facet         | Int.         |
+=======================+==============+===============+==============+
| (Intercept)           | 0.004        | 0.000         | 0.016        |
+-----------------------+--------------+---------------+--------------+
|                       | (0.062)      | (0.062)       | (0.062)      |
+-----------------------+--------------+---------------+--------------+
| z_frlf_tot            | 0.003        |               | -0.000       |
+-----------------------+--------------+---------------+--------------+
|                       | (0.073)      |               | (0.073)      |
+-----------------------+--------------+---------------+--------------+
| z_ksa3_tot            | -0.176\*     | -0.179\*      | -0.181\*     |
+-----------------------+--------------+---------------+--------------+
|                       | (0.073)      | (0.074)       | (0.073)      |
+-----------------------+--------------+---------------+--------------+
| z_kn_total            | 0.340\*\*\*  | 0.338\*\*\*   | 0.352\*\*\*  |
+-----------------------+--------------+---------------+--------------+
|                       | (0.063)      | (0.063)       | (0.063)      |
+-----------------------+--------------+---------------+--------------+
| z_sdr5_tot            | -0.068       | -0.066        | -0.074       |
+-----------------------+--------------+---------------+--------------+
|                       | (0.064)      | (0.066)       | (0.064)      |
+-----------------------+--------------+---------------+--------------+
| z_frlf_rd             |              | 0.012         |              |
+-----------------------+--------------+---------------+--------------+
|                       |              | (0.075)       |              |
+-----------------------+--------------+---------------+--------------+
| z_frlf_ns             |              | -0.004        |              |
+-----------------------+--------------+---------------+--------------+
|                       |              | (0.074)       |              |
+-----------------------+--------------+---------------+--------------+
| z_frlf_tot ×          |              |               | 0.093        |
| z_kn_total            |              |               |              |
+-----------------------+--------------+---------------+--------------+
|                       |              |               | (0.067)      |
+-----------------------+--------------+---------------+--------------+
| SD (Intercept         | 0.000        | 0.000         | 0.000        |
| class_id)             |              |               |              |
+-----------------------+--------------+---------------+--------------+
| SD (Intercept         | 0.000        | 0.000         | 0.000        |
| school_id)            |              |               |              |
+-----------------------+--------------+---------------+--------------+
| SD (Observations)     | 0.935        | 0.937         | 0.933        |
+-----------------------+--------------+---------------+--------------+
| Num.Obs.              | 228          | 227           | 228          |
+-----------------------+--------------+---------------+--------------+
| R2 Marg.              | 0.140        | 0.138         | 0.146        |
+-----------------------+--------------+---------------+--------------+
| RMSE                  | 0.92         | 0.92          | 0.92         |
+-----------------------+--------------+---------------+--------------+
| -   p \< 0.1, \* p \< |              |               |              |
|     0.05, \*\* p \<   |              |               |              |
|     0.01, \*\*\* p \< |              |               |              |
|     0.001             |              |               |              |
+-----------------------+--------------+---------------+--------------+

``` r
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

collect_metrics <- function(m) {
  icc_val <- tryCatch({
    ic <- performance::icc(m)
    as.numeric(ic$ICC_adjusted %||% ic$ICC %||% ic$ICC_conditional %||% NA_real_)
  }, error = function(e) NA_real_)

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
    `HPT total (CTX6) — Base`  = collect_metrics(fits$z_hpt_total$base),
    `HPT total (CTX6) — Facet` = collect_metrics(fits$z_hpt_total$facet),
    `HPT total (CTX6) — Int.`  = collect_metrics(fits$z_hpt_total$int),
    `CONT — Base`               = collect_metrics(fits$z_hpt_cont$base),
    `CONT — Facet`              = collect_metrics(fits$z_hpt_cont$facet),
    `CONT — Int.`               = collect_metrics(fits$z_hpt_cont$int),
    `POP_rev — Base`            = collect_metrics(fits$z_hpt_pop$base),
    `POP_rev — Facet`           = collect_metrics(fits$z_hpt_pop$facet),
    `POP_rev — Int.`            = collect_metrics(fits$z_hpt_pop$int)
  ),
  .id = "Model"
)
```

    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Random effect variances not available. Returned R2 does not account for random effects.

``` r
knitr::kable(metrics, digits = 3, caption = "Model fit and clustering (ICC, $R^2$).")
```

  Model                            ICC    R2_m    R2_c
  ---------------------------- ------- ------- -------
  HPT total (CTX6) --- Base         NA   0.105      NA
  HPT total (CTX6) --- Facet        NA   0.106      NA
  HPT total (CTX6) --- Int.         NA   0.105      NA
  CONT --- Base                  0.040   0.041   0.079
  CONT --- Facet                 0.039   0.046   0.084
  CONT --- Int.                  0.040   0.053   0.090
  POP_rev --- Base                  NA   0.140      NA
  POP_rev --- Facet                 NA   0.138      NA
  POP_rev --- Int.                  NA   0.146      NA

  : Model fit and clustering (ICC, $R^2$).

``` r
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
  tidy_all(fits$z_hpt_total, "HPT total (CTX6)"),
  tidy_all(fits$z_hpt_cont,  "CONT"),
  tidy_all(fits$z_hpt_pop,   "POP_rev")
)

knitr::kable(
  tidy_tbl |> select(dv, spec, term, estimate, conf.low, conf.high, p.value),
  digits = 3,
  caption = "Fixed effects (standardized coefficients)."
)
```

  -----------------------------------------------------------------------------------------------
  dv            spec          term                      estimate   conf.low   conf.high   p.value
  ------------- ------------- ----------------------- ---------- ---------- ----------- ---------
  HPT total     Base          z_frlf_tot                   0.036     -0.111       0.184     0.628
  (CTX6)                                                                                

  HPT total     Base          z_ksa3_tot                  -0.081     -0.229       0.066     0.279
  (CTX6)                                                                                

  HPT total     Base          z_kn_total                   0.320      0.194       0.446     0.000
  (CTX6)                                                                                

  HPT total     Base          z_sdr5_tot                  -0.048     -0.176       0.080     0.463
  (CTX6)                                                                                

  HPT total     Facet         z_frlf_rd                   -0.019     -0.168       0.129     0.799
  (CTX6)                                                                                

  HPT total     Facet         z_frlf_ns                    0.066     -0.081       0.213     0.378
  (CTX6)                                                                                

  HPT total     Facet         z_ksa3_tot                  -0.079     -0.228       0.070     0.297
  (CTX6)                                                                                

  HPT total     Facet         z_kn_total                   0.322      0.196       0.448     0.000
  (CTX6)                                                                                

  HPT total     Facet         z_sdr5_tot                  -0.034     -0.165       0.098     0.613
  (CTX6)                                                                                

  HPT total     Interaction   z_frlf_tot                   0.037     -0.111       0.185     0.626
  (CTX6)                                                                                

  HPT total     Interaction   z_kn_total                   0.318      0.190       0.445     0.000
  (CTX6)                                                                                

  HPT total     Interaction   z_ksa3_tot                  -0.080     -0.228       0.068     0.286
  (CTX6)                                                                                

  HPT total     Interaction   z_sdr5_tot                  -0.047     -0.175       0.082     0.476
  (CTX6)                                                                                

  HPT total     Interaction   z_frlf_tot:z_kn_total       -0.019     -0.153       0.115     0.779
  (CTX6)                                                                                

  CONT          Base          z_frlf_tot                   0.044     -0.109       0.197     0.571

  CONT          Base          z_ksa3_tot                   0.025     -0.128       0.178     0.744

  CONT          Base          z_kn_total                   0.199      0.068       0.329     0.003

  CONT          Base          z_sdr5_tot                  -0.015     -0.148       0.117     0.818

  CONT          Facet         z_frlf_rd                   -0.045     -0.199       0.108     0.561

  CONT          Facet         z_frlf_ns                    0.099     -0.052       0.250     0.198

  CONT          Facet         z_ksa3_tot                   0.032     -0.122       0.186     0.681

  CONT          Facet         z_kn_total                   0.202      0.072       0.333     0.002

  CONT          Facet         z_sdr5_tot                   0.005     -0.130       0.141     0.937

  CONT          Interaction   z_frlf_tot                   0.046     -0.106       0.198     0.554

  CONT          Interaction   z_kn_total                   0.184      0.053       0.314     0.006

  CONT          Interaction   z_ksa3_tot                   0.031     -0.121       0.184     0.687

  CONT          Interaction   z_sdr5_tot                  -0.007     -0.139       0.125     0.914

  CONT          Interaction   z_frlf_tot:z_kn_total       -0.119     -0.256       0.019     0.091

  POP_rev       Base          z_frlf_tot                   0.003     -0.142       0.148     0.969

  POP_rev       Base          z_ksa3_tot                  -0.176     -0.320      -0.032     0.017

  POP_rev       Base          z_kn_total                   0.340      0.216       0.463     0.000

  POP_rev       Base          z_sdr5_tot                  -0.068     -0.193       0.058     0.289

  POP_rev       Facet         z_frlf_rd                    0.012     -0.135       0.159     0.869

  POP_rev       Facet         z_frlf_ns                   -0.004     -0.149       0.141     0.955

  POP_rev       Facet         z_ksa3_tot                  -0.179     -0.324      -0.034     0.016

  POP_rev       Facet         z_kn_total                   0.338      0.214       0.462     0.000

  POP_rev       Facet         z_sdr5_tot                  -0.066     -0.195       0.063     0.314

  POP_rev       Interaction   z_frlf_tot                   0.000     -0.145       0.144     0.998

  POP_rev       Interaction   z_kn_total                   0.352      0.228       0.477     0.000

  POP_rev       Interaction   z_ksa3_tot                  -0.181     -0.325      -0.038     0.014

  POP_rev       Interaction   z_sdr5_tot                  -0.074     -0.200       0.051     0.245

  POP_rev       Interaction   z_frlf_tot:z_kn_total        0.093     -0.039       0.225     0.166
  -----------------------------------------------------------------------------------------------

  : Fixed effects (standardized coefficients).

# 4. Results --- decision rules

Interpret **only the preregistered tests**:

-   **H1 supported** if the coefficient for **FR-LF** (either
    `z_frlf_tot` in Base/Int. or `z_frlf_rd`/`z_frlf_ns` in Facet) is
    **\> 0 and p \< .05** for **HPT total (CTX6)** and/or **CONT**.
-   **H2 supported** if the same holds **after** adding controls
    (**KN**, **SDR-5**) and **KSA-3** (already included), and --- if
    preregistered --- the **FR-LF × KN** interaction is **not
    necessary** for the main effect to persist (or, if hypothesised, is
    significant in the expected direction).

**Reading POP_rev.** Because POP is reversed, higher **POP_rev** means
**less presentism / more contextualised fit** on items that originally
cued presentist endorsements. Interpret alongside **CONT**.

# 5. Brief interpretation guide (for the write-up)

-   **Effect size:** Coefficients are **standardised** (β). Values
    around 0.10 are small, 0.20--0.30 moderate for individual-level
    predictors in multilevel models; report 95% CIs.
-   **Clustering:** Report **ICC** to show class-level variance.
-   **Model fit:** Report marginal and conditional R² and compare Base
    vs. Facet vs. Interaction.
-   **Substantive meaning:** A **positive FR-LF** effect on **HPT total
    / CONT** suggests that ideological affinity **elevates apparent
    contextualisation**, consistent with the contamination concern.
-   **Controls:** If FR-LF remains significant after **KN** and
    **SDR-5**, state that results are **not explained** by prior
    knowledge or social desirability (per H2).

# 6. Transparency and provenance

-   HPT structure and reversal logic follow Hartmann & Hasselhorn /
    Huijgen et al.
-   FR-LF-mini originates from the Leipzig FR-LF.
-   Analysis plan: random-intercept LMMs; DVs: HPT total (CTX6), CONT,
    POP_rev; predictors: FR-LF (total; RD/NS facets), KSA-3; controls:
    KN, SDR-5; clustering: school + class_id.

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
