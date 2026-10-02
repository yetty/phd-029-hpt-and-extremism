# 1. Purpose and hypotheses

This document presents multilevel association models relevant to the
registered hypotheses H1 and H2. It does not implement the exact
registered one-tailed, covariate-adjusted specifications and therefore
is not a confirmatory test of those hypotheses; Table S5 documents the
deviations.

-   **H1.** Higher right‐authoritarian / pro-Nazi attitudes predict
    **higher HPT scores** on the original instrument (risk of
    ideological contamination). Predictors: FR-LF-mini (total or RD/NS
    facets) and KSA-3.
-   **H2.** The H1 effect **persists controlling** for prior knowledge
    (KN total) and social desirability (SDR-5).

Notes on constructs and scoring:

-   HPT subscores (POP, ROA, CONT) follow Hartmann & Hasselhorn /
    Huijgen et al. We treat **POP items as presentist** and therefore
    **reverse-score POP** so that **higher = more contextualised
    reasoning**. DVs used here are **HPT total (CTX6: POP_rev+CONT)**,
    **CONT**, and **POP_rev**.
-   FR-LF-mini uses **RD1-RD3** and **NS1-NS3**; we analyse **total**
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
source("submissions/pci_psychology/scoring_helpers.R")
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

dat <- dat_raw %>%
  mutate(
    kn_total = rowSums(across(all_of(kn_items)), na.rm = TRUE)
  )

# ---- HPT (use reversed POP) ----
dat <- dat %>%
  mutate(
    hpt_pop_rev = scale_mean(., paste0(pop_items, "_rev"), min_answered = 2),
    hpt_cont    = scale_mean(., cont_items, min_answered = 2),
    hpt_roa     = scale_mean(., roa_items, min_answered = 2),
    # Primary total (CTX6 = POP_rev + CONT); keep 9-item as sensitivity if needed
    hpt_total   = rowMeans(cbind(hpt_pop_rev, hpt_cont), na.rm = FALSE),
    hpt_total9  = rowMeans(cbind(hpt_pop_rev, hpt_cont, hpt_roa),
                           na.rm = FALSE)
  )

# ---- FR-LF mini ----
rd_items <- paste0("rd", 1:3)
ns_items <- paste0("ns", 1:3)

dat <- dat %>%
  mutate(
    frlf_rd  = scale_mean(., rd_items, min_answered = 2),
    frlf_ns  = scale_mean(., ns_items, min_answered = 2),
    frlf_tot = scale_mean(., c(rd_items, ns_items), min_answered = 4)
  )

# ---- KSA-3 ----
a_items   <- paste0("a", 1:3)
u_items   <- paste0("u", 1:3)
k_items   <- paste0("k", 1:3)
ksa_items <- c(a_items, u_items, k_items)

dat <- dat %>%
  mutate(
    ksa3_a   = scale_mean(., a_items, min_answered = 2),
    ksa3_u   = scale_mean(., u_items, min_answered = 2),
    ksa3_k   = scale_mean(., k_items, min_answered = 2),
    ksa3_tot = scale_mean(., ksa_items, min_answered = 7)
  )

# ---- SDR-5 ----
sdr_items <- paste0("sdr", 1:5)

dat <- dat %>%
  mutate(
    sdr5_tot = scale_mean(., sdr_items, min_answered = 4)
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
    "HPT total (CTX6) -- Base"  = fits$z_hpt_total$base,
    "HPT total (CTX6) -- Facet" = fits$z_hpt_total$facet,
    "HPT total (CTX6) -- Int."  = fits$z_hpt_total$int
  ),
  statistic = "({std.error})",
  gof_omit = "IC|Log|AIC|BIC",
  stars = TRUE
)
```

+----------------+----------------+-----------------+----------------+
|                | HPT total      | HPT total       | HPT total      |
|                | (CTX6) -- Base | (CTX6) -- Facet | (CTX6) -- Int. |
+================+================+=================+================+
| (Intercept)    | -0.019         | -0.021          | -0.020         |
+----------------+----------------+-----------------+----------------+
|                | (0.068)        | (0.069)         | (0.067)        |
+----------------+----------------+-----------------+----------------+
| z_frlf_tot     | 0.016          |                 | 0.017          |
+----------------+----------------+-----------------+----------------+
|                | (0.068)        |                 | (0.068)        |
+----------------+----------------+-----------------+----------------+
| z_ksa3_tot     | -0.061         | -0.056          | -0.061         |
+----------------+----------------+-----------------+----------------+
|                | (0.067)        | (0.067)         | (0.067)        |
+----------------+----------------+-----------------+----------------+
| z_kn_total     | 0.340\*\*\*    | 0.341\*\*\*     | 0.337\*\*\*    |
+----------------+----------------+-----------------+----------------+
|                | (0.058)        | (0.058)         | (0.058)        |
+----------------+----------------+-----------------+----------------+
| z_sdr5_tot     | -0.040         | -0.029          | -0.039         |
+----------------+----------------+-----------------+----------------+
|                | (0.058)        | (0.059)         | (0.058)        |
+----------------+----------------+-----------------+----------------+
| z_frlf_rd      |                | -0.045          |                |
+----------------+----------------+-----------------+----------------+
|                |                | (0.067)         |                |
+----------------+----------------+-----------------+----------------+
| z_frlf_ns      |                | 0.065           |                |
+----------------+----------------+-----------------+----------------+
|                |                | (0.066)         |                |
+----------------+----------------+-----------------+----------------+
| z_frlf_tot ×   |                |                 | -0.019         |
| z_kn_total     |                |                 |                |
+----------------+----------------+-----------------+----------------+
|                |                |                 | (0.060)        |
+----------------+----------------+-----------------+----------------+
| SD (Intercept  | 0.000          | 0.000           | 0.000          |
| class_id)      |                |                 |                |
+----------------+----------------+-----------------+----------------+
| SD (Intercept  | 0.096          | 0.100           | 0.092          |
| school_id)     |                |                 |                |
+----------------+----------------+-----------------+----------------+
| SD             | 0.944          | 0.944           | 0.946          |
| (Observations) |                |                 |                |
+----------------+----------------+-----------------+----------------+
| Num.Obs.       | 282            | 282             | 282            |
+----------------+----------------+-----------------+----------------+
| R2 Marg.       | 0.115          | 0.118           | 0.115          |
+----------------+----------------+-----------------+----------------+
| RMSE           | 0.93           | 0.93            | 0.93           |
+----------------+----------------+-----------------+----------------+
| -   p \< 0.1,  |                |                 |                |
|     \* p \<    |                |                 |                |
|     0.05, \*\* |                |                 |                |
|     p \< 0.01, |                |                 |                |
|     \*\*\* p   |                |                 |                |
|     \< 0.001   |                |                 |                |
+----------------+----------------+-----------------+----------------+

``` r
msummary(
  list(
    "CONT -- Base"  = fits$z_hpt_cont$base,
    "CONT -- Facet" = fits$z_hpt_cont$facet,
    "CONT -- Int."  = fits$z_hpt_cont$int
  ),
  statistic = "({std.error})",
  gof_omit = "IC|Log|AIC|BIC",
  stars = TRUE
)
```

+------------------------+-------------+--------------+-------------+
|                        | CONT --     | CONT --      | CONT --     |
|                        | Base        | Facet        | Int.        |
+========================+=============+==============+=============+
| (Intercept)            | -0.023      | -0.024       | -0.030      |
+------------------------+-------------+--------------+-------------+
|                        | (0.073)     | (0.073)      | (0.071)     |
+------------------------+-------------+--------------+-------------+
| z_frlf_tot             | 0.006       |              | 0.011       |
+------------------------+-------------+--------------+-------------+
|                        | (0.069)     |              | (0.069)     |
+------------------------+-------------+--------------+-------------+
| z_ksa3_tot             | 0.054       | 0.061        | 0.057       |
+------------------------+-------------+--------------+-------------+
|                        | (0.069)     | (0.069)      | (0.069)     |
+------------------------+-------------+--------------+-------------+
| z_kn_total             | 0.215\*\*\* | 0.218\*\*\*  | 0.201\*\*\* |
+------------------------+-------------+--------------+-------------+
|                        | (0.059)     | (0.059)      | (0.060)     |
+------------------------+-------------+--------------+-------------+
| z_sdr5_tot             | 0.011       | 0.025        | 0.015       |
+------------------------+-------------+--------------+-------------+
|                        | (0.059)     | (0.060)      | (0.059)     |
+------------------------+-------------+--------------+-------------+
| z_frlf_rd              |             | -0.075       |             |
+------------------------+-------------+--------------+-------------+
|                        |             | (0.068)      |             |
+------------------------+-------------+--------------+-------------+
| z_frlf_ns              |             | 0.082        |             |
+------------------------+-------------+--------------+-------------+
|                        |             | (0.067)      |             |
+------------------------+-------------+--------------+-------------+
| z_frlf_tot ×           |             |              | -0.097      |
| z_kn_total             |             |              |             |
+------------------------+-------------+--------------+-------------+
|                        |             |              | (0.061)     |
+------------------------+-------------+--------------+-------------+
| SD (Intercept          | 0.114       | 0.123        | 0.121       |
| class_id)              |             |              |             |
+------------------------+-------------+--------------+-------------+
| SD (Intercept          | 0.088       | 0.086        | 0.075       |
| school_id)             |             |              |             |
+------------------------+-------------+--------------+-------------+
| SD (Observations)      | 0.966       | 0.964        | 0.964       |
+------------------------+-------------+--------------+-------------+
| Num.Obs.               | 282         | 282          | 282         |
+------------------------+-------------+--------------+-------------+
| R2 Marg.               | 0.050       | 0.057        | 0.059       |
+------------------------+-------------+--------------+-------------+
| R2 Cond.               | 0.071       | 0.079        | 0.079       |
+------------------------+-------------+--------------+-------------+
| RMSE                   | 0.95        | 0.95         | 0.95        |
+------------------------+-------------+--------------+-------------+
| -   p \< 0.1, \* p \<  |             |              |             |
|     0.05, \*\* p \<    |             |              |             |
|     0.01, \*\*\* p \<  |             |              |             |
|     0.001              |             |              |             |
+------------------------+-------------+--------------+-------------+

``` r
msummary(
  list(
    "POP_rev -- Base"  = fits$z_hpt_pop$base,
    "POP_rev -- Facet" = fits$z_hpt_pop$facet,
    "POP_rev -- Int."  = fits$z_hpt_pop$int
  ),
  statistic = "({std.error})",
  gof_omit = "IC|Log|AIC|BIC",
  stars = TRUE
)
```

+----------------------+--------------+---------------+--------------+
|                      | POP_rev --   | POP_rev --    | POP_rev --   |
|                      | Base         | Facet         | Int.         |
+======================+==============+===============+==============+
| (Intercept)          | -0.007       | -0.007        | 0.001        |
+----------------------+--------------+---------------+--------------+
|                      | (0.056)      | (0.056)       | (0.056)      |
+----------------------+--------------+---------------+--------------+
| z_frlf_tot           | 0.011        |               | 0.006        |
+----------------------+--------------+---------------+--------------+
|                      | (0.067)      |               | (0.067)      |
+----------------------+--------------+---------------+--------------+
| z_ksa3_tot           | -0.166\*     | -0.166\*      | -0.168\*     |
+----------------------+--------------+---------------+--------------+
|                      | (0.066)      | (0.067)       | (0.066)      |
+----------------------+--------------+---------------+--------------+
| z_kn_total           | 0.335\*\*\*  | 0.335\*\*\*   | 0.346\*\*\*  |
+----------------------+--------------+---------------+--------------+
|                      | (0.057)      | (0.057)       | (0.057)      |
+----------------------+--------------+---------------+--------------+
| z_sdr5_tot           | -0.080       | -0.079        | -0.084       |
+----------------------+--------------+---------------+--------------+
|                      | (0.057)      | (0.058)       | (0.057)      |
+----------------------+--------------+---------------+--------------+
| z_frlf_rd            |              | 0.002         |              |
+----------------------+--------------+---------------+--------------+
|                      |              | (0.066)       |              |
+----------------------+--------------+---------------+--------------+
| z_frlf_ns            |              | 0.012         |              |
+----------------------+--------------+---------------+--------------+
|                      |              | (0.066)       |              |
+----------------------+--------------+---------------+--------------+
| z_frlf_tot ×         |              |               | 0.075        |
| z_kn_total           |              |               |              |
+----------------------+--------------+---------------+--------------+
|                      |              |               | (0.059)      |
+----------------------+--------------+---------------+--------------+
| SD (Intercept        | 0.000        | 0.000         | 0.000        |
| class_id)            |              |               |              |
+----------------------+--------------+---------------+--------------+
| SD (Intercept        | 0.000        | 0.000         | 0.000        |
| school_id)           |              |               |              |
+----------------------+--------------+---------------+--------------+
| SD (Observations)    | 0.940        | 0.942         | 0.939        |
+----------------------+--------------+---------------+--------------+
| Num.Obs.             | 282          | 282           | 282          |
+----------------------+--------------+---------------+--------------+
| R2 Marg.             | 0.131        | 0.131         | 0.135        |
+----------------------+--------------+---------------+--------------+
| RMSE                 | 0.93         | 0.93          | 0.93         |
+----------------------+--------------+---------------+--------------+
| -   p \< 0.1, \* p   |              |               |              |
|     \< 0.05, \*\* p  |              |               |              |
|     \< 0.01, \*\*\*  |              |               |              |
|     p \< 0.001       |              |               |              |
+----------------------+--------------+---------------+--------------+

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
    `HPT total (CTX6) -- Base`  = collect_metrics(fits$z_hpt_total$base),
    `HPT total (CTX6) -- Facet` = collect_metrics(fits$z_hpt_total$facet),
    `HPT total (CTX6) -- Int.`  = collect_metrics(fits$z_hpt_total$int),
    `CONT -- Base`               = collect_metrics(fits$z_hpt_cont$base),
    `CONT -- Facet`              = collect_metrics(fits$z_hpt_cont$facet),
    `CONT -- Int.`               = collect_metrics(fits$z_hpt_cont$int),
    `POP_rev -- Base`            = collect_metrics(fits$z_hpt_pop$base),
    `POP_rev -- Facet`           = collect_metrics(fits$z_hpt_pop$facet),
    `POP_rev -- Int.`            = collect_metrics(fits$z_hpt_pop$int)
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

  Model                           ICC    R2_m    R2_c
  --------------------------- ------- ------- -------
  HPT total (CTX6) -- Base         NA   0.115      NA
  HPT total (CTX6) -- Facet        NA   0.118      NA
  HPT total (CTX6) -- Int.         NA   0.115      NA
  CONT -- Base                  0.022   0.050   0.071
  CONT -- Facet                 0.024   0.057   0.079
  CONT -- Int.                  0.021   0.059   0.079
  POP_rev -- Base                  NA   0.131      NA
  POP_rev -- Facet                 NA   0.131      NA
  POP_rev -- Int.                  NA   0.135      NA

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
  HPT total     Base          z_frlf_tot                   0.016     -0.117       0.149     0.817
  (CTX6)

  HPT total     Base          z_ksa3_tot                  -0.061     -0.193       0.071     0.362
  (CTX6)

  HPT total     Base          z_kn_total                   0.340      0.226       0.453     0.000
  (CTX6)

  HPT total     Base          z_sdr5_tot                  -0.040     -0.153       0.074     0.492
  (CTX6)

  HPT total     Facet         z_frlf_rd                   -0.045     -0.176       0.087     0.503
  (CTX6)

  HPT total     Facet         z_frlf_ns                    0.065     -0.065       0.194     0.327
  (CTX6)

  HPT total     Facet         z_ksa3_tot                  -0.056     -0.189       0.076     0.402
  (CTX6)

  HPT total     Facet         z_kn_total                   0.341      0.228       0.455     0.000
  (CTX6)

  HPT total     Facet         z_sdr5_tot                  -0.029     -0.145       0.086     0.618
  (CTX6)

  HPT total     Interaction   z_frlf_tot                   0.017     -0.117       0.150     0.806
  (CTX6)

  HPT total     Interaction   z_kn_total                   0.337      0.222       0.452     0.000
  (CTX6)

  HPT total     Interaction   z_ksa3_tot                  -0.061     -0.193       0.071     0.367
  (CTX6)

  HPT total     Interaction   z_sdr5_tot                  -0.039     -0.153       0.075     0.503
  (CTX6)

  HPT total     Interaction   z_frlf_tot:z_kn_total       -0.019     -0.136       0.099     0.755
  (CTX6)

  CONT          Base          z_frlf_tot                   0.006     -0.130       0.143     0.929

  CONT          Base          z_ksa3_tot                   0.054     -0.082       0.189     0.436

  CONT          Base          z_kn_total                   0.215      0.099       0.332     0.000

  CONT          Base          z_sdr5_tot                   0.011     -0.106       0.127     0.859

  CONT          Facet         z_frlf_rd                   -0.075     -0.209       0.060     0.275

  CONT          Facet         z_frlf_ns                    0.082     -0.051       0.215     0.225

  CONT          Facet         z_ksa3_tot                   0.061     -0.075       0.197     0.375

  CONT          Facet         z_kn_total                   0.218      0.102       0.334     0.000

  CONT          Facet         z_sdr5_tot                   0.025     -0.093       0.144     0.671

  CONT          Interaction   z_frlf_tot                   0.011     -0.125       0.148     0.868

  CONT          Interaction   z_kn_total                   0.201      0.084       0.319     0.001

  CONT          Interaction   z_ksa3_tot                   0.057     -0.079       0.192     0.410

  CONT          Interaction   z_sdr5_tot                   0.015     -0.101       0.132     0.799

  CONT          Interaction   z_frlf_tot:z_kn_total       -0.097     -0.217       0.023     0.114

  POP_rev       Base          z_frlf_tot                   0.011     -0.120       0.143     0.867

  POP_rev       Base          z_ksa3_tot                  -0.166     -0.297      -0.036     0.013

  POP_rev       Base          z_kn_total                   0.335      0.223       0.447     0.000

  POP_rev       Base          z_sdr5_tot                  -0.080     -0.193       0.032     0.162

  POP_rev       Facet         z_frlf_rd                    0.002     -0.128       0.133     0.970

  POP_rev       Facet         z_frlf_ns                    0.012     -0.117       0.141     0.856

  POP_rev       Facet         z_ksa3_tot                  -0.166     -0.297      -0.035     0.013

  POP_rev       Facet         z_kn_total                   0.335      0.223       0.447     0.000

  POP_rev       Facet         z_sdr5_tot                  -0.079     -0.194       0.035     0.174

  POP_rev       Interaction   z_frlf_tot                   0.006     -0.125       0.138     0.924

  POP_rev       Interaction   z_kn_total                   0.346      0.233       0.459     0.000

  POP_rev       Interaction   z_ksa3_tot                  -0.168     -0.298      -0.038     0.011

  POP_rev       Interaction   z_sdr5_tot                  -0.084     -0.196       0.029     0.145

  POP_rev       Interaction   z_frlf_tot:z_kn_total        0.075     -0.042       0.191     0.209
  -----------------------------------------------------------------------------------------------

  : Fixed effects (standardized coefficients).

# 4. Results -- decision rules

Interpret these models as associations related to the registered
hypotheses, not as the exact confirmatory tests; see Table S5 for the
deviations:

-   An **H1-relevant association** has a positive coefficient for
    **FR-LF** (either `z_frlf_tot` in Base/Int. or
    `z_frlf_rd`/`z_frlf_ns` in Facet) with *p* \< .05 for **HPT total
    (CTX6)** and/or **CONT**.
-   An **H2-relevant controlled association** has the same pattern after
    adding controls (**KN**, **SDR-5**) and **KSA-3** (already
    included). These are not the exact registered decision rules; see
    Table S5.

**Reading POP_rev.** Because POP is reversed, higher **POP_rev** means
**less presentism / more contextualised fit** on items that originally
cued presentist endorsements. Interpret alongside **CONT**.

# 5. Brief interpretation guide (for the write-up)

-   **Effect size:** Coefficients are **standardised** (β). Values
    around 0.10 are small, 0.20-0.30 moderate for individual-level
    predictors in multilevel models; report 95% CIs.
-   **Clustering:** Report **ICC** to show class-level variance.
-   **Model fit:** Report marginal and conditional R² and compare Base
    vs. Facet vs. Interaction.
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

    ## R version 4.6.1 (2026-06-24)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.5 LTS
    ##
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.12.0
    ## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.12.0  LAPACK version 3.12.0
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
    ##  [1] kableExtra_1.4.1    glue_1.8.1          modelsummary_2.6.0
    ##  [4] broom.mixed_0.2.9.7 effectsize_1.0.3    performance_0.18.2
    ##  [7] lmerTest_3.2-1      lme4_2.0-6          Matrix_1.7-6
    ## [10] janitor_2.2.1       readxl_1.5.0.1      lubridate_1.9.5
    ## [13] forcats_1.0.1       stringr_1.6.0       dplyr_1.2.1
    ## [16] purrr_1.2.2         readr_2.2.0         tidyr_1.3.2
    ## [19] tibble_3.3.1        ggplot2_4.0.3       tidyverse_2.0.0
    ##
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1    viridisLite_0.4.3   farver_2.1.2
    ##  [4] S7_0.2.2            fastmap_1.2.0       bayestestR_0.19.0
    ##  [7] digest_0.6.39       timechange_0.4.0    lifecycle_1.0.5
    ## [10] magrittr_2.0.5      compiler_4.6.1      rlang_1.3.0
    ## [13] tools_4.6.1         yaml_2.3.12         data.table_1.18.6.1
    ## [16] knitr_1.51          xml2_1.6.0          RColorBrewer_1.1-3
    ## [19] tinytable_0.19.0    withr_3.0.3         numDeriv_2016.8-1.1
    ## [22] grid_4.6.1          datawizard_1.4.0    future_1.75.0
    ## [25] globals_0.19.1      scales_1.4.0        MASS_7.3-66
    ## [28] tinytex_0.61        insight_1.5.4       cli_3.6.6
    ## [31] rmarkdown_2.32      reformulas_0.4.4    generics_0.1.4
    ## [34] otel_0.2.0          future.apply_1.20.2 rstudioapi_0.19.0
    ## [37] tzdb_0.5.0          parameters_0.29.3   minqa_1.2.8
    ## [40] splines_4.6.1       parallel_4.6.1      cellranger_1.1.0
    ## [43] vctrs_0.7.3         boot_1.3-32         hms_1.1.4
    ## [46] listenv_1.0.0       systemfonts_1.3.2   parallelly_1.48.0
    ## [49] nloptr_2.2.1        codetools_0.2-20    stringi_1.8.7
    ## [52] gtable_0.3.6        tables_0.9.35       pillar_1.11.1
    ## [55] furrr_0.4.0         htmltools_0.5.9     R6_2.6.1
    ## [58] textshaping_1.0.5   Rdpack_2.6.6        evaluate_1.0.5
    ## [61] lattice_0.23-1      rbibutils_2.4.1     backports_1.5.1
    ## [64] broom_1.0.13        snakecase_0.11.1    Rcpp_1.1.2
    ## [67] checkmate_2.3.4     svglite_2.2.2       nlme_3.1-171
    ## [70] xfun_0.60           pkgconfig_2.0.3
