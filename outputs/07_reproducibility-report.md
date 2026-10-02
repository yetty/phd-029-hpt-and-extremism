# Scope

This documentation report records reproducibility metadata for the
public, de-identified package. It does not load confidential data.

# Public-data checksums

  -----------------------------------------------------------------------
  SHA256
  -----------------------------------------------------------------------
  e65ace539d02eb124257cff4f3465e360978754b1cd538174a9c71fd3c8f11ff
  normalised_responses.RDS

  dad5d9f736ef9c35e3bc942dc59cac182cbc6554ced1460c1e8b987c3851c071
  normalised_responses.xlsx
  -----------------------------------------------------------------------

  : SHA-256 checksums for public analysis data

# Seed policy

The numbered analytic reports set a seed only where stochastic
processing is used. Report 03 sets `1234`; the revision bootstrap uses
`20260910`. Other analyses are deterministic conditional on the declared
data and package versions. Exploratory scripts retain their own
documented seeds.

# Declared package versions

              Package     Version
  ----------- ----------- ---------
  lavaan      lavaan      0.7.2
  lme4        lme4        2.0.6
  lmerTest    lmerTest    3.2.1
  mirt        mirt        1.47
  psych       psych       2.6.5
  semTools    semTools    0.5.9
  tidyverse   tidyverse   2.0.0
  janitor     janitor     2.2.1
  knitr       knitr       1.51
  rmarkdown   rmarkdown   2.32

  : Declared package versions available at render time

# File map

  ---------------------------------------------------------------------------------
  File                                        Role
  ------------------------------------------- -------------------------------------
  01_measurement-checks.Rmd                   Measurement checks

  02_descriptives-and-zero-order.Rmd          Descriptives, correlations, and ICC
                                              components

  03_multilevel-models-hypothesis-tests.Rmd   Multilevel focal models

  04_dif-and-mg-cfa-hpt-bias.Rmd              DIF and MG-CFA

  05_sensitivity-analyses.Rmd                 Sensitivity analyses

  06_appendix-tables-and-figures.Rmd          Supplement inventory

  07_reproducibility-report.Rmd               Reproducibility metadata
  ---------------------------------------------------------------------------------

  : Numbered-report file map

# Session information

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
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1     psych_2.6.5          dplyr_1.2.1
    ##  [4] farver_2.1.2         R.utils_2.13.0       S7_0.2.2
    ##  [7] fastmap_1.2.0        janitor_2.2.1        stringfish_0.19.2
    ## [10] digest_0.6.39        timechange_0.4.0     lifecycle_1.0.5
    ## [13] Deriv_4.3.5          dcurver_0.9.3        cluster_2.1.8.2
    ## [16] mirt_1.47            magrittr_2.0.5       compiler_4.6.1
    ## [19] rlang_1.3.0          tools_4.6.1          yaml_2.3.12
    ## [22] knitr_1.51           mnormt_2.1.2         RColorBrewer_1.1-3
    ## [25] numDeriv_2016.8-1.1  R.oo_1.27.1          grid_4.6.1
    ## [28] stats4_4.6.1         lavaan_0.7-2         e1071_1.7-17
    ## [31] future_1.75.0        progressr_1.0.0      GPArotation_2026.8-2
    ## [34] ggplot2_4.0.3        globals_0.19.1       scales_1.4.0
    ## [37] MASS_7.3-66          tinytex_0.61         cli_3.6.6
    ## [40] rmarkdown_2.32       vegan_2.7-6          reformulas_0.4.4
    ## [43] generics_0.1.4       otel_0.2.0           RcppParallel_6.2.1
    ## [46] future.apply_1.20.2  SimDesign_2.27       sessioninfo_1.2.4
    ## [49] minqa_1.2.8          pbapply_1.7-5        proxy_0.4-29
    ## [52] stringr_1.6.0        audio_0.1-12         splines_4.6.1
    ## [55] parallel_4.6.1       beepr_2.0            vctrs_0.7.3
    ## [58] boot_1.3-32          Matrix_1.7-6         listenv_1.0.0
    ## [61] testthat_3.3.2       clipr_0.8.1          glue_1.8.1
    ## [64] parallelly_1.48.0    nloptr_2.2.1         semTools_0.5-9
    ## [67] codetools_0.2-20     stringi_1.8.7        lubridate_1.9.5
    ## [70] gtable_0.3.6         quadprog_1.5-8       lme4_2.0-6
    ## [73] lmerTest_3.2-1       tibble_3.3.1         pillar_1.11.1
    ## [76] splines2_0.5.4       htmltools_0.5.9      brio_1.1.5
    ## [79] R6_2.6.1             Rdpack_2.6.6         tidyverse_2.0.0
    ## [82] evaluate_1.0.5       pbivnorm_0.6.0       lattice_0.23-1
    ## [85] rbibutils_2.4.1      R.methodsS3_1.8.2    snakecase_0.11.1
    ## [88] class_7.3-24         Rcpp_1.1.2           gridExtra_2.3.1
    ## [91] nlme_3.1-171         permute_0.9-10       mgcv_1.9-4
    ## [94] qs2_0.3.1            xfun_0.60            pkgconfig_2.0.3
