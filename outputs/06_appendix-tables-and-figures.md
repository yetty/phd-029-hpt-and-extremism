``` r
library(dplyr)
library(ggplot2)
```

``` r
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
```

# CONT x POP_rev correlation

Following figure visualises the association between students'
contextualisation scores (CONT) and reversed populist reasoning
(POP_rev). Because both constructs are based on aggregated Likert-type
items, they take on a limited number of discrete values, which results
in many students sharing identical score combinations. To account for
this, the figure uses a frequency-aware scatterplot in which circle size
represents the number of students with the same pair of scores. The
solid line depicts the linear association estimated by ordinary least
squares, and the shaded band indicates the corresponding 95% confidence
interval. Overall, the figure shows a modest positive relationship:
higher levels of anti-populist reasoning tend to be associated with
higher levels of contextualisation, while substantial overlap across the
score range indicates considerable within-level variability.

![](/home/yetty/Projects/phd-029-hpt-and-extremism/outputs/06_appendix-tables-and-figures_files/figure-markdown/fig-cont-poprev-1.png)

``` r
ggplot(plot_dat, aes(x = HPT_POP_rev, y = HPT_CONT)) +
  geom_jitter(width = 0.06, height = 0.06, alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "POP (reversed; higher = less populist reasoning)",
       y = "CONT (higher = more contextualization)") +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](/home/yetty/Projects/phd-029-hpt-and-extremism/outputs/06_appendix-tables-and-figures_files/figure-markdown/jitterplot-1.png)

06_appendix-tables-and-figures.Rmd

Purpose: Produce clean publication-ready outputs. Content:

Tables for reliability, CFA, model summaries.

Plots of HPT score distributions, ideology distributions, predicted
effects.

Item characteristic curves for DIF findings.
