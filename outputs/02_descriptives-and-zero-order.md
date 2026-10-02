# Purpose

This file establishes transparent baseline patterns--distributions,
scale descriptives, zero-order correlations, and between-group
variation--before any modeling. It follows the HPT framework
(POP/ROA/CONT) and scoring conventions used in prior work, but with an
explicit **presentism reversal** so that **higher HPT scores reflect
better contextualization/agent-sensitive reasoning**. FR-LF and KSA
reflect ideological/authoritarian agreement; SDR reflects
social-desirability responding. These descriptives document the
empirical landscape your hypotheses build on. *Interpretation cheatsheet
appears under each table/figure.*

## 1) Data & packages

``` r
# Core
library(dplyr); library(stringr); library(tidyr); library(purrr); library(readxl)
# Tables & plots
library(knitr); library(kableExtra); library(ggplot2); library(scales)
# Correlations
library(psych)           # corr.test
# Multilevel & ICC
library(lme4)            # lmer
source("submissions/pci_psychology/scoring_helpers.R")

theme_set(theme_minimal(base_size = 11))
```

``` r
# Load the dataset created in 00_data-preparation
load("normalised_responses.RData")
stopifnot(exists("normalised_responses"))

all_data <- normalised_responses

# --- Presentism reversal & canonical composites ---
POP_rev_items <- paste0("POP", 1:3)
stopifnot(all(POP_rev_items %in% names(all_data)))

all_data <- all_data %>%
  mutate(
    across(all_of(POP_rev_items), ~ 5 - suppressWarnings(as.numeric(.)),
           .names = "{.col}_rev")
  )

# Ensure factors as in codebook and build unique class_id
all_data <- all_data %>%
  mutate(
    school_id    = as.factor(school_id),
    class_label  = as.factor(class_label),
    school_level = as.factor(school_level),
    school_type  = as.factor(school_type),
    region       = as.factor(region),
    gender       = as.factor(gender),
    class_id     = interaction(school_id, class_label, drop = TRUE)
  )
```

## 2) Scoring: constructs & subscores

``` r
# HPT (1-4): subscores and totals (explicitly using REVERSED POP)
hpt_pop_items_rev  <- paste0("POP", 1:3, "_rev")   # reversed presentism
hpt_roa_items      <- c("ROA1","ROA2","ROA3")
hpt_cont_items     <- c("CONT1","CONT2","CONT3")

# Knowledge (0-6 correct)
kn_items <- paste0("KN", 1:6)

# FR-LF mini (1-5 Likert): RD1-3 + NS1-3
frlf_rd <- paste0("RD", 1:3)
frlf_ns <- paste0("NS", 1:3)

# KSA-3 authoritarianism (1-5 Likert): 9 items
ksa_items <- c(paste0("A",1:3), paste0("U",1:3), paste0("K",1:3))

# SDR-5 (1-5 Likert): SDR2-SDR4 already reversed in the dataset per codebook
sdr_items <- paste0("SDR", 1:5)

# Build scores (HPT_TOTAL := CTX6 as primary; also keep HPT_TOT9 for reference)
dat <- all_data %>%
  mutate(
    HPT_POP_raw = scale_mean(., POP_rev_items, min_answered = 2),
    HPT_POP    = scale_mean(., hpt_pop_items_rev, min_answered = 2),
    HPT_ROA    = scale_mean(., hpt_roa_items, min_answered = 2),
    HPT_CONT   = scale_mean(., hpt_cont_items, min_answered = 2),
    HPT_CTX6   = rowMeans(cbind(HPT_POP, HPT_CONT), na.rm = FALSE),
    HPT_TOT9   = rowMeans(cbind(HPT_POP, HPT_CONT, HPT_ROA),
                          na.rm = FALSE),
    HPT_TOTAL  = HPT_TOT9,
    KN_TOTAL   = rowSums(dplyr::select(., all_of(kn_items)), na.rm = TRUE),
    FRLF_RD    = scale_mean(., frlf_rd, min_answered = 2),
    FRLF_NS    = scale_mean(., frlf_ns, min_answered = 2),
    FRLF_MINI  = scale_mean(., c(frlf_rd, frlf_ns), min_answered = 4),
    KSA_TOTAL  = scale_mean(., ksa_items, min_answered = 7),
    SDR_TOTAL  = scale_mean(., sdr_items, min_answered = 4)
  )
```

> **How to read:** • **HPT (HPT_TOTAL)** uses **reversed POP** by
> construction; higher = better contextualization. • **HPT_CTX6**
> (POP_rev + CONT) is our **primary descriptive score**; **HPT_TOT9**
> (adds ROA) is reported for reference. • **Knowledge (KN_TOTAL)**: 0-6
> correct. **FR-LF/KSA/SDR** follow codebook.

## 3) Sample overview

``` r
vars_context <- c("school_id","class_label","school_level","school_type","region","gender","history_grade")
n_raw <- nrow(all_data); n_anal <- nrow(dat)

kable(data.frame(
  N_raw = n_raw,
  N_after_scoring = n_anal,
  classes = dplyr::n_distinct(all_data$class_id),
  schools = dplyr::n_distinct(all_data$school_id)
), caption = "Sample counters (after loading and scoring).") %>% kable_styling(full_width = FALSE)
```

```{=html}
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
```
```{=html}
<caption>
```
Sample counters (after loading and scoring).
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
N_raw
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
N_after_scoring
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
classes
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
schools
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
293
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
293
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
20
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
10
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
## 4) Descriptives (means, SDs, n, range)

``` r
# Report both totals explicitly
desc_vars <- c("HPT_POP","HPT_ROA","HPT_CONT","HPT_CTX6","HPT_TOT9",
               "KN_TOTAL","FRLF_RD","FRLF_NS","FRLF_MINI",
               "KSA_TOTAL","SDR_TOTAL")

desc_tbl <- dat %>%
  summarise(across(all_of(desc_vars),
                   list(n = ~sum(!is.na(.)),
                        mean = ~mean(., na.rm=TRUE),
                        sd = ~sd(., na.rm=TRUE),
                        min = ~min(., na.rm=TRUE),
                        max = ~max(., na.rm=TRUE)))) %>%
  pivot_longer(everything(),
               names_to = c("variable",".value"),
               names_pattern = "([^_]+_[^_]+|[^_]+)_(n|mean|sd|min|max)")

# Keep original order
desc_tbl$variable <- factor(desc_tbl$variable, levels = desc_vars)

kable(desc_tbl[order(desc_tbl$variable),], booktabs = TRUE, digits = 2,
      caption = "Scale descriptives (student-level).") %>%
  kable_styling(latex_options = c("striped","hold_position"), full_width = FALSE)
```

```{=html}
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
```
```{=html}
<caption>
```
Scale descriptives (student-level).
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
variable
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
n
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
mean
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
sd
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
min
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
max
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
HPT_POP
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
287
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
2.97
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.64
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.33
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
4.00
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
287
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
2.80
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.68
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
4.00
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
287
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
2.71
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.74
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
4.00
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
HPT_CTX6
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
287
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
2.84
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.55
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.33
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
4.00
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
287
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
2.83
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.49
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.67
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
3.89
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
KN_TOTAL
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
293
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
3.04
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.62
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
6.00
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
FRLF_RD
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
285
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
2.54
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.88
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
5.00
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
FRLF_NS
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
286
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
2.43
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.89
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
5.00
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
FRLF_MINI
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
284
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
2.48
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.75
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
5.00
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
KSA_TOTAL
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
283
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
2.86
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.62
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
5.00
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
SDR_TOTAL
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
283
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
3.02
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.63
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
4.60
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
> **Interpretation:** HPT scales should center above 2.5 if students
> generally avoid presentism and contextualize; FR-LF/KSA are not
> "good/bad" in isolation but provide ideological context for later
> modeling; SDR alerts to response bias.

## 5) Distributions (histograms, same axes where possible)

``` r
long_scales <- dat %>%
  select(all_of(desc_vars)) %>%
  pivot_longer(everything(), names_to = "scale", values_to = "value")

ggplot(long_scales, aes(x = value)) +
  geom_histogram(bins = 20) +
  facet_wrap(~scale, scales = "free", ncol = 3) +
  labs(title = "Distributions of scales", x = "Score", y = "Count")
```

`<img src="/home/yetty/PhD/projects/phd-029-hpt-and-extremism/outputs/02_descriptives-and-zero-order_files/figure-markdown/hists-1.png" alt="" width="95%" style="display: block; margin: auto;" />`{=html}

> **Interpretation:** Watch for spikes at bounds (e.g., KN at 0 or max),
> floor/ceiling on FR-LF/KSA, and skew in HPT subscores--useful cues for
> later transformation or robust modeling.

## 6) Zero-order correlations (student level)

``` r
corr_df <- dat %>% select(all_of(desc_vars)) %>% drop_na()
ct <- psych::corr.test(corr_df, use = "pairwise")
round(ct$r, 2)
```

    ##           HPT_POP HPT_ROA HPT_CONT HPT_CTX6 HPT_TOT9 KN_TOTAL FRLF_RD FRLF_NS
    ## HPT_POP      1.00    0.16     0.26     0.76     0.64     0.32   -0.09   -0.07
    ## HPT_ROA      0.16    1.00     0.37     0.34     0.72     0.27    0.02   -0.03
    ## HPT_CONT     0.26    0.37     1.00     0.82     0.78     0.22   -0.02    0.05
    ## HPT_CTX6     0.76    0.34     0.82     1.00     0.90     0.34   -0.07   -0.01
    ## HPT_TOT9     0.64    0.72     0.78     0.90     1.00     0.37   -0.04   -0.02
    ## KN_TOTAL     0.32    0.27     0.22     0.34     0.37     1.00   -0.06   -0.11
    ## FRLF_RD     -0.09    0.02    -0.02    -0.07    -0.04    -0.06    1.00    0.41
    ## FRLF_NS     -0.07   -0.03     0.05    -0.01    -0.02    -0.11    0.41    1.00
    ## FRLF_MINI   -0.09    0.00     0.01    -0.05    -0.03    -0.10    0.84    0.84
    ## KSA_TOTAL   -0.13    0.07     0.07    -0.03     0.01     0.05    0.47    0.40
    ## SDR_TOTAL   -0.05    0.01     0.01    -0.02    -0.01     0.03   -0.07   -0.25
    ##           FRLF_MINI KSA_TOTAL SDR_TOTAL
    ## HPT_POP       -0.09     -0.13     -0.05
    ## HPT_ROA        0.00      0.07      0.01
    ## HPT_CONT       0.01      0.07      0.01
    ## HPT_CTX6      -0.05     -0.03     -0.02
    ## HPT_TOT9      -0.03      0.01     -0.01
    ## KN_TOTAL      -0.10      0.05      0.03
    ## FRLF_RD        0.84      0.47     -0.07
    ## FRLF_NS        0.84      0.40     -0.25
    ## FRLF_MINI      1.00      0.52     -0.19
    ## KSA_TOTAL      0.52      1.00     -0.15
    ## SDR_TOTAL     -0.19     -0.15      1.00

``` r
corr_tab <- as.data.frame(round(ct$r, 2))
corr_tab$Var1 <- rownames(corr_tab)
corr_tab <- corr_tab %>% relocate(Var1)

kable(corr_tab, booktabs = TRUE, caption = "Zero-order Pearson correlations among constructs (pairwise).") %>%
  kable_styling(latex_options = c("hold_position"))
```

```{=html}
<table class="table" style="margin-left: auto; margin-right: auto;">
```
```{=html}
<caption>
```
Zero-order Pearson correlations among constructs (pairwise).
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
<th style="text-align:left;">
```
Var1
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
KN_TOTAL
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
FRLF_RD
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
FRLF_NS
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
FRLF_MINI
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
KSA_TOTAL
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
SDR_TOTAL
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
HPT_POP
```{=html}
</td>
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
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.16
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.26
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.76
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.64
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.32
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.09
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.07
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.09
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.13
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.05
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
<td style="text-align:left;">
```
HPT_ROA
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.16
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.37
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.34
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.72
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.27
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.02
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.03
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.07
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.01
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
<td style="text-align:left;">
```
HPT_CONT
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.26
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.37
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.82
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.78
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.22
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.02
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.05
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.01
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.07
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.01
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
HPT_CTX6
```{=html}
</td>
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
0.76
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.34
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.82
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.90
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.34
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.07
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.01
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.05
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.03
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.02
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
<td style="text-align:left;">
```
HPT_TOT9
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.64
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.72
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.78
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.90
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.37
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.04
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.02
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.03
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.01
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.01
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
KN_TOTAL
```{=html}
</td>
```
```{=html}
<td style="text-align:left;">
```
KN_TOTAL
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.32
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.27
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.22
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.34
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.37
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.06
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.11
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.10
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.05
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.03
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
FRLF_RD
```{=html}
</td>
```
```{=html}
<td style="text-align:left;">
```
FRLF_RD
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.09
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.02
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.02
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.07
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.04
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.06
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.41
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.84
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.47
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.07
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
FRLF_NS
```{=html}
</td>
```
```{=html}
<td style="text-align:left;">
```
FRLF_NS
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.07
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.03
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.05
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.01
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.02
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.11
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.41
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.84
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.40
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.25
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
FRLF_MINI
```{=html}
</td>
```
```{=html}
<td style="text-align:left;">
```
FRLF_MINI
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.09
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.01
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.05
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.03
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.10
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.84
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.84
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.52
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.19
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
KSA_TOTAL
```{=html}
</td>
```
```{=html}
<td style="text-align:left;">
```
KSA_TOTAL
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.13
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.07
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.07
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.03
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.01
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.05
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.47
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.40
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.52
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.15
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
SDR_TOTAL
```{=html}
</td>
```
```{=html}
<td style="text-align:left;">
```
SDR_TOTAL
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.05
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.01
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.01
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.02
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.01
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.03
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.07
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.25
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.19
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
-0.15
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
1.00
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
> **Interpretation:** Correlations locate broad relationships your
> hypotheses rely on. For example, if **FRLF_MINI** correlates
> positively with **HPT_CTX6/HPT_TOT9**, this suggests possible
> ideological alignment inflating contextualization--an effect to test
> with controls in models (knowledge, SDR) and with item-level checks
> later.

## 7) Between-group variation: school/class ICCs

We estimate the variance attributable to schools and classes with
random-intercept models. ICC ≈ proportion of total variance that is
between clusters.

``` r
fit_icc <- function(v) {
  if (!v %in% names(dat)) return(NULL)
  formula <- as.formula(paste0(
    v, " ~ 1 + (1 | school_id) + (1 | school_id:class_label)"
  ))
  tryCatch(
    lme4::lmer(formula, data = dat, REML = TRUE, na.action = na.omit),
    error = function(e) NULL
  )
}

extract_icc <- function(fm) {
  empty <- list(
    ICC_school = NA_real_, ICC_class_within_school = NA_real_,
    ICC_total_cluster = NA_real_, N = NA_integer_, n_schools = NA_integer_,
    n_classes = NA_integer_
  )
  if (is.null(fm)) return(empty)

  variances <- as.data.frame(lme4::VarCorr(fm))
  variance_for <- function(group) {
    value <- variances$vcov[variances$grp == group]
    if (length(value) == 1L) value else NA_real_
  }
  school_variance <- variance_for("school_id")
  class_variance <- variance_for("school_id:class_label")
  residual_variance <- variance_for("Residual")
  total_variance <- school_variance + class_variance + residual_variance
  flist <- lme4::getME(fm, "flist")

  list(
    ICC_school = school_variance / total_variance,
    ICC_class_within_school = class_variance / total_variance,
    ICC_total_cluster = (school_variance + class_variance) / total_variance,
    N = as.integer(stats::nobs(fm)),
    n_schools = nlevels(flist$school_id),
    n_classes = nlevels(flist[["school_id:class_label"]])
  )
}

targets <- c("HPT_CTX6","HPT_TOT9","HPT_POP","HPT_ROA","HPT_CONT",
             "FRLF_MINI","KSA_TOTAL","KN_TOTAL","SDR_TOTAL")

icc_rows <- purrr::map(targets, function(sc) {
  components <- extract_icc(fit_icc(sc))
  data.frame(
    scale = sc,
    ICC_school = round(components$ICC_school, 3),
    ICC_class_within_school = round(components$ICC_class_within_school, 3),
    ICC_total_cluster = round(components$ICC_total_cluster, 3),
    N = components$N,
    n_schools = components$n_schools,
    n_classes = components$n_classes
  )
})
icc_res <- dplyr::bind_rows(icc_rows)
```

``` r
kable(icc_res, booktabs = TRUE,
      caption = "Intraclass correlations from nested school/class models.") %>%
  kable_styling(latex_options = c("striped","hold_position"), full_width = FALSE)
```

```{=html}
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
```
```{=html}
<caption>
```
Intraclass correlations from nested school/class models.
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
<th style="text-align:right;">
```
N
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
n_schools
```{=html}
</th>
```
```{=html}
<th style="text-align:right;">
```
n_classes
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
<td style="text-align:right;">
```
287
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
10
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
20
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
0.085
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
0.085
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
287
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
10
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
20
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
0.025
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
0.025
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
287
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
10
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
20
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
0.092
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.011
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.104
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
287
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
10
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
20
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
0.026
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
0.033
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
287
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
10
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
20
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
FRLF_MINI
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.055
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.015
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
284
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
10
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
20
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
KSA_TOTAL
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.057
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.042
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
283
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
10
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
20
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
KN_TOTAL
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
0.053
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
0.053
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
293
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
10
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
20
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
SDR_TOTAL
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
0.000
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
283
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
10
```{=html}
</td>
```
```{=html}
<td style="text-align:right;">
```
20
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
> **Interpretation:** • **ICC \~ 0.00-0.05** → little clustering
> (ordinary regression OK). • **ICC \~ 0.05-0.15** → modest clustering;
> use cluster-robust SE or multilevel models. • **ICC \> 0.15** → strong
> clustering; multilevel modeling recommended. Compare **HPT** vs
> **FR-LF/KSA**: larger ICCs for ideology might reflect school milieu;
> larger ICCs for HPT may signal classroom task/context effects.

## 8) Quick sanity plots by class/school (optional)

``` r
p1 <- dat %>%
  group_by(school_id, class_id, class_label) %>%
  summarise(n = n(),
            HPT_CTX6 = mean(HPT_CTX6, na.rm=TRUE),
            FRLF_MINI = mean(FRLF_MINI, na.rm=TRUE),
            .groups = "drop") %>%
  pivot_longer(c(HPT_CTX6, FRLF_MINI), names_to="scale", values_to="mean") %>%
  ggplot(aes(x = reorder(class_label, mean), y = mean)) +
  geom_point() +
  facet_wrap(scale ~ school_id, scales = "free_y") +
  coord_flip() +
  labs(x = "Class (label within school)", y = "Class mean",
       title = "Class means within schools (HPT vs FR-LF)")

p1
```

`<img src="/home/yetty/PhD/projects/phd-029-hpt-and-extremism/outputs/02_descriptives-and-zero-order_files/figure-markdown/group-plots-1.png" alt="" width="95%" style="display: block; margin: auto;" />`{=html}

> **Interpretation:** Visual check for unusually high/low classes can
> inform later sensitivity checks (e.g., re-running models without
> extreme classes).

## 9) Reproducibility appendix

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
    ##  [1] lme4_2.0-6       Matrix_1.7-6     psych_2.6.5      scales_1.4.0
    ##  [5] ggplot2_4.0.3    kableExtra_1.4.1 knitr_1.51       readxl_1.5.0.1
    ##  [9] purrr_1.2.2      tidyr_1.3.2      stringr_1.6.0    dplyr_1.2.1
    ##
    ## loaded via a namespace (and not attached):
    ##  [1] generics_0.1.4     xml2_1.6.0         stringi_1.8.7      lattice_0.23-1
    ##  [5] digest_0.6.39      magrittr_2.0.5     evaluate_1.0.5     grid_4.6.1
    ##  [9] RColorBrewer_1.1-3 fastmap_1.2.0      cellranger_1.1.0   tinytex_0.61
    ## [13] viridisLite_0.4.3  textshaping_1.0.5  reformulas_0.4.4   Rdpack_2.6.6
    ## [17] mnormt_2.1.2       cli_3.6.6          rlang_1.3.0        rbibutils_2.4.1
    ## [21] splines_4.6.1      withr_3.0.3        yaml_2.3.12        otel_0.2.0
    ## [25] tools_4.6.1        parallel_4.6.1     nloptr_2.2.1       minqa_1.2.8
    ## [29] boot_1.3-32        vctrs_0.7.3        R6_2.6.1           lifecycle_1.0.5
    ## [33] MASS_7.3-66        pkgconfig_2.0.3    pillar_1.11.1      gtable_0.3.6
    ## [37] Rcpp_1.1.2         glue_1.8.1         systemfonts_1.3.2  xfun_0.60
    ## [41] tibble_3.3.1       tidyselect_1.2.1   rstudioapi_0.19.0  farver_2.1.2
    ## [45] htmltools_0.5.9    nlme_3.1-171       labeling_0.4.3     rmarkdown_2.32
    ## [49] svglite_2.2.2      compiler_4.6.1     S7_0.2.2

------------------------------------------------------------------------

### Notes & interpretation pointers

-   **HPT scales (1-4):** Higher indicates better
    contextualization/agent-sensitive reasoning--**after POP reversal**;
    we report CTX6 (primary) and TOT9 (with ROA) side-by-side.
-   **FR-LF mini (1-5):** Short right-wing authoritarian/Nazi
    relativization composite; higher = stronger endorsement. Use
    primarily as a predictor/covariate and for DIF checks later.
-   **Knowledge:** Treat as a covariate; it often shows small but
    non-zero links to HPT.
-   **SDR:** Use to check attenuation/amplification of sensitive
    attitudes.
