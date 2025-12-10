# Purpose and scope

This file documents **exploratory robustness checks** of our main
results. We vary how HPT is scored, how ideology is operationalised,
which observations are included, and whether class-level **random
slopes** are needed. The goal is to see if substantive conclusions
survive reasonable perturbations---**not** to hunt for significance.

HPT scoring follows the Hartmann--Hasselhorn / Huijgen instrument logic;
note earlier reports that ROA items can behave inconsistently across
samples, motivating ROA-free alternatives here. We also leverage the
FR-LF dimensions RD and NS for ideology variants, aligned with the
codebook of our dataset. These analyses correspond to the
"contamination" checks pre-registered in the project snapshot.

## Setup

``` r
# Core packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(broom.mixed)
library(performance)
library(gt)
library(glue)

# Nice printing
theme_set(theme_bw())
```

## Data

``` r
# Load the dataset created in 00_data-preparation
load("normalised_responses.RData")
stopifnot(exists("normalised_responses"))
dat_raw <- normalised_responses
```

**How to read variables.** Variable names and coding (KN, POP/ROA/CONT,
RD/NS, KSA facets, SDR) are defined in the project codebook and used
verbatim here.

# 1. Scoring variants for HPT

**Why:** In prior literature, POP and CONT often form one factor, while
ROA can be unstable (e.g., ROA1 cross-loads in some samples). We
therefore compare the **original 9-item average** with **ROA-free** and
**problem-item-free** scores.

``` r
dat <- dat_raw %>%
  mutate(
    # Per codebook, POP/ROA/CONT are coded 1–4 (higher = better fit). :contentReference[oaicite:5]{index=5}
    HPT_total_9   = rowMeans(across(c(POP1:POP3, ROA1:ROA3, CONT1:CONT3)), na.rm = TRUE),
    HPT_total_6   = rowMeans(across(c(POP1:POP3, CONT1:CONT3)), na.rm = TRUE),           # exclude all ROA
    HPT_total_8   = rowMeans(across(c(POP1:POP3, ROA2:ROA3, CONT1:CONT3)), na.rm = TRUE) # drop ROA1 only
  )
```

### Descriptives

``` r
hpt_desc <- dat %>%
  summarise(
    `9-item (POP+ROA+CONT)` = mean(HPT_total_9,  na.rm=TRUE),
    `8-item (drop ROA1)`     = mean(HPT_total_8,  na.rm=TRUE),
    `6-item (drop all ROA)`  = mean(HPT_total_6,  na.rm=TRUE)
  ) %>% pivot_longer(everything(), names_to="Score", values_to="Mean")

gt(hpt_desc) %>%
  fmt_number(columns=Mean, decimals=2) %>%
  tab_header(title="HPT scoring variants — means (higher = better)")
```

```{=html}
<div id="eydbyhqqek" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#eydbyhqqek table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#eydbyhqqek thead, #eydbyhqqek tbody, #eydbyhqqek tfoot, #eydbyhqqek tr, #eydbyhqqek td, #eydbyhqqek th {
  border-style: none;
}

#eydbyhqqek p {
  margin: 0;
  padding: 0;
}

#eydbyhqqek .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#eydbyhqqek .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#eydbyhqqek .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#eydbyhqqek .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#eydbyhqqek .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#eydbyhqqek .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eydbyhqqek .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#eydbyhqqek .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#eydbyhqqek .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#eydbyhqqek .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#eydbyhqqek .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#eydbyhqqek .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#eydbyhqqek .gt_spanner_row {
  border-bottom-style: hidden;
}

#eydbyhqqek .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#eydbyhqqek .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#eydbyhqqek .gt_from_md > :first-child {
  margin-top: 0;
}

#eydbyhqqek .gt_from_md > :last-child {
  margin-bottom: 0;
}

#eydbyhqqek .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#eydbyhqqek .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#eydbyhqqek .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#eydbyhqqek .gt_row_group_first td {
  border-top-width: 2px;
}

#eydbyhqqek .gt_row_group_first th {
  border-top-width: 2px;
}

#eydbyhqqek .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eydbyhqqek .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#eydbyhqqek .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#eydbyhqqek .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eydbyhqqek .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eydbyhqqek .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#eydbyhqqek .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#eydbyhqqek .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#eydbyhqqek .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eydbyhqqek .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#eydbyhqqek .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#eydbyhqqek .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#eydbyhqqek .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#eydbyhqqek .gt_left {
  text-align: left;
}

#eydbyhqqek .gt_center {
  text-align: center;
}

#eydbyhqqek .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#eydbyhqqek .gt_font_normal {
  font-weight: normal;
}

#eydbyhqqek .gt_font_bold {
  font-weight: bold;
}

#eydbyhqqek .gt_font_italic {
  font-style: italic;
}

#eydbyhqqek .gt_super {
  font-size: 65%;
}

#eydbyhqqek .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#eydbyhqqek .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#eydbyhqqek .gt_indent_1 {
  text-indent: 5px;
}

#eydbyhqqek .gt_indent_2 {
  text-indent: 10px;
}

#eydbyhqqek .gt_indent_3 {
  text-indent: 15px;
}

#eydbyhqqek .gt_indent_4 {
  text-indent: 20px;
}

#eydbyhqqek .gt_indent_5 {
  text-indent: 25px;
}

#eydbyhqqek .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#eydbyhqqek div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>HPT scoring variants — means (higher = better)</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Score">Score</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Mean">Mean</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Score" class="gt_row gt_left">9-item (POP+ROA+CONT)</td>
<td headers="Mean" class="gt_row gt_right">2.51</td></tr>
    <tr><td headers="Score" class="gt_row gt_left">8-item (drop ROA1)</td>
<td headers="Mean" class="gt_row gt_right">2.47</td></tr>
    <tr><td headers="Score" class="gt_row gt_left">6-item (drop all ROA)</td>
<td headers="Mean" class="gt_row gt_right">2.36</td></tr>
  </tbody>
  
</table>
</div>
```
**Interpretation.** If the **rankings of groups/effects** are stable
across these scores, conclusions do not hinge on ROA behaviour. If
results flip only when ROA is included, they are **fragile** and likely
influenced by ROA idiosyncrasies noted in earlier work.

------------------------------------------------------------------------

# 2. Ideology operationalisations

**Why:** FR-LF defines six dimensions; we focus on **RD
(right-authoritarian rule)** and **NS (Nazi relativisation)**. We test
(a) **NS-only**, (b) **RD+NS combined** (FR-LF mini), and (c) **KSA-3**
authoritarianism (total and facets).

``` r
dat <- dat %>%
  mutate(
    KN_total   = rowSums(across(KN1:KN6), na.rm = TRUE),        # knowledge mini-test
    SDR_total  = rowSums(across(starts_with("SDR")), na.rm = TRUE),
    NS_sum     = rowSums(across(NS1:NS3), na.rm = TRUE),
    RD_sum     = rowSums(across(RD1:RD3), na.rm = TRUE),
    FRLF_mini  = NS_sum + RD_sum,                               # FR-LF logic (NS + RD)
    KSA_A      = rowSums(across(A1:A3), na.rm = TRUE),
    KSA_U      = rowSums(across(U1:U3), na.rm = TRUE),
    KSA_K      = rowSums(across(K1:K3), na.rm = TRUE),
    KSA_total  = KSA_A + KSA_U + KSA_K
  ) %>%
  # z-standardize predictors for comparability of β
  mutate(across(c(NS_sum, FRLF_mini, KSA_total, KN_total, SDR_total), scale, .names="{.col}_z"))
```

**Interpretation.** If **NS-only** predicts HPT similarly to (or more
strongly than) broad authoritarianism (KSA-3), the HPT score may be
**ideologically contaminated** by Nazi-congruent attitudes, consistent
with our preregistered concern.

------------------------------------------------------------------------

# 3. Exclusions: knowledge outliers & extreme SDR

**Rules (predefined here for sensitivity only):**

-   **Knowledge outliers:** drop KN totals outside the Tukey fence
    (\[Q_1-1.5,IQR,;Q_3+1.5,IQR\]).
-   **Extreme SDR:** drop the **top 10%** of SDR totals (possible
    "faking good"). Codebook notes SDR2--SDR4 are reversed already.

``` r
# Compute fences
kn_q <- quantile(dat$KN_total, probs = c(.25, .75), na.rm = TRUE)
kn_iqr <- kn_q[2]-kn_q[1]
kn_low <- kn_q[1] - 1.5*kn_iqr
kn_high<- kn_q[2] + 1.5*kn_iqr

sdr_p90 <- quantile(dat$SDR_total, probs = .90, na.rm = TRUE)

dat <- dat %>%
  mutate(
    excl_KN  = KN_total < kn_low | KN_total > kn_high,
    excl_SDR = SDR_total >= sdr_p90,
    keep_all = TRUE,
    keep_excl= !(excl_KN | excl_SDR)
  )

table_excl <- tibble(
  Criterion = c("Total N", "Drop KN outliers", "Drop top-10% SDR", "Kept (both rules)"),
  N = c(nrow(dat),
        sum(dat$excl_KN, na.rm=TRUE),
        sum(dat$excl_SDR, na.rm=TRUE),
        sum(dat$keep_excl, na.rm=TRUE))
)

gt(table_excl) %>%
  tab_header(title="Exclusion counts (for sensitivity runs)")
```

```{=html}
<div id="cqeklbckor" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#cqeklbckor table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#cqeklbckor thead, #cqeklbckor tbody, #cqeklbckor tfoot, #cqeklbckor tr, #cqeklbckor td, #cqeklbckor th {
  border-style: none;
}

#cqeklbckor p {
  margin: 0;
  padding: 0;
}

#cqeklbckor .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#cqeklbckor .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#cqeklbckor .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#cqeklbckor .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#cqeklbckor .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cqeklbckor .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cqeklbckor .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cqeklbckor .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#cqeklbckor .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#cqeklbckor .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#cqeklbckor .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#cqeklbckor .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#cqeklbckor .gt_spanner_row {
  border-bottom-style: hidden;
}

#cqeklbckor .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#cqeklbckor .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#cqeklbckor .gt_from_md > :first-child {
  margin-top: 0;
}

#cqeklbckor .gt_from_md > :last-child {
  margin-bottom: 0;
}

#cqeklbckor .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#cqeklbckor .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#cqeklbckor .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#cqeklbckor .gt_row_group_first td {
  border-top-width: 2px;
}

#cqeklbckor .gt_row_group_first th {
  border-top-width: 2px;
}

#cqeklbckor .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cqeklbckor .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#cqeklbckor .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#cqeklbckor .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cqeklbckor .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cqeklbckor .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#cqeklbckor .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#cqeklbckor .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#cqeklbckor .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cqeklbckor .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cqeklbckor .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cqeklbckor .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cqeklbckor .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cqeklbckor .gt_left {
  text-align: left;
}

#cqeklbckor .gt_center {
  text-align: center;
}

#cqeklbckor .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#cqeklbckor .gt_font_normal {
  font-weight: normal;
}

#cqeklbckor .gt_font_bold {
  font-weight: bold;
}

#cqeklbckor .gt_font_italic {
  font-style: italic;
}

#cqeklbckor .gt_super {
  font-size: 65%;
}

#cqeklbckor .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#cqeklbckor .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#cqeklbckor .gt_indent_1 {
  text-indent: 5px;
}

#cqeklbckor .gt_indent_2 {
  text-indent: 10px;
}

#cqeklbckor .gt_indent_3 {
  text-indent: 15px;
}

#cqeklbckor .gt_indent_4 {
  text-indent: 20px;
}

#cqeklbckor .gt_indent_5 {
  text-indent: 25px;
}

#cqeklbckor .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#cqeklbckor div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Exclusion counts (for sensitivity runs)</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Criterion">Criterion</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="N">N</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Criterion" class="gt_row gt_left">Total N</td>
<td headers="N" class="gt_row gt_right">164</td></tr>
    <tr><td headers="Criterion" class="gt_row gt_left">Drop KN outliers</td>
<td headers="N" class="gt_row gt_right">0</td></tr>
    <tr><td headers="Criterion" class="gt_row gt_left">Drop top-10% SDR</td>
<td headers="N" class="gt_row gt_right">22</td></tr>
    <tr><td headers="Criterion" class="gt_row gt_left">Kept (both rules)</td>
<td headers="N" class="gt_row gt_right">142</td></tr>
  </tbody>
  
</table>
</div>
```
**Interpretation.** If effects persist after dropping **low-knowledge**
and **high-SDR** respondents, results are less likely to be artefacts of
misunderstanding or impression management.

------------------------------------------------------------------------

# 4. Mixed models with class clustering & random slopes

We estimate multilevel models (students nested in classes), starting
with random intercepts and then allowing the **ideology effect to vary
by class**. We fit the models for each **HPT scoring** and **ideology**
variant.

``` r
fit_models <- function(data, hpt_var, ideol_var){
  form0 <- as.formula(glue("{hpt_var} ~ {ideol_var} + KN_total_z + SDR_total_z + (1 | class_label)"))
  form1 <- as.formula(glue("{hpt_var} ~ {ideol_var} + KN_total_z + SDR_total_z + (1 + {ideol_var} | class_label)"))
  m0 <- lmer(form0, data = data)
  # Try random slope; if singular, fall back to intercept-only
  m1 <- try(lmer(form1, data = data), silent = TRUE)
  if(inherits(m1,"try-error") || isTRUE(isSingular(m1))) m1 <- NULL
  list(m0=m0, m1=m1)
}

tidy_model <- function(m){
  tibble(
    term   = broom.mixed::tidy(m, effects="fixed")$term,
    estimate = broom.mixed::tidy(m, effects="fixed")$estimate,
    conf.low = confint(m, method="Wald")[names(fixef(m)),1],
    conf.high= confint(m, method="Wald")[names(fixef(m)),2],
    p.value  = broom.mixed::tidy(m, effects="fixed")$p.value,
    R2_marg  = performance::r2_nakagawa(m)$R2_marginal,
    R2_cond  = performance::r2_nakagawa(m)$R2_conditional
  )
}
```

### Run model grid

``` r
hpt_vars   <- c("HPT_total_9","HPT_total_8","HPT_total_6")
ideol_vars <- c("NS_sum_z","FRLF_mini_z","KSA_total_z")

# Full sample
grid_full <- expand_grid(hpt=hpt_vars, ideol=ideol_vars) %>%
  mutate(fits = map2(hpt, ideol, ~fit_models(dat %>% filter(keep_all), .x, .y)),
         m0   = map(fits, "m0"),
         m1   = map(fits, "m1"))

# Exclusion sample (drop KN outliers & top-10% SDR)
grid_excl <- expand_grid(hpt=hpt_vars, ideol=ideol_vars) %>%
  mutate(fits = map2(hpt, ideol, ~fit_models(dat %>% filter(keep_excl), .x, .y)),
         m0   = map(fits, "m0"),
         m1   = map(fits, "m1"))
```

### Summaries (key coefficient = ideology)

``` r
summarise_grid <- function(grid, label){
  out0 <- grid %>%
    mutate(tidy0 = map(m0, tidy_model)) %>%
    unnest(tidy0) %>%
    filter(term == "(Intercept)" | str_detect(term, "NS_sum_z|FRLF_mini_z|KSA_total_z")) %>%
    select(hpt, ideol, term, estimate, conf.low, conf.high, p.value, R2_marg, R2_cond) %>%
    mutate(model = "RI") # random intercept

  out1 <- grid %>%
    filter(!map_lgl(m1, is.null)) %>%
    mutate(tidy1 = map(m1, tidy_model)) %>%
    unnest(tidy1) %>%
    filter(term == "(Intercept)" | str_detect(term, "NS_sum_z|FRLF_mini_z|KSA_total_z")) %>%
    select(hpt, ideol, term, estimate, conf.low, conf.high, p.value, R2_marg, R2_cond) %>%
    mutate(model = "RS") # random slope (ideology)

  bind_rows(out0, out1) %>% mutate(sample = label)
}

tab_full <- summarise_grid(grid_full, "Full")
```

    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Warning: There were 2 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `tidy0 = map(m0, tidy_model)`.
    ## Caused by warning:
    ## ! Can't compute random effect variances. Some variance components equal
    ##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
    ##   and `?performance::check_singularity`).
    ##   Decrease the `tolerance` level to force the calculation of random effect
    ##   variances, or impose priors on your random effects parameters (using
    ##   packages like `brms` or `glmmTMB`).
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Warning: There were 2 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `tidy1 = map(m1, tidy_model)`.
    ## Caused by warning:
    ## ! Can't compute random effect variances. Some variance components equal
    ##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
    ##   and `?performance::check_singularity`).
    ##   Decrease the `tolerance` level to force the calculation of random effect
    ##   variances, or impose priors on your random effects parameters (using
    ##   packages like `brms` or `glmmTMB`).
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

``` r
tab_excl <- summarise_grid(grid_excl, "Exclusions applied")
```

    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Warning: There were 14 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `tidy0 = map(m0, tidy_model)`.
    ## Caused by warning:
    ## ! Can't compute random effect variances. Some variance components equal
    ##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
    ##   and `?performance::check_singularity`).
    ##   Decrease the `tolerance` level to force the calculation of random effect
    ##   variances, or impose priors on your random effects parameters (using
    ##   packages like `brms` or `glmmTMB`).
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 13 remaining warnings.

    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Warning: There were 4 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `tidy1 = map(m1, tidy_model)`.
    ## Caused by warning:
    ## ! Can't compute random effect variances. Some variance components equal
    ##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
    ##   and `?performance::check_singularity`).
    ##   Decrease the `tolerance` level to force the calculation of random effect
    ##   variances, or impose priors on your random effects parameters (using
    ##   packages like `brms` or `glmmTMB`).
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 3 remaining warnings.

``` r
tab_models <- bind_rows(tab_full, tab_excl) %>%
  mutate(ideol = recode(ideol,
                        NS_sum_z="NS (z)", FRLF_mini_z="FR-LF: RD+NS (z)", KSA_total_z="KSA-3 total (z)"),
         hpt   = recode(hpt,
                        HPT_total_9="HPT 9-item", HPT_total_8="HPT 8-item (drop ROA1)",
                        HPT_total_6="HPT 6-item (no ROA)")) %>%
  arrange(sample, hpt, ideol, model)
```

``` r
tab_models %>%
  mutate(across(c(estimate, conf.low, conf.high, R2_marg, R2_cond), ~round(., 3)),
         p.value = signif(p.value, 3)) %>%
  gt() %>%
  tab_header(title="Multilevel models: ideology → HPT (controls: KN, SDR; class clustered)") %>%
  tab_spanner(label = "Effect (β and 95% CI)", columns = c(estimate, conf.low, conf.high)) %>%
  cols_label(sample="Sample", hpt="HPT score", ideol="Ideology", model="Model",
             estimate="β", conf.low="CI low", conf.high="CI high",
             p.value="p", R2_marg="R² (marg.)", R2_cond="R² (cond.)")
```

```{=html}
<div id="vfotjmupmo" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#vfotjmupmo table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#vfotjmupmo thead, #vfotjmupmo tbody, #vfotjmupmo tfoot, #vfotjmupmo tr, #vfotjmupmo td, #vfotjmupmo th {
  border-style: none;
}

#vfotjmupmo p {
  margin: 0;
  padding: 0;
}

#vfotjmupmo .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#vfotjmupmo .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#vfotjmupmo .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vfotjmupmo .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vfotjmupmo .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vfotjmupmo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vfotjmupmo .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vfotjmupmo .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#vfotjmupmo .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#vfotjmupmo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vfotjmupmo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vfotjmupmo .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vfotjmupmo .gt_spanner_row {
  border-bottom-style: hidden;
}

#vfotjmupmo .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#vfotjmupmo .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#vfotjmupmo .gt_from_md > :first-child {
  margin-top: 0;
}

#vfotjmupmo .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vfotjmupmo .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#vfotjmupmo .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#vfotjmupmo .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#vfotjmupmo .gt_row_group_first td {
  border-top-width: 2px;
}

#vfotjmupmo .gt_row_group_first th {
  border-top-width: 2px;
}

#vfotjmupmo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vfotjmupmo .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#vfotjmupmo .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#vfotjmupmo .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vfotjmupmo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vfotjmupmo .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vfotjmupmo .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#vfotjmupmo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vfotjmupmo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vfotjmupmo .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vfotjmupmo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vfotjmupmo .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vfotjmupmo .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vfotjmupmo .gt_left {
  text-align: left;
}

#vfotjmupmo .gt_center {
  text-align: center;
}

#vfotjmupmo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vfotjmupmo .gt_font_normal {
  font-weight: normal;
}

#vfotjmupmo .gt_font_bold {
  font-weight: bold;
}

#vfotjmupmo .gt_font_italic {
  font-style: italic;
}

#vfotjmupmo .gt_super {
  font-size: 65%;
}

#vfotjmupmo .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#vfotjmupmo .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#vfotjmupmo .gt_indent_1 {
  text-indent: 5px;
}

#vfotjmupmo .gt_indent_2 {
  text-indent: 10px;
}

#vfotjmupmo .gt_indent_3 {
  text-indent: 15px;
}

#vfotjmupmo .gt_indent_4 {
  text-indent: 20px;
}

#vfotjmupmo .gt_indent_5 {
  text-indent: 25px;
}

#vfotjmupmo .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#vfotjmupmo div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="11" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Multilevel models: ideology → HPT (controls: KN, SDR; class clustered)</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="hpt">HPT score</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="ideol">Ideology</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="term">term</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="3" scope="colgroup" id="Effect (β and 95% CI)">
        <div class="gt_column_spanner">Effect (β and 95% CI)</div>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="p.value">p</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="R2_marg">R² (marg.)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="R2_cond">R² (cond.)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="model">Model</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="sample">Sample</th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="estimate">β</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="conf.low">CI low</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="conf.high">CI high</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.339</td>
<td headers="conf.low" class="gt_row gt_right">2.263</td>
<td headers="conf.high" class="gt_row gt_right">2.415</td>
<td headers="p.value" class="gt_row gt_right">2.42e-14</td>
<td headers="R2_marg" class="gt_row gt_right">0.001</td>
<td headers="R2_cond" class="gt_row gt_right">0.016</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">0.005</td>
<td headers="conf.low" class="gt_row gt_right">-0.069</td>
<td headers="conf.high" class="gt_row gt_right">0.079</td>
<td headers="p.value" class="gt_row gt_right">8.90e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.001</td>
<td headers="R2_cond" class="gt_row gt_right">0.016</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.337</td>
<td headers="conf.low" class="gt_row gt_right">2.267</td>
<td headers="conf.high" class="gt_row gt_right">2.407</td>
<td headers="p.value" class="gt_row gt_right">9.35e-27</td>
<td headers="R2_marg" class="gt_row gt_right">0.003</td>
<td headers="R2_cond" class="gt_row gt_right">0.058</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">0.011</td>
<td headers="conf.low" class="gt_row gt_right">-0.087</td>
<td headers="conf.high" class="gt_row gt_right">0.110</td>
<td headers="p.value" class="gt_row gt_right">8.27e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.003</td>
<td headers="R2_cond" class="gt_row gt_right">0.058</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.336</td>
<td headers="conf.low" class="gt_row gt_right">2.268</td>
<td headers="conf.high" class="gt_row gt_right">2.404</td>
<td headers="p.value" class="gt_row gt_right">1.40e-105</td>
<td headers="R2_marg" class="gt_row gt_right">0.032</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.078</td>
<td headers="conf.low" class="gt_row gt_right">0.005</td>
<td headers="conf.high" class="gt_row gt_right">0.152</td>
<td headers="p.value" class="gt_row gt_right">3.83e-02</td>
<td headers="R2_marg" class="gt_row gt_right">0.032</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.333</td>
<td headers="conf.low" class="gt_row gt_right">2.264</td>
<td headers="conf.high" class="gt_row gt_right">2.402</td>
<td headers="p.value" class="gt_row gt_right">2.92e-103</td>
<td headers="R2_marg" class="gt_row gt_right">0.039</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.084</td>
<td headers="conf.low" class="gt_row gt_right">-0.014</td>
<td headers="conf.high" class="gt_row gt_right">0.183</td>
<td headers="p.value" class="gt_row gt_right">1.32e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.039</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.339</td>
<td headers="conf.low" class="gt_row gt_right">2.268</td>
<td headers="conf.high" class="gt_row gt_right">2.410</td>
<td headers="p.value" class="gt_row gt_right">1.07e-14</td>
<td headers="R2_marg" class="gt_row gt_right">0.010</td>
<td headers="R2_cond" class="gt_row gt_right">0.015</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">0.040</td>
<td headers="conf.low" class="gt_row gt_right">-0.033</td>
<td headers="conf.high" class="gt_row gt_right">0.113</td>
<td headers="p.value" class="gt_row gt_right">2.85e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.010</td>
<td headers="R2_cond" class="gt_row gt_right">0.015</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.336</td>
<td headers="conf.low" class="gt_row gt_right">2.268</td>
<td headers="conf.high" class="gt_row gt_right">2.405</td>
<td headers="p.value" class="gt_row gt_right">1.55e-94</td>
<td headers="R2_marg" class="gt_row gt_right">0.010</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">0.039</td>
<td headers="conf.low" class="gt_row gt_right">-0.050</td>
<td headers="conf.high" class="gt_row gt_right">0.127</td>
<td headers="p.value" class="gt_row gt_right">4.06e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.010</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.462</td>
<td headers="conf.low" class="gt_row gt_right">2.397</td>
<td headers="conf.high" class="gt_row gt_right">2.528</td>
<td headers="p.value" class="gt_row gt_right">1.17e-110</td>
<td headers="R2_marg" class="gt_row gt_right">0.007</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">-0.001</td>
<td headers="conf.low" class="gt_row gt_right">-0.071</td>
<td headers="conf.high" class="gt_row gt_right">0.068</td>
<td headers="p.value" class="gt_row gt_right">9.74e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.007</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.458</td>
<td headers="conf.low" class="gt_row gt_right">2.393</td>
<td headers="conf.high" class="gt_row gt_right">2.523</td>
<td headers="p.value" class="gt_row gt_right">3.42e-111</td>
<td headers="R2_marg" class="gt_row gt_right">0.030</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.065</td>
<td headers="conf.low" class="gt_row gt_right">-0.005</td>
<td headers="conf.high" class="gt_row gt_right">0.135</td>
<td headers="p.value" class="gt_row gt_right">7.11e-02</td>
<td headers="R2_marg" class="gt_row gt_right">0.030</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.461</td>
<td headers="conf.low" class="gt_row gt_right">2.396</td>
<td headers="conf.high" class="gt_row gt_right">2.527</td>
<td headers="p.value" class="gt_row gt_right">9.23e-111</td>
<td headers="R2_marg" class="gt_row gt_right">0.009</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">0.020</td>
<td headers="conf.low" class="gt_row gt_right">-0.050</td>
<td headers="conf.high" class="gt_row gt_right">0.090</td>
<td headers="p.value" class="gt_row gt_right">5.72e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.009</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.497</td>
<td headers="conf.low" class="gt_row gt_right">2.430</td>
<td headers="conf.high" class="gt_row gt_right">2.564</td>
<td headers="p.value" class="gt_row gt_right">4.56e-110</td>
<td headers="R2_marg" class="gt_row gt_right">0.017</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">-0.004</td>
<td headers="conf.low" class="gt_row gt_right">-0.076</td>
<td headers="conf.high" class="gt_row gt_right">0.067</td>
<td headers="p.value" class="gt_row gt_right">9.03e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.017</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.492</td>
<td headers="conf.low" class="gt_row gt_right">2.425</td>
<td headers="conf.high" class="gt_row gt_right">2.558</td>
<td headers="p.value" class="gt_row gt_right">1.23e-110</td>
<td headers="R2_marg" class="gt_row gt_right">0.042</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.069</td>
<td headers="conf.low" class="gt_row gt_right">-0.003</td>
<td headers="conf.high" class="gt_row gt_right">0.141</td>
<td headers="p.value" class="gt_row gt_right">6.18e-02</td>
<td headers="R2_marg" class="gt_row gt_right">0.042</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.496</td>
<td headers="conf.low" class="gt_row gt_right">2.429</td>
<td headers="conf.high" class="gt_row gt_right">2.563</td>
<td headers="p.value" class="gt_row gt_right">3.70e-110</td>
<td headers="R2_marg" class="gt_row gt_right">0.019</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">0.020</td>
<td headers="conf.low" class="gt_row gt_right">-0.051</td>
<td headers="conf.high" class="gt_row gt_right">0.092</td>
<td headers="p.value" class="gt_row gt_right">5.81e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.019</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.348</td>
<td headers="conf.low" class="gt_row gt_right">2.268</td>
<td headers="conf.high" class="gt_row gt_right">2.427</td>
<td headers="p.value" class="gt_row gt_right">3.60e-15</td>
<td headers="R2_marg" class="gt_row gt_right">0.008</td>
<td headers="R2_cond" class="gt_row gt_right">0.053</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">-0.015</td>
<td headers="conf.low" class="gt_row gt_right">-0.082</td>
<td headers="conf.high" class="gt_row gt_right">0.053</td>
<td headers="p.value" class="gt_row gt_right">6.65e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.008</td>
<td headers="R2_cond" class="gt_row gt_right">0.053</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.349</td>
<td headers="conf.low" class="gt_row gt_right">2.270</td>
<td headers="conf.high" class="gt_row gt_right">2.428</td>
<td headers="p.value" class="gt_row gt_right">7.60e-15</td>
<td headers="R2_marg" class="gt_row gt_right">0.008</td>
<td headers="R2_cond" class="gt_row gt_right">0.069</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">-0.010</td>
<td headers="conf.low" class="gt_row gt_right">-0.088</td>
<td headers="conf.high" class="gt_row gt_right">0.068</td>
<td headers="p.value" class="gt_row gt_right">8.04e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.008</td>
<td headers="R2_cond" class="gt_row gt_right">0.069</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.346</td>
<td headers="conf.low" class="gt_row gt_right">2.274</td>
<td headers="conf.high" class="gt_row gt_right">2.419</td>
<td headers="p.value" class="gt_row gt_right">3.80e-15</td>
<td headers="R2_marg" class="gt_row gt_right">0.019</td>
<td headers="R2_cond" class="gt_row gt_right">0.046</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.047</td>
<td headers="conf.low" class="gt_row gt_right">-0.022</td>
<td headers="conf.high" class="gt_row gt_right">0.116</td>
<td headers="p.value" class="gt_row gt_right">1.86e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.019</td>
<td headers="R2_cond" class="gt_row gt_right">0.046</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.346</td>
<td headers="conf.low" class="gt_row gt_right">2.278</td>
<td headers="conf.high" class="gt_row gt_right">2.415</td>
<td headers="p.value" class="gt_row gt_right">1.68e-15</td>
<td headers="R2_marg" class="gt_row gt_right">0.028</td>
<td headers="R2_cond" class="gt_row gt_right">0.103</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.057</td>
<td headers="conf.low" class="gt_row gt_right">-0.040</td>
<td headers="conf.high" class="gt_row gt_right">0.155</td>
<td headers="p.value" class="gt_row gt_right">2.83e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.028</td>
<td headers="R2_cond" class="gt_row gt_right">0.103</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.347</td>
<td headers="conf.low" class="gt_row gt_right">2.270</td>
<td headers="conf.high" class="gt_row gt_right">2.424</td>
<td headers="p.value" class="gt_row gt_right">1.64e-15</td>
<td headers="R2_marg" class="gt_row gt_right">0.008</td>
<td headers="R2_cond" class="gt_row gt_right">0.045</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">0.010</td>
<td headers="conf.low" class="gt_row gt_right">-0.055</td>
<td headers="conf.high" class="gt_row gt_right">0.076</td>
<td headers="p.value" class="gt_row gt_right">7.56e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.008</td>
<td headers="R2_cond" class="gt_row gt_right">0.045</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.348</td>
<td headers="conf.low" class="gt_row gt_right">2.271</td>
<td headers="conf.high" class="gt_row gt_right">2.424</td>
<td headers="p.value" class="gt_row gt_right">7.61e-15</td>
<td headers="R2_marg" class="gt_row gt_right">0.008</td>
<td headers="R2_cond" class="gt_row gt_right">0.052</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">0.011</td>
<td headers="conf.low" class="gt_row gt_right">-0.059</td>
<td headers="conf.high" class="gt_row gt_right">0.080</td>
<td headers="p.value" class="gt_row gt_right">7.73e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.008</td>
<td headers="R2_cond" class="gt_row gt_right">0.052</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.469</td>
<td headers="conf.low" class="gt_row gt_right">2.401</td>
<td headers="conf.high" class="gt_row gt_right">2.537</td>
<td headers="p.value" class="gt_row gt_right">1.03e-12</td>
<td headers="R2_marg" class="gt_row gt_right">0.018</td>
<td headers="R2_cond" class="gt_row gt_right">0.039</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">-0.009</td>
<td headers="conf.low" class="gt_row gt_right">-0.073</td>
<td headers="conf.high" class="gt_row gt_right">0.055</td>
<td headers="p.value" class="gt_row gt_right">7.88e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.018</td>
<td headers="R2_cond" class="gt_row gt_right">0.039</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.471</td>
<td headers="conf.low" class="gt_row gt_right">2.400</td>
<td headers="conf.high" class="gt_row gt_right">2.542</td>
<td headers="p.value" class="gt_row gt_right">4.22e-16</td>
<td headers="R2_marg" class="gt_row gt_right">0.011</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">0.000</td>
<td headers="conf.low" class="gt_row gt_right">-0.080</td>
<td headers="conf.high" class="gt_row gt_right">0.081</td>
<td headers="p.value" class="gt_row gt_right">9.97e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.011</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.468</td>
<td headers="conf.low" class="gt_row gt_right">2.407</td>
<td headers="conf.high" class="gt_row gt_right">2.529</td>
<td headers="p.value" class="gt_row gt_right">3.15e-11</td>
<td headers="R2_marg" class="gt_row gt_right">0.028</td>
<td headers="R2_cond" class="gt_row gt_right">0.034</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.043</td>
<td headers="conf.low" class="gt_row gt_right">-0.023</td>
<td headers="conf.high" class="gt_row gt_right">0.108</td>
<td headers="p.value" class="gt_row gt_right">2.03e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.028</td>
<td headers="R2_cond" class="gt_row gt_right">0.034</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.469</td>
<td headers="conf.low" class="gt_row gt_right">2.403</td>
<td headers="conf.high" class="gt_row gt_right">2.535</td>
<td headers="p.value" class="gt_row gt_right">1.03e-12</td>
<td headers="R2_marg" class="gt_row gt_right">0.017</td>
<td headers="R2_cond" class="gt_row gt_right">0.035</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">0.006</td>
<td headers="conf.low" class="gt_row gt_right">-0.057</td>
<td headers="conf.high" class="gt_row gt_right">0.068</td>
<td headers="p.value" class="gt_row gt_right">8.61e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.017</td>
<td headers="R2_cond" class="gt_row gt_right">0.035</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.509</td>
<td headers="conf.low" class="gt_row gt_right">2.445</td>
<td headers="conf.high" class="gt_row gt_right">2.573</td>
<td headers="p.value" class="gt_row gt_right">4.04e-12</td>
<td headers="R2_marg" class="gt_row gt_right">0.026</td>
<td headers="R2_cond" class="gt_row gt_right">0.035</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">-0.001</td>
<td headers="conf.low" class="gt_row gt_right">-0.067</td>
<td headers="conf.high" class="gt_row gt_right">0.064</td>
<td headers="p.value" class="gt_row gt_right">9.66e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.026</td>
<td headers="R2_cond" class="gt_row gt_right">0.035</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.507</td>
<td headers="conf.low" class="gt_row gt_right">2.448</td>
<td headers="conf.high" class="gt_row gt_right">2.567</td>
<td headers="p.value" class="gt_row gt_right">1.45e-130</td>
<td headers="R2_marg" class="gt_row gt_right">0.043</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.055</td>
<td headers="conf.low" class="gt_row gt_right">-0.011</td>
<td headers="conf.high" class="gt_row gt_right">0.121</td>
<td headers="p.value" class="gt_row gt_right">1.04e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.043</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.508</td>
<td headers="conf.low" class="gt_row gt_right">2.446</td>
<td headers="conf.high" class="gt_row gt_right">2.571</td>
<td headers="p.value" class="gt_row gt_right">5.38e-12</td>
<td headers="R2_marg" class="gt_row gt_right">0.028</td>
<td headers="R2_cond" class="gt_row gt_right">0.034</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">0.016</td>
<td headers="conf.low" class="gt_row gt_right">-0.048</td>
<td headers="conf.high" class="gt_row gt_right">0.079</td>
<td headers="p.value" class="gt_row gt_right">6.30e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.028</td>
<td headers="R2_cond" class="gt_row gt_right">0.034</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
  </tbody>
  
</table>
</div>
```
**How to read the table.**

-   **Rows** = combinations of HPT scoring (9/8/6 items), ideology
    metric (NS only; FR-LF RD+NS; KSA-3), and model type: **RI** =
    random-intercept by class; **RS** = also random **slope** of
    ideology by class (shown only when not singular).
-   **Key cell** = the **β for ideology** (standardised), with **95%
    CI**.
-   **R² (marg./cond.)** give variance explained by fixed effects and by
    full model.

**Interpretation guide.**

-   If **NS (z)** predicts HPT strongly while KSA-3 does not, HPT may be
    **aligned with Nazi-congruent content** rather than general
    authoritarianism---i.e., content **congruence** instead of better
    historical reasoning. This is the contamination mechanism we
    flagged.
-   If effects are **stable across 9/8/6-item** HPT scores, conclusions
    are **robust** to ROA decisions. If they require ROA to appear,
    caution is warranted given prior ROA instability.
-   If **random slopes** improve fit (higher R²_cond; non-singular), the
    ideology--HPT link **varies by class**. That suggests classroom
    climate/teaching may moderate how ideology maps onto HPT.

------------------------------------------------------------------------

# 5. Sanity checks & clarity plots (optional quick look)

``` r
dat %>%
  ggplot(aes(NS_sum, HPT_total_9)) +
  geom_point(alpha=.3) + geom_smooth(method="lm", se=TRUE) +
  labs(x="NS (sum)", y="HPT total (9-item)",
       title="Bivariate check (unadjusted): NS vs. HPT") +
  theme(plot.title.position="plot")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 4 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 4 rows containing missing values or values outside the scale
    ## range (`geom_point()`).

![](/home/yetty/Projects/phd-029-hpt-and-extremism/outputs/05_sensitivity-analyses_files/figure-markdown/quick-plots-1.png)

**Interpretation.** These quick plots are only to **visualise
direction**; final inferences come from the multilevel models with
controls.

------------------------------------------------------------------------

# 6. Read-outs you can cite in prose

-   **Stable conclusions** across HPT **9/8/6** scoring → results **do
    not depend** on ROA items. (ROA instability has been noted
    previously.)
-   **NS-only** predicting as much/more than **KSA-3** → supports the
    **ideological contamination** concern in our PCI RR snapshot.
-   Survives **knowledge/SDR exclusions** → less likely driven by
    misunderstanding or impression management; SDR handling follows our
    codebook.
-   **Random slopes needed** → ideology effects differ **by class**,
    implying a pedagogical moderation worth exploring (teaching of
    context vs. presentism etc.), in line with the HPT literature's
    emphasis on contextual frames.

------------------------------------------------------------------------

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
    ##  [1] glue_1.8.0          gt_1.1.0            performance_0.15.1 
    ##  [4] broom.mixed_0.2.9.6 broom_1.0.7         lmerTest_3.1-3     
    ##  [7] lme4_1.1-38         Matrix_1.7-1        lubridate_1.9.4    
    ## [10] forcats_1.0.0       stringr_1.5.1       dplyr_1.1.4        
    ## [13] purrr_1.1.0         readr_2.1.5         tidyr_1.3.1        
    ## [16] tibble_3.2.1        ggplot2_4.0.1       tidyverse_2.0.0    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.6        xfun_0.54           insight_1.4.2      
    ##  [4] lattice_0.22-5      tzdb_0.5.0          numDeriv_2016.8-1.1
    ##  [7] vctrs_0.6.5         tools_4.4.2         Rdpack_2.6.4       
    ## [10] generics_0.1.3      parallel_4.4.2      pkgconfig_2.0.3    
    ## [13] RColorBrewer_1.1-3  S7_0.2.1            lifecycle_1.0.4    
    ## [16] compiler_4.4.2      farver_2.1.2        tinytex_0.54       
    ## [19] codetools_0.2-20    sass_0.4.9          htmltools_0.5.8.1  
    ## [22] yaml_2.3.10         pillar_1.10.0       furrr_0.3.1        
    ## [25] nloptr_2.2.1        MASS_7.3-61         reformulas_0.4.1   
    ## [28] boot_1.3-31         nlme_3.1-166        parallelly_1.45.1  
    ## [31] tidyselect_1.2.1    digest_0.6.37       stringi_1.8.4      
    ## [34] future_1.68.0       listenv_0.10.0      labeling_0.4.3     
    ## [37] splines_4.4.2       fastmap_1.2.0       grid_4.4.2         
    ## [40] cli_3.6.5           magrittr_2.0.3      withr_3.0.2        
    ## [43] scales_1.4.0        backports_1.5.0     timechange_0.3.0   
    ## [46] rmarkdown_2.29      globals_0.18.0      hms_1.1.3          
    ## [49] evaluate_1.0.5      knitr_1.50          rbibutils_2.3      
    ## [52] mgcv_1.9-1          rlang_1.1.6         Rcpp_1.0.13-1      
    ## [55] xml2_1.3.6          minqa_1.2.8         R6_2.6.1           
    ## [58] fs_1.6.5
