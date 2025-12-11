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
<div id="xmvnndvejr" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#xmvnndvejr table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#xmvnndvejr thead, #xmvnndvejr tbody, #xmvnndvejr tfoot, #xmvnndvejr tr, #xmvnndvejr td, #xmvnndvejr th {
  border-style: none;
}

#xmvnndvejr p {
  margin: 0;
  padding: 0;
}

#xmvnndvejr .gt_table {
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

#xmvnndvejr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#xmvnndvejr .gt_title {
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

#xmvnndvejr .gt_subtitle {
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

#xmvnndvejr .gt_heading {
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

#xmvnndvejr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xmvnndvejr .gt_col_headings {
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

#xmvnndvejr .gt_col_heading {
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

#xmvnndvejr .gt_column_spanner_outer {
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

#xmvnndvejr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xmvnndvejr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xmvnndvejr .gt_column_spanner {
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

#xmvnndvejr .gt_spanner_row {
  border-bottom-style: hidden;
}

#xmvnndvejr .gt_group_heading {
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

#xmvnndvejr .gt_empty_group_heading {
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

#xmvnndvejr .gt_from_md > :first-child {
  margin-top: 0;
}

#xmvnndvejr .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xmvnndvejr .gt_row {
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

#xmvnndvejr .gt_stub {
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

#xmvnndvejr .gt_stub_row_group {
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

#xmvnndvejr .gt_row_group_first td {
  border-top-width: 2px;
}

#xmvnndvejr .gt_row_group_first th {
  border-top-width: 2px;
}

#xmvnndvejr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xmvnndvejr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#xmvnndvejr .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#xmvnndvejr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xmvnndvejr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xmvnndvejr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xmvnndvejr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#xmvnndvejr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xmvnndvejr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xmvnndvejr .gt_footnotes {
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

#xmvnndvejr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xmvnndvejr .gt_sourcenotes {
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

#xmvnndvejr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xmvnndvejr .gt_left {
  text-align: left;
}

#xmvnndvejr .gt_center {
  text-align: center;
}

#xmvnndvejr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xmvnndvejr .gt_font_normal {
  font-weight: normal;
}

#xmvnndvejr .gt_font_bold {
  font-weight: bold;
}

#xmvnndvejr .gt_font_italic {
  font-style: italic;
}

#xmvnndvejr .gt_super {
  font-size: 65%;
}

#xmvnndvejr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#xmvnndvejr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#xmvnndvejr .gt_indent_1 {
  text-indent: 5px;
}

#xmvnndvejr .gt_indent_2 {
  text-indent: 10px;
}

#xmvnndvejr .gt_indent_3 {
  text-indent: 15px;
}

#xmvnndvejr .gt_indent_4 {
  text-indent: 20px;
}

#xmvnndvejr .gt_indent_5 {
  text-indent: 25px;
}

#xmvnndvejr .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#xmvnndvejr div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="Mean" class="gt_row gt_right">2.35</td></tr>
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
<div id="ndluvlolev" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ndluvlolev table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ndluvlolev thead, #ndluvlolev tbody, #ndluvlolev tfoot, #ndluvlolev tr, #ndluvlolev td, #ndluvlolev th {
  border-style: none;
}

#ndluvlolev p {
  margin: 0;
  padding: 0;
}

#ndluvlolev .gt_table {
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

#ndluvlolev .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ndluvlolev .gt_title {
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

#ndluvlolev .gt_subtitle {
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

#ndluvlolev .gt_heading {
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

#ndluvlolev .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ndluvlolev .gt_col_headings {
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

#ndluvlolev .gt_col_heading {
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

#ndluvlolev .gt_column_spanner_outer {
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

#ndluvlolev .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ndluvlolev .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ndluvlolev .gt_column_spanner {
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

#ndluvlolev .gt_spanner_row {
  border-bottom-style: hidden;
}

#ndluvlolev .gt_group_heading {
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

#ndluvlolev .gt_empty_group_heading {
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

#ndluvlolev .gt_from_md > :first-child {
  margin-top: 0;
}

#ndluvlolev .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ndluvlolev .gt_row {
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

#ndluvlolev .gt_stub {
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

#ndluvlolev .gt_stub_row_group {
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

#ndluvlolev .gt_row_group_first td {
  border-top-width: 2px;
}

#ndluvlolev .gt_row_group_first th {
  border-top-width: 2px;
}

#ndluvlolev .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ndluvlolev .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ndluvlolev .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ndluvlolev .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ndluvlolev .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ndluvlolev .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ndluvlolev .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ndluvlolev .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ndluvlolev .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ndluvlolev .gt_footnotes {
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

#ndluvlolev .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ndluvlolev .gt_sourcenotes {
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

#ndluvlolev .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ndluvlolev .gt_left {
  text-align: left;
}

#ndluvlolev .gt_center {
  text-align: center;
}

#ndluvlolev .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ndluvlolev .gt_font_normal {
  font-weight: normal;
}

#ndluvlolev .gt_font_bold {
  font-weight: bold;
}

#ndluvlolev .gt_font_italic {
  font-style: italic;
}

#ndluvlolev .gt_super {
  font-size: 65%;
}

#ndluvlolev .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ndluvlolev .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ndluvlolev .gt_indent_1 {
  text-indent: 5px;
}

#ndluvlolev .gt_indent_2 {
  text-indent: 10px;
}

#ndluvlolev .gt_indent_3 {
  text-indent: 15px;
}

#ndluvlolev .gt_indent_4 {
  text-indent: 20px;
}

#ndluvlolev .gt_indent_5 {
  text-indent: 25px;
}

#ndluvlolev .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ndluvlolev div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="N" class="gt_row gt_right">184</td></tr>
    <tr><td headers="Criterion" class="gt_row gt_left">Drop KN outliers</td>
<td headers="N" class="gt_row gt_right">0</td></tr>
    <tr><td headers="Criterion" class="gt_row gt_left">Drop top-10% SDR</td>
<td headers="N" class="gt_row gt_right">22</td></tr>
    <tr><td headers="Criterion" class="gt_row gt_left">Kept (both rules)</td>
<td headers="N" class="gt_row gt_right">162</td></tr>
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
<div id="iwifwlkfba" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#iwifwlkfba table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#iwifwlkfba thead, #iwifwlkfba tbody, #iwifwlkfba tfoot, #iwifwlkfba tr, #iwifwlkfba td, #iwifwlkfba th {
  border-style: none;
}

#iwifwlkfba p {
  margin: 0;
  padding: 0;
}

#iwifwlkfba .gt_table {
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

#iwifwlkfba .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#iwifwlkfba .gt_title {
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

#iwifwlkfba .gt_subtitle {
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

#iwifwlkfba .gt_heading {
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

#iwifwlkfba .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#iwifwlkfba .gt_col_headings {
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

#iwifwlkfba .gt_col_heading {
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

#iwifwlkfba .gt_column_spanner_outer {
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

#iwifwlkfba .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#iwifwlkfba .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#iwifwlkfba .gt_column_spanner {
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

#iwifwlkfba .gt_spanner_row {
  border-bottom-style: hidden;
}

#iwifwlkfba .gt_group_heading {
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

#iwifwlkfba .gt_empty_group_heading {
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

#iwifwlkfba .gt_from_md > :first-child {
  margin-top: 0;
}

#iwifwlkfba .gt_from_md > :last-child {
  margin-bottom: 0;
}

#iwifwlkfba .gt_row {
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

#iwifwlkfba .gt_stub {
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

#iwifwlkfba .gt_stub_row_group {
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

#iwifwlkfba .gt_row_group_first td {
  border-top-width: 2px;
}

#iwifwlkfba .gt_row_group_first th {
  border-top-width: 2px;
}

#iwifwlkfba .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#iwifwlkfba .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#iwifwlkfba .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#iwifwlkfba .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#iwifwlkfba .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#iwifwlkfba .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#iwifwlkfba .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#iwifwlkfba .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#iwifwlkfba .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#iwifwlkfba .gt_footnotes {
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

#iwifwlkfba .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#iwifwlkfba .gt_sourcenotes {
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

#iwifwlkfba .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#iwifwlkfba .gt_left {
  text-align: left;
}

#iwifwlkfba .gt_center {
  text-align: center;
}

#iwifwlkfba .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#iwifwlkfba .gt_font_normal {
  font-weight: normal;
}

#iwifwlkfba .gt_font_bold {
  font-weight: bold;
}

#iwifwlkfba .gt_font_italic {
  font-style: italic;
}

#iwifwlkfba .gt_super {
  font-size: 65%;
}

#iwifwlkfba .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#iwifwlkfba .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#iwifwlkfba .gt_indent_1 {
  text-indent: 5px;
}

#iwifwlkfba .gt_indent_2 {
  text-indent: 10px;
}

#iwifwlkfba .gt_indent_3 {
  text-indent: 15px;
}

#iwifwlkfba .gt_indent_4 {
  text-indent: 20px;
}

#iwifwlkfba .gt_indent_5 {
  text-indent: 25px;
}

#iwifwlkfba .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#iwifwlkfba div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="conf.low" class="gt_row gt_right">2.272</td>
<td headers="conf.high" class="gt_row gt_right">2.407</td>
<td headers="p.value" class="gt_row gt_right">4.71e-14</td>
<td headers="R2_marg" class="gt_row gt_right">0.008</td>
<td headers="R2_cond" class="gt_row gt_right">0.012</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">0.012</td>
<td headers="conf.low" class="gt_row gt_right">-0.056</td>
<td headers="conf.high" class="gt_row gt_right">0.081</td>
<td headers="p.value" class="gt_row gt_right">7.24e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.008</td>
<td headers="R2_cond" class="gt_row gt_right">0.012</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.335</td>
<td headers="conf.low" class="gt_row gt_right">2.270</td>
<td headers="conf.high" class="gt_row gt_right">2.401</td>
<td headers="p.value" class="gt_row gt_right">2.56e-117</td>
<td headers="R2_marg" class="gt_row gt_right">0.009</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">0.005</td>
<td headers="conf.low" class="gt_row gt_right">-0.086</td>
<td headers="conf.high" class="gt_row gt_right">0.096</td>
<td headers="p.value" class="gt_row gt_right">9.22e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.009</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.335</td>
<td headers="conf.low" class="gt_row gt_right">2.271</td>
<td headers="conf.high" class="gt_row gt_right">2.400</td>
<td headers="p.value" class="gt_row gt_right">3.04e-120</td>
<td headers="R2_marg" class="gt_row gt_right">0.034</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.073</td>
<td headers="conf.low" class="gt_row gt_right">0.004</td>
<td headers="conf.high" class="gt_row gt_right">0.142</td>
<td headers="p.value" class="gt_row gt_right">3.92e-02</td>
<td headers="R2_marg" class="gt_row gt_right">0.034</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.339</td>
<td headers="conf.low" class="gt_row gt_right">2.273</td>
<td headers="conf.high" class="gt_row gt_right">2.404</td>
<td headers="p.value" class="gt_row gt_right">6.89e-15</td>
<td headers="R2_marg" class="gt_row gt_right">0.013</td>
<td headers="R2_cond" class="gt_row gt_right">0.013</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">0.033</td>
<td headers="conf.low" class="gt_row gt_right">-0.036</td>
<td headers="conf.high" class="gt_row gt_right">0.101</td>
<td headers="p.value" class="gt_row gt_right">3.52e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.013</td>
<td headers="R2_cond" class="gt_row gt_right">0.013</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.457</td>
<td headers="conf.low" class="gt_row gt_right">2.394</td>
<td headers="conf.high" class="gt_row gt_right">2.520</td>
<td headers="p.value" class="gt_row gt_right">4.91e-125</td>
<td headers="R2_marg" class="gt_row gt_right">0.001</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">-0.004</td>
<td headers="conf.low" class="gt_row gt_right">-0.070</td>
<td headers="conf.high" class="gt_row gt_right">0.062</td>
<td headers="p.value" class="gt_row gt_right">9.02e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.001</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.454</td>
<td headers="conf.low" class="gt_row gt_right">2.391</td>
<td headers="conf.high" class="gt_row gt_right">2.516</td>
<td headers="p.value" class="gt_row gt_right">2.19e-125</td>
<td headers="R2_marg" class="gt_row gt_right">0.015</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.052</td>
<td headers="conf.low" class="gt_row gt_right">-0.015</td>
<td headers="conf.high" class="gt_row gt_right">0.119</td>
<td headers="p.value" class="gt_row gt_right">1.31e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.015</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.457</td>
<td headers="conf.low" class="gt_row gt_right">2.394</td>
<td headers="conf.high" class="gt_row gt_right">2.520</td>
<td headers="p.value" class="gt_row gt_right">4.47e-125</td>
<td headers="R2_marg" class="gt_row gt_right">0.001</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">0.008</td>
<td headers="conf.low" class="gt_row gt_right">-0.058</td>
<td headers="conf.high" class="gt_row gt_right">0.075</td>
<td headers="p.value" class="gt_row gt_right">8.09e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.001</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.492</td>
<td headers="conf.low" class="gt_row gt_right">2.428</td>
<td headers="conf.high" class="gt_row gt_right">2.556</td>
<td headers="p.value" class="gt_row gt_right">4.00e-125</td>
<td headers="R2_marg" class="gt_row gt_right">0.007</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">-0.006</td>
<td headers="conf.low" class="gt_row gt_right">-0.073</td>
<td headers="conf.high" class="gt_row gt_right">0.060</td>
<td headers="p.value" class="gt_row gt_right">8.53e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.007</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.488</td>
<td headers="conf.low" class="gt_row gt_right">2.425</td>
<td headers="conf.high" class="gt_row gt_right">2.551</td>
<td headers="p.value" class="gt_row gt_right">1.52e-125</td>
<td headers="R2_marg" class="gt_row gt_right">0.024</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.057</td>
<td headers="conf.low" class="gt_row gt_right">-0.011</td>
<td headers="conf.high" class="gt_row gt_right">0.124</td>
<td headers="p.value" class="gt_row gt_right">1.02e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.024</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.491</td>
<td headers="conf.low" class="gt_row gt_right">2.428</td>
<td headers="conf.high" class="gt_row gt_right">2.555</td>
<td headers="p.value" class="gt_row gt_right">3.69e-125</td>
<td headers="R2_marg" class="gt_row gt_right">0.007</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">0.009</td>
<td headers="conf.low" class="gt_row gt_right">-0.059</td>
<td headers="conf.high" class="gt_row gt_right">0.076</td>
<td headers="p.value" class="gt_row gt_right">8.02e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.007</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Exclusions applied</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.354</td>
<td headers="conf.low" class="gt_row gt_right">2.283</td>
<td headers="conf.high" class="gt_row gt_right">2.426</td>
<td headers="p.value" class="gt_row gt_right">8.29e-16</td>
<td headers="R2_marg" class="gt_row gt_right">0.009</td>
<td headers="R2_cond" class="gt_row gt_right">0.035</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">-0.005</td>
<td headers="conf.low" class="gt_row gt_right">-0.069</td>
<td headers="conf.high" class="gt_row gt_right">0.058</td>
<td headers="p.value" class="gt_row gt_right">8.68e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.009</td>
<td headers="R2_cond" class="gt_row gt_right">0.035</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.356</td>
<td headers="conf.low" class="gt_row gt_right">2.284</td>
<td headers="conf.high" class="gt_row gt_right">2.428</td>
<td headers="p.value" class="gt_row gt_right">1.89e-15</td>
<td headers="R2_marg" class="gt_row gt_right">0.010</td>
<td headers="R2_cond" class="gt_row gt_right">0.055</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">-0.009</td>
<td headers="conf.low" class="gt_row gt_right">-0.083</td>
<td headers="conf.high" class="gt_row gt_right">0.066</td>
<td headers="p.value" class="gt_row gt_right">8.18e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.010</td>
<td headers="R2_cond" class="gt_row gt_right">0.055</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.351</td>
<td headers="conf.low" class="gt_row gt_right">2.285</td>
<td headers="conf.high" class="gt_row gt_right">2.418</td>
<td headers="p.value" class="gt_row gt_right">1.35e-15</td>
<td headers="R2_marg" class="gt_row gt_right">0.022</td>
<td headers="R2_cond" class="gt_row gt_right">0.036</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.049</td>
<td headers="conf.low" class="gt_row gt_right">-0.016</td>
<td headers="conf.high" class="gt_row gt_right">0.114</td>
<td headers="p.value" class="gt_row gt_right">1.42e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.022</td>
<td headers="R2_cond" class="gt_row gt_right">0.036</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.349</td>
<td headers="conf.low" class="gt_row gt_right">2.283</td>
<td headers="conf.high" class="gt_row gt_right">2.414</td>
<td headers="p.value" class="gt_row gt_right">3.74e-15</td>
<td headers="R2_marg" class="gt_row gt_right">0.022</td>
<td headers="R2_cond" class="gt_row gt_right">0.059</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.047</td>
<td headers="conf.low" class="gt_row gt_right">-0.033</td>
<td headers="conf.high" class="gt_row gt_right">0.126</td>
<td headers="p.value" class="gt_row gt_right">2.85e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.022</td>
<td headers="R2_cond" class="gt_row gt_right">0.059</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.354</td>
<td headers="conf.low" class="gt_row gt_right">2.283</td>
<td headers="conf.high" class="gt_row gt_right">2.424</td>
<td headers="p.value" class="gt_row gt_right">3.85e-16</td>
<td headers="R2_marg" class="gt_row gt_right">0.009</td>
<td headers="R2_cond" class="gt_row gt_right">0.032</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">0.009</td>
<td headers="conf.low" class="gt_row gt_right">-0.053</td>
<td headers="conf.high" class="gt_row gt_right">0.072</td>
<td headers="p.value" class="gt_row gt_right">7.70e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.009</td>
<td headers="R2_cond" class="gt_row gt_right">0.032</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.353</td>
<td headers="conf.low" class="gt_row gt_right">2.283</td>
<td headers="conf.high" class="gt_row gt_right">2.423</td>
<td headers="p.value" class="gt_row gt_right">7.05e-16</td>
<td headers="R2_marg" class="gt_row gt_right">0.010</td>
<td headers="R2_cond" class="gt_row gt_right">0.038</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">0.008</td>
<td headers="conf.low" class="gt_row gt_right">-0.058</td>
<td headers="conf.high" class="gt_row gt_right">0.074</td>
<td headers="p.value" class="gt_row gt_right">8.23e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.010</td>
<td headers="R2_cond" class="gt_row gt_right">0.038</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.470</td>
<td headers="conf.low" class="gt_row gt_right">2.408</td>
<td headers="conf.high" class="gt_row gt_right">2.532</td>
<td headers="p.value" class="gt_row gt_right">1.25e-15</td>
<td headers="R2_marg" class="gt_row gt_right">0.006</td>
<td headers="R2_cond" class="gt_row gt_right">0.016</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">-0.008</td>
<td headers="conf.low" class="gt_row gt_right">-0.069</td>
<td headers="conf.high" class="gt_row gt_right">0.053</td>
<td headers="p.value" class="gt_row gt_right">7.97e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.006</td>
<td headers="R2_cond" class="gt_row gt_right">0.016</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.467</td>
<td headers="conf.low" class="gt_row gt_right">2.408</td>
<td headers="conf.high" class="gt_row gt_right">2.526</td>
<td headers="p.value" class="gt_row gt_right">3.18e-15</td>
<td headers="R2_marg" class="gt_row gt_right">0.012</td>
<td headers="R2_cond" class="gt_row gt_right">0.016</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.034</td>
<td headers="conf.low" class="gt_row gt_right">-0.029</td>
<td headers="conf.high" class="gt_row gt_right">0.096</td>
<td headers="p.value" class="gt_row gt_right">2.93e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.012</td>
<td headers="R2_cond" class="gt_row gt_right">0.016</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.469</td>
<td headers="conf.low" class="gt_row gt_right">2.408</td>
<td headers="conf.high" class="gt_row gt_right">2.531</td>
<td headers="p.value" class="gt_row gt_right">6.07e-16</td>
<td headers="R2_marg" class="gt_row gt_right">0.006</td>
<td headers="R2_cond" class="gt_row gt_right">0.015</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">-0.001</td>
<td headers="conf.low" class="gt_row gt_right">-0.062</td>
<td headers="conf.high" class="gt_row gt_right">0.059</td>
<td headers="p.value" class="gt_row gt_right">9.67e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.006</td>
<td headers="R2_cond" class="gt_row gt_right">0.015</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.507</td>
<td headers="conf.low" class="gt_row gt_right">2.446</td>
<td headers="conf.high" class="gt_row gt_right">2.568</td>
<td headers="p.value" class="gt_row gt_right">5.76e-18</td>
<td headers="R2_marg" class="gt_row gt_right">0.013</td>
<td headers="R2_cond" class="gt_row gt_right">0.019</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="term" class="gt_row gt_left">FRLF_mini_z</td>
<td headers="estimate" class="gt_row gt_right">-0.003</td>
<td headers="conf.low" class="gt_row gt_right">-0.064</td>
<td headers="conf.high" class="gt_row gt_right">0.059</td>
<td headers="p.value" class="gt_row gt_right">9.31e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.013</td>
<td headers="R2_cond" class="gt_row gt_right">0.019</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.504</td>
<td headers="conf.low" class="gt_row gt_right">2.445</td>
<td headers="conf.high" class="gt_row gt_right">2.562</td>
<td headers="p.value" class="gt_row gt_right">2.34e-17</td>
<td headers="R2_marg" class="gt_row gt_right">0.024</td>
<td headers="R2_cond" class="gt_row gt_right">0.026</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="term" class="gt_row gt_left">KSA_total_z</td>
<td headers="estimate" class="gt_row gt_right">0.046</td>
<td headers="conf.low" class="gt_row gt_right">-0.017</td>
<td headers="conf.high" class="gt_row gt_right">0.109</td>
<td headers="p.value" class="gt_row gt_right">1.58e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.024</td>
<td headers="R2_cond" class="gt_row gt_right">0.026</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.507</td>
<td headers="conf.low" class="gt_row gt_right">2.446</td>
<td headers="conf.high" class="gt_row gt_right">2.567</td>
<td headers="p.value" class="gt_row gt_right">3.08e-18</td>
<td headers="R2_marg" class="gt_row gt_right">0.013</td>
<td headers="R2_cond" class="gt_row gt_right">0.018</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="sample" class="gt_row gt_left">Full</td></tr>
    <tr><td headers="hpt" class="gt_row gt_left">HPT 9-item</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="term" class="gt_row gt_left">NS_sum_z</td>
<td headers="estimate" class="gt_row gt_right">0.007</td>
<td headers="conf.low" class="gt_row gt_right">-0.054</td>
<td headers="conf.high" class="gt_row gt_right">0.068</td>
<td headers="p.value" class="gt_row gt_right">8.25e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.013</td>
<td headers="R2_cond" class="gt_row gt_right">0.018</td>
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
