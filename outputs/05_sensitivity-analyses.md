# Purpose and scope

This file documents **exploratory robustness checks** of our main
results. We vary how HPT is scored, how ideology is operationalised,
which observations are included, and whether class-level **random
slopes** are needed. The goal is to see if substantive conclusions
survive reasonable perturbations---**not** to hunt for significance.

HPT scoring follows the Hartmann--Hasselhorn / Huijgen instrument logic;
note earlier reports that ROA items can behave inconsistently across
samples, motivating ROA-free alternatives here. We also leverage the
FR-LF dimensions RD and NS for ideology variants. All results explicitly
use **reversed POP items** so that higher scores mean **more
contextualised/agent-aware** reasoning.

## Setup

``` r
# Core packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(broom.mixed)
library(performance)
library(glue)
library(gt)

# Nice printing
theme_set(theme_bw())
```

## Data

``` r
# Load the dataset created in 00_data-preparation
load("normalised_responses.RData")
stopifnot(exists("normalised_responses"))
dat_raw <- normalised_responses

# Cluster identifiers
dat_raw <- dat_raw %>%
  mutate(
    school_id   = as.factor(school_id),
    class_label = as.factor(class_label),
    class_id    = interaction(school_id, class_label, drop = TRUE)
  )

# Reverse POP (1-4) so higher = more contextualised
POP_rev_items <- paste0("POP", 1:3)
dat_raw <- dat_raw %>%
  mutate(across(all_of(POP_rev_items), ~ 5 - as.numeric(.), .names = "{.col}_rev")) %>%
  mutate(
    HPT_POP_rev = rowMeans(across(paste0(POP_rev_items, "_rev")), na.rm = TRUE),
    HPT_CONT    = rowMeans(across(CONT1:CONT3), na.rm = TRUE),
    HPT_ROA     = rowMeans(across(ROA1:ROA3),   na.rm = TRUE),
    # Canonical composites
    HPT_CTX6    = rowMeans(cbind(HPT_POP_rev, HPT_CONT), na.rm = TRUE),
    HPT_TOT9    = rowMeans(cbind(HPT_POP_rev, HPT_CONT, HPT_ROA), na.rm = TRUE)
  )
```

**Variable dictionary.** KN, POP/ROA/CONT, RD/NS, KSA facets, SDR as per
codebook.

# 1. Scoring variants for HPT (with POP reversed)

``` r
# IMPORTANT: use reversed POP columns in all totals

dat <- dat_raw %>%
  mutate(
    HPT_total_9 = rowMeans(across(c(paste0("POP",1:3, "_rev"), ROA1:ROA3, CONT1:CONT3)), na.rm = TRUE),
    HPT_total_8 = rowMeans(across(c(paste0("POP",1:3, "_rev"), ROA2:ROA3, CONT1:CONT3)), na.rm = TRUE), # drop ROA1
    HPT_total_6 = rowMeans(across(c(paste0("POP",1:3, "_rev"), CONT1:CONT3)), na.rm = TRUE)             # no ROA
  )

# Means & SDs so the reader sees scale location and spread
hpt_desc <- dat %>%
  summarise(
    `9-item (POP_rev + ROA + CONT)` := mean(HPT_total_9,  na.rm=TRUE),
    `8-item (drop ROA1)`            := mean(HPT_total_8,  na.rm=TRUE),
    `6-item (no ROA)`               := mean(HPT_total_6,  na.rm=TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(everything(), names_to = "Score", values_to = "Mean")

hpt_sd <- dat %>%
  summarise(
    `9-item (POP_rev + ROA + CONT)` := sd(HPT_total_9,  na.rm=TRUE),
    `8-item (drop ROA1)`            := sd(HPT_total_8,  na.rm=TRUE),
    `6-item (no ROA)`               := sd(HPT_total_6,  na.rm=TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Score", values_to = "SD")

hpt_desc_tbl <- left_join(hpt_desc, hpt_sd, by = "Score")

hpt_desc_tbl %>%
  gt() %>%
  fmt_number(columns = c(Mean, SD), decimals = 2) %>%
  tab_header(title = "HPT scoring variants (POP reversed): means and SDs")
```

```{=html}
<div id="jvkkneusqy" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jvkkneusqy table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#jvkkneusqy thead, #jvkkneusqy tbody, #jvkkneusqy tfoot, #jvkkneusqy tr, #jvkkneusqy td, #jvkkneusqy th {
  border-style: none;
}

#jvkkneusqy p {
  margin: 0;
  padding: 0;
}

#jvkkneusqy .gt_table {
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

#jvkkneusqy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#jvkkneusqy .gt_title {
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

#jvkkneusqy .gt_subtitle {
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

#jvkkneusqy .gt_heading {
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

#jvkkneusqy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jvkkneusqy .gt_col_headings {
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

#jvkkneusqy .gt_col_heading {
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

#jvkkneusqy .gt_column_spanner_outer {
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

#jvkkneusqy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jvkkneusqy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jvkkneusqy .gt_column_spanner {
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

#jvkkneusqy .gt_spanner_row {
  border-bottom-style: hidden;
}

#jvkkneusqy .gt_group_heading {
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

#jvkkneusqy .gt_empty_group_heading {
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

#jvkkneusqy .gt_from_md > :first-child {
  margin-top: 0;
}

#jvkkneusqy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jvkkneusqy .gt_row {
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

#jvkkneusqy .gt_stub {
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

#jvkkneusqy .gt_stub_row_group {
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

#jvkkneusqy .gt_row_group_first td {
  border-top-width: 2px;
}

#jvkkneusqy .gt_row_group_first th {
  border-top-width: 2px;
}

#jvkkneusqy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jvkkneusqy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#jvkkneusqy .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#jvkkneusqy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jvkkneusqy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jvkkneusqy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jvkkneusqy .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#jvkkneusqy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jvkkneusqy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jvkkneusqy .gt_footnotes {
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

#jvkkneusqy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jvkkneusqy .gt_sourcenotes {
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

#jvkkneusqy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jvkkneusqy .gt_left {
  text-align: left;
}

#jvkkneusqy .gt_center {
  text-align: center;
}

#jvkkneusqy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jvkkneusqy .gt_font_normal {
  font-weight: normal;
}

#jvkkneusqy .gt_font_bold {
  font-weight: bold;
}

#jvkkneusqy .gt_font_italic {
  font-style: italic;
}

#jvkkneusqy .gt_super {
  font-size: 65%;
}

#jvkkneusqy .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#jvkkneusqy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#jvkkneusqy .gt_indent_1 {
  text-indent: 5px;
}

#jvkkneusqy .gt_indent_2 {
  text-indent: 10px;
}

#jvkkneusqy .gt_indent_3 {
  text-indent: 15px;
}

#jvkkneusqy .gt_indent_4 {
  text-indent: 20px;
}

#jvkkneusqy .gt_indent_5 {
  text-indent: 25px;
}

#jvkkneusqy .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#jvkkneusqy div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>HPT scoring variants (POP reversed): means and SDs</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Score">Score</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Mean">Mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SD">SD</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Score" class="gt_row gt_left">9-item (POP_rev + ROA + CONT)</td>
<td headers="Mean" class="gt_row gt_right">2.83</td>
<td headers="SD" class="gt_row gt_right">0.49</td></tr>
    <tr><td headers="Score" class="gt_row gt_left">8-item (drop ROA1)</td>
<td headers="Mean" class="gt_row gt_right">2.82</td>
<td headers="SD" class="gt_row gt_right">0.50</td></tr>
    <tr><td headers="Score" class="gt_row gt_left">6-item (no ROA)</td>
<td headers="Mean" class="gt_row gt_right">2.84</td>
<td headers="SD" class="gt_row gt_right">0.55</td></tr>
  </tbody>
  
</table>
</div>
```
# 2. Ideology operationalisations

``` r
dat <- dat %>%
  mutate(
    KN_total   = rowSums(across(KN1:KN6), na.rm = TRUE),
    SDR_total  = rowSums(across(starts_with("SDR")), na.rm = TRUE),
    NS_sum     = rowSums(across(NS1:NS3), na.rm = TRUE),
    RD_sum     = rowSums(across(RD1:RD3), na.rm = TRUE),
    FRLF_mini  = NS_sum + RD_sum,
    KSA_A      = rowSums(across(A1:A3), na.rm = TRUE),
    KSA_U      = rowSums(across(U1:U3), na.rm = TRUE),
    KSA_K      = rowSums(across(K1:K3), na.rm = TRUE),
    KSA_total  = KSA_A + KSA_U + KSA_K
  ) %>%
  mutate(across(c(NS_sum, RD_sum, FRLF_mini, KSA_total, KN_total, SDR_total), scale, .names = "{.col}_z"))

# Show quick reliables for predictors (descriptive only)
ideo_desc <- dat %>% summarise(
  KN_mean = mean(KN_total, na.rm=TRUE), KN_sd = sd(KN_total, na.rm=TRUE),
  SDR_mean = mean(SDR_total, na.rm=TRUE), SDR_sd = sd(SDR_total, na.rm=TRUE),
  NS_mean = mean(NS_sum, na.rm=TRUE), NS_sd = sd(NS_sum, na.rm=TRUE),
  RD_mean = mean(RD_sum, na.rm=TRUE), RD_sd = sd(RD_sum, na.rm=TRUE),
  KSA_mean = mean(KSA_total, na.rm=TRUE), KSA_sd = sd(KSA_total, na.rm=TRUE)
)
ideo_desc %>% gt() %>% tab_header(title = "Predictor summaries (raw scale units)")
```

```{=html}
<div id="deqtbarktq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#deqtbarktq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#deqtbarktq thead, #deqtbarktq tbody, #deqtbarktq tfoot, #deqtbarktq tr, #deqtbarktq td, #deqtbarktq th {
  border-style: none;
}

#deqtbarktq p {
  margin: 0;
  padding: 0;
}

#deqtbarktq .gt_table {
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

#deqtbarktq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#deqtbarktq .gt_title {
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

#deqtbarktq .gt_subtitle {
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

#deqtbarktq .gt_heading {
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

#deqtbarktq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#deqtbarktq .gt_col_headings {
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

#deqtbarktq .gt_col_heading {
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

#deqtbarktq .gt_column_spanner_outer {
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

#deqtbarktq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#deqtbarktq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#deqtbarktq .gt_column_spanner {
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

#deqtbarktq .gt_spanner_row {
  border-bottom-style: hidden;
}

#deqtbarktq .gt_group_heading {
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

#deqtbarktq .gt_empty_group_heading {
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

#deqtbarktq .gt_from_md > :first-child {
  margin-top: 0;
}

#deqtbarktq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#deqtbarktq .gt_row {
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

#deqtbarktq .gt_stub {
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

#deqtbarktq .gt_stub_row_group {
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

#deqtbarktq .gt_row_group_first td {
  border-top-width: 2px;
}

#deqtbarktq .gt_row_group_first th {
  border-top-width: 2px;
}

#deqtbarktq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#deqtbarktq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#deqtbarktq .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#deqtbarktq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#deqtbarktq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#deqtbarktq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#deqtbarktq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#deqtbarktq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#deqtbarktq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#deqtbarktq .gt_footnotes {
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

#deqtbarktq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#deqtbarktq .gt_sourcenotes {
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

#deqtbarktq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#deqtbarktq .gt_left {
  text-align: left;
}

#deqtbarktq .gt_center {
  text-align: center;
}

#deqtbarktq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#deqtbarktq .gt_font_normal {
  font-weight: normal;
}

#deqtbarktq .gt_font_bold {
  font-weight: bold;
}

#deqtbarktq .gt_font_italic {
  font-style: italic;
}

#deqtbarktq .gt_super {
  font-size: 65%;
}

#deqtbarktq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#deqtbarktq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#deqtbarktq .gt_indent_1 {
  text-indent: 5px;
}

#deqtbarktq .gt_indent_2 {
  text-indent: 10px;
}

#deqtbarktq .gt_indent_3 {
  text-indent: 15px;
}

#deqtbarktq .gt_indent_4 {
  text-indent: 20px;
}

#deqtbarktq .gt_indent_5 {
  text-indent: 25px;
}

#deqtbarktq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#deqtbarktq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="10" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Predictor summaries (raw scale units)</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="KN_mean">KN_mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="KN_sd">KN_sd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SDR_mean">SDR_mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SDR_sd">SDR_sd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="NS_mean">NS_mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="NS_sd">NS_sd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="RD_mean">RD_mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="RD_sd">RD_sd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="KSA_mean">KSA_mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="KSA_sd">KSA_sd</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="KN_mean" class="gt_row gt_right">3.037543</td>
<td headers="KN_sd" class="gt_row gt_right">1.622389</td>
<td headers="SDR_mean" class="gt_row gt_right">14.5802</td>
<td headers="SDR_sd" class="gt_row gt_right">3.914919</td>
<td headers="NS_mean" class="gt_row gt_right">7.078498</td>
<td headers="NS_sd" class="gt_row gt_right">2.825516</td>
<td headers="RD_mean" class="gt_row gt_right">7.351536</td>
<td headers="RD_sd" class="gt_row gt_right">2.892356</td>
<td headers="KSA_mean" class="gt_row gt_right">24.82935</td>
<td headers="KSA_sd" class="gt_row gt_right">6.86351</td></tr>
  </tbody>
  
</table>
</div>
```
# 3. Exclusions: knowledge outliers & extreme SDR

``` r
# Tukey fence for KN; top 10% for SDR
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

excl_tbl <- tibble(
  Criterion = c("Total N", "Drop KN outliers", "Drop top-10% SDR", "Kept (both rules)"),
  N = c(nrow(dat), sum(dat$excl_KN, na.rm=TRUE), sum(dat$excl_SDR, na.rm=TRUE), sum(dat$keep_excl, na.rm=TRUE))
) %>%
  mutate(Percent = scales::percent(N / first(N)))

excl_tbl %>% gt() %>% tab_header(title = "Exclusion counts and percentages")
```

```{=html}
<div id="tafvqeypmj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#tafvqeypmj table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#tafvqeypmj thead, #tafvqeypmj tbody, #tafvqeypmj tfoot, #tafvqeypmj tr, #tafvqeypmj td, #tafvqeypmj th {
  border-style: none;
}

#tafvqeypmj p {
  margin: 0;
  padding: 0;
}

#tafvqeypmj .gt_table {
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

#tafvqeypmj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#tafvqeypmj .gt_title {
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

#tafvqeypmj .gt_subtitle {
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

#tafvqeypmj .gt_heading {
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

#tafvqeypmj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tafvqeypmj .gt_col_headings {
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

#tafvqeypmj .gt_col_heading {
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

#tafvqeypmj .gt_column_spanner_outer {
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

#tafvqeypmj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#tafvqeypmj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#tafvqeypmj .gt_column_spanner {
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

#tafvqeypmj .gt_spanner_row {
  border-bottom-style: hidden;
}

#tafvqeypmj .gt_group_heading {
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

#tafvqeypmj .gt_empty_group_heading {
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

#tafvqeypmj .gt_from_md > :first-child {
  margin-top: 0;
}

#tafvqeypmj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#tafvqeypmj .gt_row {
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

#tafvqeypmj .gt_stub {
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

#tafvqeypmj .gt_stub_row_group {
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

#tafvqeypmj .gt_row_group_first td {
  border-top-width: 2px;
}

#tafvqeypmj .gt_row_group_first th {
  border-top-width: 2px;
}

#tafvqeypmj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tafvqeypmj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#tafvqeypmj .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#tafvqeypmj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tafvqeypmj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tafvqeypmj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#tafvqeypmj .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#tafvqeypmj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#tafvqeypmj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tafvqeypmj .gt_footnotes {
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

#tafvqeypmj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tafvqeypmj .gt_sourcenotes {
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

#tafvqeypmj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tafvqeypmj .gt_left {
  text-align: left;
}

#tafvqeypmj .gt_center {
  text-align: center;
}

#tafvqeypmj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#tafvqeypmj .gt_font_normal {
  font-weight: normal;
}

#tafvqeypmj .gt_font_bold {
  font-weight: bold;
}

#tafvqeypmj .gt_font_italic {
  font-style: italic;
}

#tafvqeypmj .gt_super {
  font-size: 65%;
}

#tafvqeypmj .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#tafvqeypmj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#tafvqeypmj .gt_indent_1 {
  text-indent: 5px;
}

#tafvqeypmj .gt_indent_2 {
  text-indent: 10px;
}

#tafvqeypmj .gt_indent_3 {
  text-indent: 15px;
}

#tafvqeypmj .gt_indent_4 {
  text-indent: 20px;
}

#tafvqeypmj .gt_indent_5 {
  text-indent: 25px;
}

#tafvqeypmj .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#tafvqeypmj div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Exclusion counts and percentages</td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Criterion">Criterion</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="N">N</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Percent">Percent</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Criterion" class="gt_row gt_left">Total N</td>
<td headers="N" class="gt_row gt_right">293</td>
<td headers="Percent" class="gt_row gt_right">100%</td></tr>
    <tr><td headers="Criterion" class="gt_row gt_left">Drop KN outliers</td>
<td headers="N" class="gt_row gt_right">0</td>
<td headers="Percent" class="gt_row gt_right">0%</td></tr>
    <tr><td headers="Criterion" class="gt_row gt_left">Drop top-10% SDR</td>
<td headers="N" class="gt_row gt_right">31</td>
<td headers="Percent" class="gt_row gt_right">11%</td></tr>
    <tr><td headers="Criterion" class="gt_row gt_left">Kept (both rules)</td>
<td headers="N" class="gt_row gt_right">262</td>
<td headers="Percent" class="gt_row gt_right">89%</td></tr>
  </tbody>
  
</table>
</div>
```
# 4. Mixed models with clustering & random slopes

``` r
fit_models <- function(data, hpt_var, ideol_var){
  form0 <- as.formula(glue(
    "{hpt_var} ~ {ideol_var} + KN_total_z + SDR_total_z + (1 | school_id) + (1 | class_id)"
  ))
  form1 <- as.formula(glue(
    "{hpt_var} ~ {ideol_var} + KN_total_z + SDR_total_z + (1 | school_id) + (1 + {ideol_var} | class_id)"
  ))
  m0 <- lmer(form0, data = data)
  m1 <- try(lmer(form1, data = data), silent = TRUE)
  if (inherits(m1, "try-error") || isTRUE(isSingular(m1))) m1 <- NULL
  list(m0 = m0, m1 = m1)
}

summarise_model <- function(m){
  fx <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE)
  r2 <- performance::r2_nakagawa(m)
  fx %>% mutate(R2_marg = r2$R2_marginal, R2_cond = r2$R2_conditional)
}
```

``` r
hpt_vars   <- c("HPT_total_9","HPT_total_8","HPT_total_6")
ideol_vars <- c("NS_sum_z","FRLF_mini_z","KSA_total_z")

# Full sample
full_grid <- tidyr::expand_grid(hpt = hpt_vars, ideol = ideol_vars) %>%
  mutate(fits = map2(hpt, ideol, ~fit_models(dat %>% filter(keep_all), .x, .y)),
         m0   = map(fits, "m0"),
         m1   = map(fits, "m1"))

# Exclusion sample
excl_grid <- tidyr::expand_grid(hpt = hpt_vars, ideol = ideol_vars) %>%
  mutate(fits = map2(hpt, ideol, ~fit_models(dat %>% filter(keep_excl), .x, .y)),
         m0   = map(fits, "m0"),
         m1   = map(fits, "m1"))
```

``` r
collect_table <- function(grid, label){
  out0 <- grid %>% mutate(t0 = map(m0, summarise_model)) %>% unnest(t0) %>% mutate(model = "RI")
  out1 <- grid %>% filter(!map_lgl(m1, is.null)) %>% mutate(t1 = map(m1, summarise_model)) %>% unnest(t1) %>% mutate(model = "RS")
  bind_rows(out0, out1) %>% mutate(sample = label)
}

tab_full <- collect_table(full_grid, "Full")
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

    ## Warning: There were 9 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `t0 = map(m0, summarise_model)`.
    ## Caused by warning:
    ## ! Can't compute random effect variances. Some variance components equal
    ##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
    ##   and `?performance::check_singularity`).
    ##   Decrease the `tolerance` level to force the calculation of random effect
    ##   variances, or impose priors on your random effects parameters (using
    ##   packages like `brms` or `glmmTMB`).
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 8 remaining warnings.

    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `t1 = map(m1, summarise_model)`.
    ## Caused by warning:
    ## ! Can't compute random effect variances. Some variance components equal
    ##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
    ##   and `?performance::check_singularity`).
    ##   Decrease the `tolerance` level to force the calculation of random effect
    ##   variances, or impose priors on your random effects parameters (using
    ##   packages like `brms` or `glmmTMB`).

``` r
tab_excl <- collect_table(excl_grid, "Exclusions applied")
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

    ## Warning: There were 9 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `t0 = map(m0, summarise_model)`.
    ## Caused by warning:
    ## ! Can't compute random effect variances. Some variance components equal
    ##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
    ##   and `?performance::check_singularity`).
    ##   Decrease the `tolerance` level to force the calculation of random effect
    ##   variances, or impose priors on your random effects parameters (using
    ##   packages like `brms` or `glmmTMB`).
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 8 remaining warnings.

    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Warning: There were 3 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `t1 = map(m1, summarise_model)`.
    ## Caused by warning:
    ## ! Can't compute random effect variances. Some variance components equal
    ##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
    ##   and `?performance::check_singularity`).
    ##   Decrease the `tolerance` level to force the calculation of random effect
    ##   variances, or impose priors on your random effects parameters (using
    ##   packages like `brms` or `glmmTMB`).
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 2 remaining warnings.

``` r
# Keep only ideology terms + intercept
tab_models <- bind_rows(tab_full, tab_excl) %>%
  filter(term %in% c("(Intercept)", "NS_sum_z", "FRLF_mini_z", "KSA_total_z")) %>%
  mutate(
    ideol = recode(term, NS_sum_z = "NS (z)", FRLF_mini_z = "FR-LF: RD+NS (z)", KSA_total_z = "KSA-3 total (z)", `(Intercept)` = "(Intercept)"),
    hpt = recode(hpt,
      HPT_total_9 = "HPT 9-item (POP_rev + ROA + CONT)",
      HPT_total_8 = "HPT 8-item (drop ROA1)",
      HPT_total_6 = "HPT 6-item (no ROA)"
    )
  ) %>%
  select(sample, hpt, model, ideol, estimate, conf.low, conf.high, p.value, R2_marg, R2_cond) %>%
  arrange(sample, hpt, ideol, model)

# Display as a compact table
(tab_models %>%
  mutate(across(c(estimate, conf.low, conf.high, R2_marg, R2_cond), ~round(., 3)),
         p.value = signif(p.value, 3)) %>%
  gt() %>%
  tab_header(title = "Multilevel models: ideology → HPT (POP reversed; controls: KN, SDR; school + class clustering)") %>%
  tab_spanner(label = "Effect (β and 95% CI)", columns = c(estimate, conf.low, conf.high)) %>%
  cols_label(sample="Sample", hpt="HPT score", model="Model", ideol="Predictor",
             estimate="β", conf.low="CI low", conf.high="CI high", p.value="p",
             R2_marg="R² (marg.)", R2_cond="R² (cond.)"))
```

```{=html}
<div id="kffwhjlmvc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#kffwhjlmvc table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#kffwhjlmvc thead, #kffwhjlmvc tbody, #kffwhjlmvc tfoot, #kffwhjlmvc tr, #kffwhjlmvc td, #kffwhjlmvc th {
  border-style: none;
}

#kffwhjlmvc p {
  margin: 0;
  padding: 0;
}

#kffwhjlmvc .gt_table {
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

#kffwhjlmvc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#kffwhjlmvc .gt_title {
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

#kffwhjlmvc .gt_subtitle {
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

#kffwhjlmvc .gt_heading {
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

#kffwhjlmvc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kffwhjlmvc .gt_col_headings {
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

#kffwhjlmvc .gt_col_heading {
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

#kffwhjlmvc .gt_column_spanner_outer {
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

#kffwhjlmvc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#kffwhjlmvc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#kffwhjlmvc .gt_column_spanner {
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

#kffwhjlmvc .gt_spanner_row {
  border-bottom-style: hidden;
}

#kffwhjlmvc .gt_group_heading {
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

#kffwhjlmvc .gt_empty_group_heading {
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

#kffwhjlmvc .gt_from_md > :first-child {
  margin-top: 0;
}

#kffwhjlmvc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#kffwhjlmvc .gt_row {
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

#kffwhjlmvc .gt_stub {
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

#kffwhjlmvc .gt_stub_row_group {
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

#kffwhjlmvc .gt_row_group_first td {
  border-top-width: 2px;
}

#kffwhjlmvc .gt_row_group_first th {
  border-top-width: 2px;
}

#kffwhjlmvc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kffwhjlmvc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#kffwhjlmvc .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#kffwhjlmvc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kffwhjlmvc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kffwhjlmvc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#kffwhjlmvc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#kffwhjlmvc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#kffwhjlmvc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kffwhjlmvc .gt_footnotes {
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

#kffwhjlmvc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#kffwhjlmvc .gt_sourcenotes {
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

#kffwhjlmvc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#kffwhjlmvc .gt_left {
  text-align: left;
}

#kffwhjlmvc .gt_center {
  text-align: center;
}

#kffwhjlmvc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#kffwhjlmvc .gt_font_normal {
  font-weight: normal;
}

#kffwhjlmvc .gt_font_bold {
  font-weight: bold;
}

#kffwhjlmvc .gt_font_italic {
  font-style: italic;
}

#kffwhjlmvc .gt_super {
  font-size: 65%;
}

#kffwhjlmvc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#kffwhjlmvc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#kffwhjlmvc .gt_indent_1 {
  text-indent: 5px;
}

#kffwhjlmvc .gt_indent_2 {
  text-indent: 10px;
}

#kffwhjlmvc .gt_indent_3 {
  text-indent: 15px;
}

#kffwhjlmvc .gt_indent_4 {
  text-indent: 20px;
}

#kffwhjlmvc .gt_indent_5 {
  text-indent: 25px;
}

#kffwhjlmvc .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#kffwhjlmvc div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="10" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Multilevel models: ideology → HPT (POP reversed; controls: KN, SDR; school + class clustering)</td>
    </tr>
    
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="sample">Sample</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="hpt">HPT score</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="model">Model</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="ideol">Predictor</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="3" scope="colgroup" id="Effect (β and 95% CI)">
        <div class="gt_column_spanner">Effect (β and 95% CI)</div>
      </th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="p.value">p</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="R2_marg">R² (marg.)</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="R2_cond">R² (cond.)</th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="estimate">β</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="conf.low">CI low</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="conf.high">CI high</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.830</td>
<td headers="conf.low" class="gt_row gt_right">2.716</td>
<td headers="conf.high" class="gt_row gt_right">2.944</td>
<td headers="p.value" class="gt_row gt_right">1.13e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.122</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.831</td>
<td headers="conf.low" class="gt_row gt_right">2.719</td>
<td headers="conf.high" class="gt_row gt_right">2.943</td>
<td headers="p.value" class="gt_row gt_right">1.56e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.121</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.831</td>
<td headers="conf.low" class="gt_row gt_right">2.720</td>
<td headers="conf.high" class="gt_row gt_right">2.943</td>
<td headers="p.value" class="gt_row gt_right">1.03e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.123</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.826</td>
<td headers="conf.low" class="gt_row gt_right">2.712</td>
<td headers="conf.high" class="gt_row gt_right">2.940</td>
<td headers="p.value" class="gt_row gt_right">6.11e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.122</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.006</td>
<td headers="conf.low" class="gt_row gt_right">-0.075</td>
<td headers="conf.high" class="gt_row gt_right">0.062</td>
<td headers="p.value" class="gt_row gt_right">8.53e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.121</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.022</td>
<td headers="conf.low" class="gt_row gt_right">-0.094</td>
<td headers="conf.high" class="gt_row gt_right">0.050</td>
<td headers="p.value" class="gt_row gt_right">5.43e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.123</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.022</td>
<td headers="conf.low" class="gt_row gt_right">-0.046</td>
<td headers="conf.high" class="gt_row gt_right">0.089</td>
<td headers="p.value" class="gt_row gt_right">5.23e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.122</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.025</td>
<td headers="conf.low" class="gt_row gt_right">-0.070</td>
<td headers="conf.high" class="gt_row gt_right">0.121</td>
<td headers="p.value" class="gt_row gt_right">5.70e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.122</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.802</td>
<td headers="conf.low" class="gt_row gt_right">2.689</td>
<td headers="conf.high" class="gt_row gt_right">2.915</td>
<td headers="p.value" class="gt_row gt_right">5.13e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.145</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.802</td>
<td headers="conf.low" class="gt_row gt_right">2.690</td>
<td headers="conf.high" class="gt_row gt_right">2.915</td>
<td headers="p.value" class="gt_row gt_right">4.96e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.145</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.802</td>
<td headers="conf.low" class="gt_row gt_right">2.690</td>
<td headers="conf.high" class="gt_row gt_right">2.915</td>
<td headers="p.value" class="gt_row gt_right">4.37e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.145</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.800</td>
<td headers="conf.low" class="gt_row gt_right">2.687</td>
<td headers="conf.high" class="gt_row gt_right">2.912</td>
<td headers="p.value" class="gt_row gt_right">9.29e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.142</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.006</td>
<td headers="conf.low" class="gt_row gt_right">-0.068</td>
<td headers="conf.high" class="gt_row gt_right">0.055</td>
<td headers="p.value" class="gt_row gt_right">8.38e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.145</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.011</td>
<td headers="conf.low" class="gt_row gt_right">-0.075</td>
<td headers="conf.high" class="gt_row gt_right">0.053</td>
<td headers="p.value" class="gt_row gt_right">7.29e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.145</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.010</td>
<td headers="conf.low" class="gt_row gt_right">-0.050</td>
<td headers="conf.high" class="gt_row gt_right">0.070</td>
<td headers="p.value" class="gt_row gt_right">7.38e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.145</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.020</td>
<td headers="conf.low" class="gt_row gt_right">-0.070</td>
<td headers="conf.high" class="gt_row gt_right">0.109</td>
<td headers="p.value" class="gt_row gt_right">6.38e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.142</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.796</td>
<td headers="conf.low" class="gt_row gt_right">2.674</td>
<td headers="conf.high" class="gt_row gt_right">2.918</td>
<td headers="p.value" class="gt_row gt_right">1.42e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.145</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.796</td>
<td headers="conf.low" class="gt_row gt_right">2.674</td>
<td headers="conf.high" class="gt_row gt_right">2.917</td>
<td headers="p.value" class="gt_row gt_right">1.60e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.145</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.796</td>
<td headers="conf.low" class="gt_row gt_right">2.675</td>
<td headers="conf.high" class="gt_row gt_right">2.917</td>
<td headers="p.value" class="gt_row gt_right">1.55e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.145</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.794</td>
<td headers="conf.low" class="gt_row gt_right">2.674</td>
<td headers="conf.high" class="gt_row gt_right">2.914</td>
<td headers="p.value" class="gt_row gt_right">5.73e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.143</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.005</td>
<td headers="conf.low" class="gt_row gt_right">-0.056</td>
<td headers="conf.high" class="gt_row gt_right">0.065</td>
<td headers="p.value" class="gt_row gt_right">8.76e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.145</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">0.005</td>
<td headers="conf.low" class="gt_row gt_right">-0.058</td>
<td headers="conf.high" class="gt_row gt_right">0.069</td>
<td headers="p.value" class="gt_row gt_right">8.67e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.145</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.018</td>
<td headers="conf.low" class="gt_row gt_right">-0.041</td>
<td headers="conf.high" class="gt_row gt_right">0.077</td>
<td headers="p.value" class="gt_row gt_right">5.53e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.145</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.029</td>
<td headers="conf.low" class="gt_row gt_right">-0.059</td>
<td headers="conf.high" class="gt_row gt_right">0.116</td>
<td headers="p.value" class="gt_row gt_right">4.91e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.143</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.829</td>
<td headers="conf.low" class="gt_row gt_right">2.724</td>
<td headers="conf.high" class="gt_row gt_right">2.934</td>
<td headers="p.value" class="gt_row gt_right">1.62e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.110</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.830</td>
<td headers="conf.low" class="gt_row gt_right">2.726</td>
<td headers="conf.high" class="gt_row gt_right">2.935</td>
<td headers="p.value" class="gt_row gt_right">3.46e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.109</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.832</td>
<td headers="conf.low" class="gt_row gt_right">2.729</td>
<td headers="conf.high" class="gt_row gt_right">2.935</td>
<td headers="p.value" class="gt_row gt_right">5.70e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.110</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.822</td>
<td headers="conf.low" class="gt_row gt_right">2.713</td>
<td headers="conf.high" class="gt_row gt_right">2.930</td>
<td headers="p.value" class="gt_row gt_right">2.27e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.108</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.009</td>
<td headers="conf.low" class="gt_row gt_right">-0.057</td>
<td headers="conf.high" class="gt_row gt_right">0.074</td>
<td headers="p.value" class="gt_row gt_right">7.98e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.109</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.007</td>
<td headers="conf.low" class="gt_row gt_right">-0.081</td>
<td headers="conf.high" class="gt_row gt_right">0.096</td>
<td headers="p.value" class="gt_row gt_right">8.64e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.108</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.020</td>
<td headers="conf.low" class="gt_row gt_right">-0.088</td>
<td headers="conf.high" class="gt_row gt_right">0.048</td>
<td headers="p.value" class="gt_row gt_right">5.61e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.110</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.026</td>
<td headers="conf.low" class="gt_row gt_right">-0.038</td>
<td headers="conf.high" class="gt_row gt_right">0.089</td>
<td headers="p.value" class="gt_row gt_right">4.29e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.110</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.800</td>
<td headers="conf.low" class="gt_row gt_right">2.697</td>
<td headers="conf.high" class="gt_row gt_right">2.903</td>
<td headers="p.value" class="gt_row gt_right">4.25e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.137</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.800</td>
<td headers="conf.low" class="gt_row gt_right">2.698</td>
<td headers="conf.high" class="gt_row gt_right">2.903</td>
<td headers="p.value" class="gt_row gt_right">4.84e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.137</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.801</td>
<td headers="conf.low" class="gt_row gt_right">2.699</td>
<td headers="conf.high" class="gt_row gt_right">2.902</td>
<td headers="p.value" class="gt_row gt_right">4.55e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.137</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.009</td>
<td headers="conf.low" class="gt_row gt_right">-0.049</td>
<td headers="conf.high" class="gt_row gt_right">0.067</td>
<td headers="p.value" class="gt_row gt_right">7.60e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.137</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.011</td>
<td headers="conf.low" class="gt_row gt_right">-0.072</td>
<td headers="conf.high" class="gt_row gt_right">0.050</td>
<td headers="p.value" class="gt_row gt_right">7.22e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.137</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.016</td>
<td headers="conf.low" class="gt_row gt_right">-0.041</td>
<td headers="conf.high" class="gt_row gt_right">0.073</td>
<td headers="p.value" class="gt_row gt_right">5.85e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.137</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.798</td>
<td headers="conf.low" class="gt_row gt_right">2.688</td>
<td headers="conf.high" class="gt_row gt_right">2.907</td>
<td headers="p.value" class="gt_row gt_right">3.89e-10</td>
<td headers="R2_marg" class="gt_row gt_right">0.135</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.798</td>
<td headers="conf.low" class="gt_row gt_right">2.689</td>
<td headers="conf.high" class="gt_row gt_right">2.907</td>
<td headers="p.value" class="gt_row gt_right">4.50e-10</td>
<td headers="R2_marg" class="gt_row gt_right">0.134</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.798</td>
<td headers="conf.low" class="gt_row gt_right">2.690</td>
<td headers="conf.high" class="gt_row gt_right">2.906</td>
<td headers="p.value" class="gt_row gt_right">4.66e-10</td>
<td headers="R2_marg" class="gt_row gt_right">0.133</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.021</td>
<td headers="conf.low" class="gt_row gt_right">-0.037</td>
<td headers="conf.high" class="gt_row gt_right">0.079</td>
<td headers="p.value" class="gt_row gt_right">4.71e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.134</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">0.009</td>
<td headers="conf.low" class="gt_row gt_right">-0.052</td>
<td headers="conf.high" class="gt_row gt_right">0.069</td>
<td headers="p.value" class="gt_row gt_right">7.81e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.133</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.026</td>
<td headers="conf.low" class="gt_row gt_right">-0.030</td>
<td headers="conf.high" class="gt_row gt_right">0.083</td>
<td headers="p.value" class="gt_row gt_right">3.56e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.135</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
  </tbody>
  
</table>
</div>
```
# 5. Sanity plots

``` r
dat %>%
  ggplot(aes(NS_sum, HPT_total_9)) +
  geom_point(alpha=.25) + geom_smooth(method="lm", se=TRUE) +
  labs(x="NS (sum)", y="HPT total (9-item, POP_rev)",
       title="Bivariate check (unadjusted): NS vs. HPT total (POP reversed)") +
  theme(plot.title.position="plot")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 6 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 6 rows containing missing values or values outside the scale
    ## range (`geom_point()`).

![](/home/yetty/Projects/phd-029-hpt-and-extremism/outputs/05_sensitivity-analyses_files/figure-markdown/quick-plots-1.png)

# 6. Read-outs for prose

-   **Stable conclusions** across HPT **9/8/6** scoring → results **do
    not depend** on ROA items.
-   **NS-only** ≳ **KSA-3** → supports the **ideological contamination**
    concern.
-   Survives **knowledge/SDR exclusions** → less likely driven by
    misunderstanding or impression management.
-   **Random slopes needed** → ideology effects differ **by class**
    (pedagogical moderation hypothesis).

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
    ##  [1] gt_1.1.0            glue_1.8.0          performance_0.15.1 
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
