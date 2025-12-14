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
<div id="dipdzmhbmc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#dipdzmhbmc table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#dipdzmhbmc thead, #dipdzmhbmc tbody, #dipdzmhbmc tfoot, #dipdzmhbmc tr, #dipdzmhbmc td, #dipdzmhbmc th {
  border-style: none;
}

#dipdzmhbmc p {
  margin: 0;
  padding: 0;
}

#dipdzmhbmc .gt_table {
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

#dipdzmhbmc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#dipdzmhbmc .gt_title {
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

#dipdzmhbmc .gt_subtitle {
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

#dipdzmhbmc .gt_heading {
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

#dipdzmhbmc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dipdzmhbmc .gt_col_headings {
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

#dipdzmhbmc .gt_col_heading {
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

#dipdzmhbmc .gt_column_spanner_outer {
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

#dipdzmhbmc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#dipdzmhbmc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#dipdzmhbmc .gt_column_spanner {
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

#dipdzmhbmc .gt_spanner_row {
  border-bottom-style: hidden;
}

#dipdzmhbmc .gt_group_heading {
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

#dipdzmhbmc .gt_empty_group_heading {
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

#dipdzmhbmc .gt_from_md > :first-child {
  margin-top: 0;
}

#dipdzmhbmc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#dipdzmhbmc .gt_row {
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

#dipdzmhbmc .gt_stub {
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

#dipdzmhbmc .gt_stub_row_group {
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

#dipdzmhbmc .gt_row_group_first td {
  border-top-width: 2px;
}

#dipdzmhbmc .gt_row_group_first th {
  border-top-width: 2px;
}

#dipdzmhbmc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dipdzmhbmc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#dipdzmhbmc .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#dipdzmhbmc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dipdzmhbmc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dipdzmhbmc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#dipdzmhbmc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#dipdzmhbmc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#dipdzmhbmc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dipdzmhbmc .gt_footnotes {
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

#dipdzmhbmc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#dipdzmhbmc .gt_sourcenotes {
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

#dipdzmhbmc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#dipdzmhbmc .gt_left {
  text-align: left;
}

#dipdzmhbmc .gt_center {
  text-align: center;
}

#dipdzmhbmc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#dipdzmhbmc .gt_font_normal {
  font-weight: normal;
}

#dipdzmhbmc .gt_font_bold {
  font-weight: bold;
}

#dipdzmhbmc .gt_font_italic {
  font-style: italic;
}

#dipdzmhbmc .gt_super {
  font-size: 65%;
}

#dipdzmhbmc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#dipdzmhbmc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#dipdzmhbmc .gt_indent_1 {
  text-indent: 5px;
}

#dipdzmhbmc .gt_indent_2 {
  text-indent: 10px;
}

#dipdzmhbmc .gt_indent_3 {
  text-indent: 15px;
}

#dipdzmhbmc .gt_indent_4 {
  text-indent: 20px;
}

#dipdzmhbmc .gt_indent_5 {
  text-indent: 25px;
}

#dipdzmhbmc .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#dipdzmhbmc div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="Mean" class="gt_row gt_right">2.84</td>
<td headers="SD" class="gt_row gt_right">0.49</td></tr>
    <tr><td headers="Score" class="gt_row gt_left">8-item (drop ROA1)</td>
<td headers="Mean" class="gt_row gt_right">2.84</td>
<td headers="SD" class="gt_row gt_right">0.50</td></tr>
    <tr><td headers="Score" class="gt_row gt_left">6-item (no ROA)</td>
<td headers="Mean" class="gt_row gt_right">2.86</td>
<td headers="SD" class="gt_row gt_right">0.57</td></tr>
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
<div id="bgzewzhqre" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#bgzewzhqre table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#bgzewzhqre thead, #bgzewzhqre tbody, #bgzewzhqre tfoot, #bgzewzhqre tr, #bgzewzhqre td, #bgzewzhqre th {
  border-style: none;
}

#bgzewzhqre p {
  margin: 0;
  padding: 0;
}

#bgzewzhqre .gt_table {
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

#bgzewzhqre .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#bgzewzhqre .gt_title {
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

#bgzewzhqre .gt_subtitle {
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

#bgzewzhqre .gt_heading {
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

#bgzewzhqre .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bgzewzhqre .gt_col_headings {
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

#bgzewzhqre .gt_col_heading {
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

#bgzewzhqre .gt_column_spanner_outer {
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

#bgzewzhqre .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#bgzewzhqre .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#bgzewzhqre .gt_column_spanner {
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

#bgzewzhqre .gt_spanner_row {
  border-bottom-style: hidden;
}

#bgzewzhqre .gt_group_heading {
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

#bgzewzhqre .gt_empty_group_heading {
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

#bgzewzhqre .gt_from_md > :first-child {
  margin-top: 0;
}

#bgzewzhqre .gt_from_md > :last-child {
  margin-bottom: 0;
}

#bgzewzhqre .gt_row {
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

#bgzewzhqre .gt_stub {
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

#bgzewzhqre .gt_stub_row_group {
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

#bgzewzhqre .gt_row_group_first td {
  border-top-width: 2px;
}

#bgzewzhqre .gt_row_group_first th {
  border-top-width: 2px;
}

#bgzewzhqre .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bgzewzhqre .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#bgzewzhqre .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#bgzewzhqre .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bgzewzhqre .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bgzewzhqre .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#bgzewzhqre .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#bgzewzhqre .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#bgzewzhqre .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bgzewzhqre .gt_footnotes {
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

#bgzewzhqre .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bgzewzhqre .gt_sourcenotes {
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

#bgzewzhqre .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#bgzewzhqre .gt_left {
  text-align: left;
}

#bgzewzhqre .gt_center {
  text-align: center;
}

#bgzewzhqre .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#bgzewzhqre .gt_font_normal {
  font-weight: normal;
}

#bgzewzhqre .gt_font_bold {
  font-weight: bold;
}

#bgzewzhqre .gt_font_italic {
  font-style: italic;
}

#bgzewzhqre .gt_super {
  font-size: 65%;
}

#bgzewzhqre .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#bgzewzhqre .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#bgzewzhqre .gt_indent_1 {
  text-indent: 5px;
}

#bgzewzhqre .gt_indent_2 {
  text-indent: 10px;
}

#bgzewzhqre .gt_indent_3 {
  text-indent: 15px;
}

#bgzewzhqre .gt_indent_4 {
  text-indent: 20px;
}

#bgzewzhqre .gt_indent_5 {
  text-indent: 25px;
}

#bgzewzhqre .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#bgzewzhqre div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
    <tr><td headers="KN_mean" class="gt_row gt_right">3.153846</td>
<td headers="KN_sd" class="gt_row gt_right">1.657939</td>
<td headers="SDR_mean" class="gt_row gt_right">14.61111</td>
<td headers="SDR_sd" class="gt_row gt_right">3.828743</td>
<td headers="NS_mean" class="gt_row gt_right">7.136752</td>
<td headers="NS_sd" class="gt_row gt_right">2.923652</td>
<td headers="RD_mean" class="gt_row gt_right">7.34188</td>
<td headers="RD_sd" class="gt_row gt_right">2.939044</td>
<td headers="KSA_mean" class="gt_row gt_right">25.13248</td>
<td headers="KSA_sd" class="gt_row gt_right">6.821102</td></tr>
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
<div id="sonondxqas" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#sonondxqas table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#sonondxqas thead, #sonondxqas tbody, #sonondxqas tfoot, #sonondxqas tr, #sonondxqas td, #sonondxqas th {
  border-style: none;
}

#sonondxqas p {
  margin: 0;
  padding: 0;
}

#sonondxqas .gt_table {
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

#sonondxqas .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#sonondxqas .gt_title {
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

#sonondxqas .gt_subtitle {
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

#sonondxqas .gt_heading {
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

#sonondxqas .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#sonondxqas .gt_col_headings {
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

#sonondxqas .gt_col_heading {
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

#sonondxqas .gt_column_spanner_outer {
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

#sonondxqas .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#sonondxqas .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#sonondxqas .gt_column_spanner {
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

#sonondxqas .gt_spanner_row {
  border-bottom-style: hidden;
}

#sonondxqas .gt_group_heading {
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

#sonondxqas .gt_empty_group_heading {
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

#sonondxqas .gt_from_md > :first-child {
  margin-top: 0;
}

#sonondxqas .gt_from_md > :last-child {
  margin-bottom: 0;
}

#sonondxqas .gt_row {
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

#sonondxqas .gt_stub {
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

#sonondxqas .gt_stub_row_group {
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

#sonondxqas .gt_row_group_first td {
  border-top-width: 2px;
}

#sonondxqas .gt_row_group_first th {
  border-top-width: 2px;
}

#sonondxqas .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#sonondxqas .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#sonondxqas .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#sonondxqas .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#sonondxqas .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#sonondxqas .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#sonondxqas .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#sonondxqas .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#sonondxqas .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#sonondxqas .gt_footnotes {
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

#sonondxqas .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#sonondxqas .gt_sourcenotes {
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

#sonondxqas .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#sonondxqas .gt_left {
  text-align: left;
}

#sonondxqas .gt_center {
  text-align: center;
}

#sonondxqas .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#sonondxqas .gt_font_normal {
  font-weight: normal;
}

#sonondxqas .gt_font_bold {
  font-weight: bold;
}

#sonondxqas .gt_font_italic {
  font-style: italic;
}

#sonondxqas .gt_super {
  font-size: 65%;
}

#sonondxqas .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#sonondxqas .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#sonondxqas .gt_indent_1 {
  text-indent: 5px;
}

#sonondxqas .gt_indent_2 {
  text-indent: 10px;
}

#sonondxqas .gt_indent_3 {
  text-indent: 15px;
}

#sonondxqas .gt_indent_4 {
  text-indent: 20px;
}

#sonondxqas .gt_indent_5 {
  text-indent: 25px;
}

#sonondxqas .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#sonondxqas div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="N" class="gt_row gt_right">234</td>
<td headers="Percent" class="gt_row gt_right">100%</td></tr>
    <tr><td headers="Criterion" class="gt_row gt_left">Drop KN outliers</td>
<td headers="N" class="gt_row gt_right">0</td>
<td headers="Percent" class="gt_row gt_right">0%</td></tr>
    <tr><td headers="Criterion" class="gt_row gt_left">Drop top-10% SDR</td>
<td headers="N" class="gt_row gt_right">25</td>
<td headers="Percent" class="gt_row gt_right">11%</td></tr>
    <tr><td headers="Criterion" class="gt_row gt_left">Kept (both rules)</td>
<td headers="N" class="gt_row gt_right">209</td>
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
<div id="nimheosvqu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#nimheosvqu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#nimheosvqu thead, #nimheosvqu tbody, #nimheosvqu tfoot, #nimheosvqu tr, #nimheosvqu td, #nimheosvqu th {
  border-style: none;
}

#nimheosvqu p {
  margin: 0;
  padding: 0;
}

#nimheosvqu .gt_table {
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

#nimheosvqu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#nimheosvqu .gt_title {
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

#nimheosvqu .gt_subtitle {
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

#nimheosvqu .gt_heading {
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

#nimheosvqu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nimheosvqu .gt_col_headings {
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

#nimheosvqu .gt_col_heading {
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

#nimheosvqu .gt_column_spanner_outer {
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

#nimheosvqu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#nimheosvqu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#nimheosvqu .gt_column_spanner {
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

#nimheosvqu .gt_spanner_row {
  border-bottom-style: hidden;
}

#nimheosvqu .gt_group_heading {
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

#nimheosvqu .gt_empty_group_heading {
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

#nimheosvqu .gt_from_md > :first-child {
  margin-top: 0;
}

#nimheosvqu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#nimheosvqu .gt_row {
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

#nimheosvqu .gt_stub {
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

#nimheosvqu .gt_stub_row_group {
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

#nimheosvqu .gt_row_group_first td {
  border-top-width: 2px;
}

#nimheosvqu .gt_row_group_first th {
  border-top-width: 2px;
}

#nimheosvqu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#nimheosvqu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#nimheosvqu .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#nimheosvqu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nimheosvqu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#nimheosvqu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#nimheosvqu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#nimheosvqu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#nimheosvqu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nimheosvqu .gt_footnotes {
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

#nimheosvqu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#nimheosvqu .gt_sourcenotes {
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

#nimheosvqu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#nimheosvqu .gt_left {
  text-align: left;
}

#nimheosvqu .gt_center {
  text-align: center;
}

#nimheosvqu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#nimheosvqu .gt_font_normal {
  font-weight: normal;
}

#nimheosvqu .gt_font_bold {
  font-weight: bold;
}

#nimheosvqu .gt_font_italic {
  font-style: italic;
}

#nimheosvqu .gt_super {
  font-size: 65%;
}

#nimheosvqu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#nimheosvqu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#nimheosvqu .gt_indent_1 {
  text-indent: 5px;
}

#nimheosvqu .gt_indent_2 {
  text-indent: 10px;
}

#nimheosvqu .gt_indent_3 {
  text-indent: 15px;
}

#nimheosvqu .gt_indent_4 {
  text-indent: 20px;
}

#nimheosvqu .gt_indent_5 {
  text-indent: 25px;
}

#nimheosvqu .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#nimheosvqu div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="estimate" class="gt_row gt_right">2.840</td>
<td headers="conf.low" class="gt_row gt_right">2.688</td>
<td headers="conf.high" class="gt_row gt_right">2.991</td>
<td headers="p.value" class="gt_row gt_right">1.47e-07</td>
<td headers="R2_marg" class="gt_row gt_right">0.106</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.840</td>
<td headers="conf.low" class="gt_row gt_right">2.691</td>
<td headers="conf.high" class="gt_row gt_right">2.990</td>
<td headers="p.value" class="gt_row gt_right">1.69e-07</td>
<td headers="R2_marg" class="gt_row gt_right">0.105</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.842</td>
<td headers="conf.low" class="gt_row gt_right">2.694</td>
<td headers="conf.high" class="gt_row gt_right">2.989</td>
<td headers="p.value" class="gt_row gt_right">1.18e-07</td>
<td headers="R2_marg" class="gt_row gt_right">0.108</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.835</td>
<td headers="conf.low" class="gt_row gt_right">2.682</td>
<td headers="conf.high" class="gt_row gt_right">2.989</td>
<td headers="p.value" class="gt_row gt_right">9.91e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.108</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.824</td>
<td headers="conf.low" class="gt_row gt_right">2.666</td>
<td headers="conf.high" class="gt_row gt_right">2.982</td>
<td headers="p.value" class="gt_row gt_right">9.66e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.105</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.002</td>
<td headers="conf.low" class="gt_row gt_right">-0.083</td>
<td headers="conf.high" class="gt_row gt_right">0.079</td>
<td headers="p.value" class="gt_row gt_right">9.60e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.105</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.002</td>
<td headers="conf.low" class="gt_row gt_right">-0.129</td>
<td headers="conf.high" class="gt_row gt_right">0.125</td>
<td headers="p.value" class="gt_row gt_right">9.76e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.105</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.033</td>
<td headers="conf.low" class="gt_row gt_right">-0.118</td>
<td headers="conf.high" class="gt_row gt_right">0.052</td>
<td headers="p.value" class="gt_row gt_right">4.44e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.108</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.027</td>
<td headers="conf.low" class="gt_row gt_right">-0.054</td>
<td headers="conf.high" class="gt_row gt_right">0.107</td>
<td headers="p.value" class="gt_row gt_right">5.16e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.106</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.036</td>
<td headers="conf.low" class="gt_row gt_right">-0.094</td>
<td headers="conf.high" class="gt_row gt_right">0.165</td>
<td headers="p.value" class="gt_row gt_right">5.45e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.108</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.826</td>
<td headers="conf.low" class="gt_row gt_right">2.686</td>
<td headers="conf.high" class="gt_row gt_right">2.967</td>
<td headers="p.value" class="gt_row gt_right">1.40e-07</td>
<td headers="R2_marg" class="gt_row gt_right">0.125</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.827</td>
<td headers="conf.low" class="gt_row gt_right">2.688</td>
<td headers="conf.high" class="gt_row gt_right">2.966</td>
<td headers="p.value" class="gt_row gt_right">1.39e-07</td>
<td headers="R2_marg" class="gt_row gt_right">0.126</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.828</td>
<td headers="conf.low" class="gt_row gt_right">2.690</td>
<td headers="conf.high" class="gt_row gt_right">2.966</td>
<td headers="p.value" class="gt_row gt_right">9.32e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.129</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.013</td>
<td headers="conf.low" class="gt_row gt_right">-0.083</td>
<td headers="conf.high" class="gt_row gt_right">0.058</td>
<td headers="p.value" class="gt_row gt_right">7.25e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.126</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.032</td>
<td headers="conf.low" class="gt_row gt_right">-0.105</td>
<td headers="conf.high" class="gt_row gt_right">0.042</td>
<td headers="p.value" class="gt_row gt_right">3.93e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.129</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.007</td>
<td headers="conf.low" class="gt_row gt_right">-0.063</td>
<td headers="conf.high" class="gt_row gt_right">0.077</td>
<td headers="p.value" class="gt_row gt_right">8.47e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.125</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.819</td>
<td headers="conf.low" class="gt_row gt_right">2.668</td>
<td headers="conf.high" class="gt_row gt_right">2.970</td>
<td headers="p.value" class="gt_row gt_right">7.81e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.131</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.820</td>
<td headers="conf.low" class="gt_row gt_right">2.671</td>
<td headers="conf.high" class="gt_row gt_right">2.968</td>
<td headers="p.value" class="gt_row gt_right">8.91e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.132</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.820</td>
<td headers="conf.low" class="gt_row gt_right">2.672</td>
<td headers="conf.high" class="gt_row gt_right">2.968</td>
<td headers="p.value" class="gt_row gt_right">7.27e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.133</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.820</td>
<td headers="conf.low" class="gt_row gt_right">2.660</td>
<td headers="conf.high" class="gt_row gt_right">2.979</td>
<td headers="p.value" class="gt_row gt_right">1.67e-07</td>
<td headers="R2_marg" class="gt_row gt_right">0.131</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.007</td>
<td headers="conf.low" class="gt_row gt_right">-0.076</td>
<td headers="conf.high" class="gt_row gt_right">0.062</td>
<td headers="p.value" class="gt_row gt_right">8.47e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.132</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.018</td>
<td headers="conf.low" class="gt_row gt_right">-0.090</td>
<td headers="conf.high" class="gt_row gt_right">0.054</td>
<td headers="p.value" class="gt_row gt_right">6.24e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.133</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.016</td>
<td headers="conf.low" class="gt_row gt_right">-0.052</td>
<td headers="conf.high" class="gt_row gt_right">0.085</td>
<td headers="p.value" class="gt_row gt_right">6.42e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.131</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.032</td>
<td headers="conf.low" class="gt_row gt_right">-0.081</td>
<td headers="conf.high" class="gt_row gt_right">0.144</td>
<td headers="p.value" class="gt_row gt_right">5.41e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.131</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.842</td>
<td headers="conf.low" class="gt_row gt_right">2.703</td>
<td headers="conf.high" class="gt_row gt_right">2.981</td>
<td headers="p.value" class="gt_row gt_right">1.04e-07</td>
<td headers="R2_marg" class="gt_row gt_right">0.096</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.843</td>
<td headers="conf.low" class="gt_row gt_right">2.706</td>
<td headers="conf.high" class="gt_row gt_right">2.981</td>
<td headers="p.value" class="gt_row gt_right">1.33e-07</td>
<td headers="R2_marg" class="gt_row gt_right">0.095</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.846</td>
<td headers="conf.low" class="gt_row gt_right">2.712</td>
<td headers="conf.high" class="gt_row gt_right">2.980</td>
<td headers="p.value" class="gt_row gt_right">1.67e-07</td>
<td headers="R2_marg" class="gt_row gt_right">0.097</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.017</td>
<td headers="conf.low" class="gt_row gt_right">-0.060</td>
<td headers="conf.high" class="gt_row gt_right">0.094</td>
<td headers="p.value" class="gt_row gt_right">6.71e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.095</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.026</td>
<td headers="conf.low" class="gt_row gt_right">-0.107</td>
<td headers="conf.high" class="gt_row gt_right">0.054</td>
<td headers="p.value" class="gt_row gt_right">5.22e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.097</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.032</td>
<td headers="conf.low" class="gt_row gt_right">-0.044</td>
<td headers="conf.high" class="gt_row gt_right">0.107</td>
<td headers="p.value" class="gt_row gt_right">4.12e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.096</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.826</td>
<td headers="conf.low" class="gt_row gt_right">2.698</td>
<td headers="conf.high" class="gt_row gt_right">2.954</td>
<td headers="p.value" class="gt_row gt_right">9.96e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.123</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.826</td>
<td headers="conf.low" class="gt_row gt_right">2.700</td>
<td headers="conf.high" class="gt_row gt_right">2.953</td>
<td headers="p.value" class="gt_row gt_right">1.09e-07</td>
<td headers="R2_marg" class="gt_row gt_right">0.123</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.829</td>
<td headers="conf.low" class="gt_row gt_right">2.705</td>
<td headers="conf.high" class="gt_row gt_right">2.952</td>
<td headers="p.value" class="gt_row gt_right">9.66e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.126</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.822</td>
<td headers="conf.low" class="gt_row gt_right">2.687</td>
<td headers="conf.high" class="gt_row gt_right">2.958</td>
<td headers="p.value" class="gt_row gt_right">1.66e-07</td>
<td headers="R2_marg" class="gt_row gt_right">0.126</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.008</td>
<td headers="conf.low" class="gt_row gt_right">-0.059</td>
<td headers="conf.high" class="gt_row gt_right">0.075</td>
<td headers="p.value" class="gt_row gt_right">8.11e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.123</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.027</td>
<td headers="conf.low" class="gt_row gt_right">-0.097</td>
<td headers="conf.high" class="gt_row gt_right">0.043</td>
<td headers="p.value" class="gt_row gt_right">4.45e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.126</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.015</td>
<td headers="conf.low" class="gt_row gt_right">-0.050</td>
<td headers="conf.high" class="gt_row gt_right">0.081</td>
<td headers="p.value" class="gt_row gt_right">6.43e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.123</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.029</td>
<td headers="conf.low" class="gt_row gt_right">-0.075</td>
<td headers="conf.high" class="gt_row gt_right">0.132</td>
<td headers="p.value" class="gt_row gt_right">5.57e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.126</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.822</td>
<td headers="conf.low" class="gt_row gt_right">2.689</td>
<td headers="conf.high" class="gt_row gt_right">2.956</td>
<td headers="p.value" class="gt_row gt_right">1.95e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.128</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.823</td>
<td headers="conf.low" class="gt_row gt_right">2.691</td>
<td headers="conf.high" class="gt_row gt_right">2.955</td>
<td headers="p.value" class="gt_row gt_right">2.20e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.126</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.824</td>
<td headers="conf.low" class="gt_row gt_right">2.695</td>
<td headers="conf.high" class="gt_row gt_right">2.953</td>
<td headers="p.value" class="gt_row gt_right">2.17e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.127</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.016</td>
<td headers="conf.low" class="gt_row gt_right">-0.050</td>
<td headers="conf.high" class="gt_row gt_right">0.081</td>
<td headers="p.value" class="gt_row gt_right">6.40e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.126</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.009</td>
<td headers="conf.low" class="gt_row gt_right">-0.078</td>
<td headers="conf.high" class="gt_row gt_right">0.060</td>
<td headers="p.value" class="gt_row gt_right">7.90e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.127</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.028</td>
<td headers="conf.low" class="gt_row gt_right">-0.037</td>
<td headers="conf.high" class="gt_row gt_right">0.092</td>
<td headers="p.value" class="gt_row gt_right">3.99e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.128</td>
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

    ## Warning: Removed 5 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 5 rows containing missing values or values outside the scale
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
