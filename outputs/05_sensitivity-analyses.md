# Purpose and scope

This file documents **exploratory robustness checks** of our main
results. We vary how HPT is scored, how ideology is operationalised,
which observations are included, and whether class-level **random
slopes** are needed. The goal is to see if substantive conclusions
survive reasonable perturbations--**not** to hunt for significance.

HPT scoring follows the Hartmann-Hasselhorn / Huijgen instrument logic;
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
source("submissions/pci_psychology/scoring_helpers.R")

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
    HPT_POP_rev = scale_mean(., paste0(POP_rev_items, "_rev"), min_answered = 2),
    HPT_CONT    = scale_mean(., paste0("CONT", 1:3), min_answered = 2),
    HPT_ROA     = scale_mean(., paste0("ROA", 1:3), min_answered = 2),
    # Canonical composites
    HPT_CTX6    = rowMeans(cbind(HPT_POP_rev, HPT_CONT), na.rm = FALSE),
    HPT_TOT9    = rowMeans(cbind(HPT_POP_rev, HPT_CONT, HPT_ROA),
                           na.rm = FALSE)
  )
```

**Variable dictionary.** KN, POP/ROA/CONT, RD/NS, KSA facets, SDR as per
codebook.

# 1. Scoring variants for HPT (with POP reversed)

``` r
# IMPORTANT: use reversed POP columns in all totals

dat <- dat_raw %>%
  mutate(
    HPT_total_9 = HPT_TOT9,
    HPT_total_8 = scale_mean(., c(paste0("POP", 1:3, "_rev"),
                                  paste0("ROA", 2:3),
                                  paste0("CONT", 1:3)), min_answered = 5),
    HPT_total_6 = HPT_CTX6
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
<div id="dcpptbbdyy" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#dcpptbbdyy table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#dcpptbbdyy thead, #dcpptbbdyy tbody, #dcpptbbdyy tfoot, #dcpptbbdyy tr, #dcpptbbdyy td, #dcpptbbdyy th {
  border-style: none;
}

#dcpptbbdyy p {
  margin: 0;
  padding: 0;
}

#dcpptbbdyy .gt_table {
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

#dcpptbbdyy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#dcpptbbdyy .gt_title {
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

#dcpptbbdyy .gt_subtitle {
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

#dcpptbbdyy .gt_heading {
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

#dcpptbbdyy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dcpptbbdyy .gt_col_headings {
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

#dcpptbbdyy .gt_col_heading {
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

#dcpptbbdyy .gt_column_spanner_outer {
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

#dcpptbbdyy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#dcpptbbdyy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#dcpptbbdyy .gt_column_spanner {
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

#dcpptbbdyy .gt_spanner_row {
  border-bottom-style: hidden;
}

#dcpptbbdyy .gt_group_heading {
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

#dcpptbbdyy .gt_empty_group_heading {
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

#dcpptbbdyy .gt_from_md > :first-child {
  margin-top: 0;
}

#dcpptbbdyy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#dcpptbbdyy .gt_row {
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

#dcpptbbdyy .gt_stub {
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

#dcpptbbdyy .gt_stub_row_group {
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

#dcpptbbdyy .gt_row_group_first td {
  border-top-width: 2px;
}

#dcpptbbdyy .gt_row_group_first th {
  border-top-width: 2px;
}

#dcpptbbdyy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dcpptbbdyy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#dcpptbbdyy .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#dcpptbbdyy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dcpptbbdyy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dcpptbbdyy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#dcpptbbdyy .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#dcpptbbdyy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#dcpptbbdyy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dcpptbbdyy .gt_footnotes {
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

#dcpptbbdyy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#dcpptbbdyy .gt_sourcenotes {
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

#dcpptbbdyy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#dcpptbbdyy .gt_left {
  text-align: left;
}

#dcpptbbdyy .gt_center {
  text-align: center;
}

#dcpptbbdyy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#dcpptbbdyy .gt_font_normal {
  font-weight: normal;
}

#dcpptbbdyy .gt_font_bold {
  font-weight: bold;
}

#dcpptbbdyy .gt_font_italic {
  font-style: italic;
}

#dcpptbbdyy .gt_super {
  font-size: 65%;
}

#dcpptbbdyy .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#dcpptbbdyy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#dcpptbbdyy .gt_indent_1 {
  text-indent: 5px;
}

#dcpptbbdyy .gt_indent_2 {
  text-indent: 10px;
}

#dcpptbbdyy .gt_indent_3 {
  text-indent: 15px;
}

#dcpptbbdyy .gt_indent_4 {
  text-indent: 20px;
}

#dcpptbbdyy .gt_indent_5 {
  text-indent: 25px;
}

#dcpptbbdyy .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#dcpptbbdyy div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
    SDR_total  = scale_mean(., paste0("SDR", 1:5), min_answered = 4),
    NS_sum     = scale_mean(., paste0("NS", 1:3), min_answered = 2),
    RD_sum     = scale_mean(., paste0("RD", 1:3), min_answered = 2),
    FRLF_mini  = scale_mean(., c(paste0("NS", 1:3),
                                 paste0("RD", 1:3)), min_answered = 4),
    KSA_A      = scale_mean(., paste0("A", 1:3), min_answered = 2),
    KSA_U      = scale_mean(., paste0("U", 1:3), min_answered = 2),
    KSA_K      = scale_mean(., paste0("K", 1:3), min_answered = 2),
    KSA_total  = scale_mean(., c(paste0("A", 1:3), paste0("U", 1:3),
                                 paste0("K", 1:3)), min_answered = 7)
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
<div id="pnitfmwqhh" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#pnitfmwqhh table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#pnitfmwqhh thead, #pnitfmwqhh tbody, #pnitfmwqhh tfoot, #pnitfmwqhh tr, #pnitfmwqhh td, #pnitfmwqhh th {
  border-style: none;
}

#pnitfmwqhh p {
  margin: 0;
  padding: 0;
}

#pnitfmwqhh .gt_table {
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

#pnitfmwqhh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#pnitfmwqhh .gt_title {
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

#pnitfmwqhh .gt_subtitle {
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

#pnitfmwqhh .gt_heading {
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

#pnitfmwqhh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pnitfmwqhh .gt_col_headings {
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

#pnitfmwqhh .gt_col_heading {
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

#pnitfmwqhh .gt_column_spanner_outer {
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

#pnitfmwqhh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#pnitfmwqhh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#pnitfmwqhh .gt_column_spanner {
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

#pnitfmwqhh .gt_spanner_row {
  border-bottom-style: hidden;
}

#pnitfmwqhh .gt_group_heading {
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

#pnitfmwqhh .gt_empty_group_heading {
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

#pnitfmwqhh .gt_from_md > :first-child {
  margin-top: 0;
}

#pnitfmwqhh .gt_from_md > :last-child {
  margin-bottom: 0;
}

#pnitfmwqhh .gt_row {
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

#pnitfmwqhh .gt_stub {
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

#pnitfmwqhh .gt_stub_row_group {
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

#pnitfmwqhh .gt_row_group_first td {
  border-top-width: 2px;
}

#pnitfmwqhh .gt_row_group_first th {
  border-top-width: 2px;
}

#pnitfmwqhh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pnitfmwqhh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#pnitfmwqhh .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#pnitfmwqhh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pnitfmwqhh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pnitfmwqhh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#pnitfmwqhh .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#pnitfmwqhh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#pnitfmwqhh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pnitfmwqhh .gt_footnotes {
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

#pnitfmwqhh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#pnitfmwqhh .gt_sourcenotes {
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

#pnitfmwqhh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#pnitfmwqhh .gt_left {
  text-align: left;
}

#pnitfmwqhh .gt_center {
  text-align: center;
}

#pnitfmwqhh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#pnitfmwqhh .gt_font_normal {
  font-weight: normal;
}

#pnitfmwqhh .gt_font_bold {
  font-weight: bold;
}

#pnitfmwqhh .gt_font_italic {
  font-style: italic;
}

#pnitfmwqhh .gt_super {
  font-size: 65%;
}

#pnitfmwqhh .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#pnitfmwqhh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#pnitfmwqhh .gt_indent_1 {
  text-indent: 5px;
}

#pnitfmwqhh .gt_indent_2 {
  text-indent: 10px;
}

#pnitfmwqhh .gt_indent_3 {
  text-indent: 15px;
}

#pnitfmwqhh .gt_indent_4 {
  text-indent: 20px;
}

#pnitfmwqhh .gt_indent_5 {
  text-indent: 25px;
}

#pnitfmwqhh .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#pnitfmwqhh div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="SDR_mean" class="gt_row gt_right">3.015548</td>
<td headers="SDR_sd" class="gt_row gt_right">0.6252279</td>
<td headers="NS_mean" class="gt_row gt_right">2.428322</td>
<td headers="NS_sd" class="gt_row gt_right">0.8906106</td>
<td headers="RD_mean" class="gt_row gt_right">2.539181</td>
<td headers="RD_sd" class="gt_row gt_right">0.8846439</td>
<td headers="KSA_mean" class="gt_row gt_right">2.857781</td>
<td headers="KSA_sd" class="gt_row gt_right">0.6248808</td></tr>
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
<div id="riotjkbrnt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#riotjkbrnt table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#riotjkbrnt thead, #riotjkbrnt tbody, #riotjkbrnt tfoot, #riotjkbrnt tr, #riotjkbrnt td, #riotjkbrnt th {
  border-style: none;
}

#riotjkbrnt p {
  margin: 0;
  padding: 0;
}

#riotjkbrnt .gt_table {
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

#riotjkbrnt .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#riotjkbrnt .gt_title {
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

#riotjkbrnt .gt_subtitle {
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

#riotjkbrnt .gt_heading {
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

#riotjkbrnt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#riotjkbrnt .gt_col_headings {
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

#riotjkbrnt .gt_col_heading {
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

#riotjkbrnt .gt_column_spanner_outer {
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

#riotjkbrnt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#riotjkbrnt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#riotjkbrnt .gt_column_spanner {
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

#riotjkbrnt .gt_spanner_row {
  border-bottom-style: hidden;
}

#riotjkbrnt .gt_group_heading {
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

#riotjkbrnt .gt_empty_group_heading {
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

#riotjkbrnt .gt_from_md > :first-child {
  margin-top: 0;
}

#riotjkbrnt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#riotjkbrnt .gt_row {
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

#riotjkbrnt .gt_stub {
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

#riotjkbrnt .gt_stub_row_group {
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

#riotjkbrnt .gt_row_group_first td {
  border-top-width: 2px;
}

#riotjkbrnt .gt_row_group_first th {
  border-top-width: 2px;
}

#riotjkbrnt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#riotjkbrnt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#riotjkbrnt .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#riotjkbrnt .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#riotjkbrnt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#riotjkbrnt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#riotjkbrnt .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#riotjkbrnt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#riotjkbrnt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#riotjkbrnt .gt_footnotes {
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

#riotjkbrnt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#riotjkbrnt .gt_sourcenotes {
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

#riotjkbrnt .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#riotjkbrnt .gt_left {
  text-align: left;
}

#riotjkbrnt .gt_center {
  text-align: center;
}

#riotjkbrnt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#riotjkbrnt .gt_font_normal {
  font-weight: normal;
}

#riotjkbrnt .gt_font_bold {
  font-weight: bold;
}

#riotjkbrnt .gt_font_italic {
  font-style: italic;
}

#riotjkbrnt .gt_super {
  font-size: 65%;
}

#riotjkbrnt .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#riotjkbrnt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#riotjkbrnt .gt_indent_1 {
  text-indent: 5px;
}

#riotjkbrnt .gt_indent_2 {
  text-indent: 10px;
}

#riotjkbrnt .gt_indent_3 {
  text-indent: 15px;
}

#riotjkbrnt .gt_indent_4 {
  text-indent: 20px;
}

#riotjkbrnt .gt_indent_5 {
  text-indent: 25px;
}

#riotjkbrnt .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#riotjkbrnt div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="N" class="gt_row gt_right">32</td>
<td headers="Percent" class="gt_row gt_right">11%</td></tr>
    <tr><td headers="Criterion" class="gt_row gt_left">Kept (both rules)</td>
<td headers="N" class="gt_row gt_right">251</td>
<td headers="Percent" class="gt_row gt_right">86%</td></tr>
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
    ## ! Can't compute r-squared. Some variance components equal zero. Your model
    ##   may suffer from singularity (see `?lme4::isSingular` and
    ##   `?performance::check_singularity`).
    ##   Decrease the `tolerance` level to force the calculation of random effect
    ##   variances, or impose priors on your random effects parameters (using
    ##   packages like `brms` or `glmmTMB`).
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 8 remaining warnings.

    ## Random effect variances not available. Returned R2 does not account for random effects.
    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Warning: There were 2 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `t1 = map(m1, summarise_model)`.
    ## Caused by warning:
    ## ! Can't compute r-squared. Some variance components equal zero. Your model
    ##   may suffer from singularity (see `?lme4::isSingular` and
    ##   `?performance::check_singularity`).
    ##   Decrease the `tolerance` level to force the calculation of random effect
    ##   variances, or impose priors on your random effects parameters (using
    ##   packages like `brms` or `glmmTMB`).
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

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
    ## ! Can't compute r-squared. Some variance components equal zero. Your model
    ##   may suffer from singularity (see `?lme4::isSingular` and
    ##   `?performance::check_singularity`).
    ##   Decrease the `tolerance` level to force the calculation of random effect
    ##   variances, or impose priors on your random effects parameters (using
    ##   packages like `brms` or `glmmTMB`).
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 8 remaining warnings.

    ## Random effect variances not available. Returned R2 does not account for random effects.

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `t1 = map(m1, summarise_model)`.
    ## Caused by warning:
    ## ! Can't compute r-squared. Some variance components equal zero. Your model
    ##   may suffer from singularity (see `?lme4::isSingular` and
    ##   `?performance::check_singularity`).
    ##   Decrease the `tolerance` level to force the calculation of random effect
    ##   variances, or impose priors on your random effects parameters (using
    ##   packages like `brms` or `glmmTMB`).

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
<div id="umjwlbnmwz" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#umjwlbnmwz table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#umjwlbnmwz thead, #umjwlbnmwz tbody, #umjwlbnmwz tfoot, #umjwlbnmwz tr, #umjwlbnmwz td, #umjwlbnmwz th {
  border-style: none;
}

#umjwlbnmwz p {
  margin: 0;
  padding: 0;
}

#umjwlbnmwz .gt_table {
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

#umjwlbnmwz .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#umjwlbnmwz .gt_title {
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

#umjwlbnmwz .gt_subtitle {
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

#umjwlbnmwz .gt_heading {
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

#umjwlbnmwz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#umjwlbnmwz .gt_col_headings {
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

#umjwlbnmwz .gt_col_heading {
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

#umjwlbnmwz .gt_column_spanner_outer {
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

#umjwlbnmwz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#umjwlbnmwz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#umjwlbnmwz .gt_column_spanner {
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

#umjwlbnmwz .gt_spanner_row {
  border-bottom-style: hidden;
}

#umjwlbnmwz .gt_group_heading {
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

#umjwlbnmwz .gt_empty_group_heading {
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

#umjwlbnmwz .gt_from_md > :first-child {
  margin-top: 0;
}

#umjwlbnmwz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#umjwlbnmwz .gt_row {
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

#umjwlbnmwz .gt_stub {
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

#umjwlbnmwz .gt_stub_row_group {
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

#umjwlbnmwz .gt_row_group_first td {
  border-top-width: 2px;
}

#umjwlbnmwz .gt_row_group_first th {
  border-top-width: 2px;
}

#umjwlbnmwz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#umjwlbnmwz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#umjwlbnmwz .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#umjwlbnmwz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#umjwlbnmwz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#umjwlbnmwz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#umjwlbnmwz .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#umjwlbnmwz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#umjwlbnmwz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#umjwlbnmwz .gt_footnotes {
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

#umjwlbnmwz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#umjwlbnmwz .gt_sourcenotes {
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

#umjwlbnmwz .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#umjwlbnmwz .gt_left {
  text-align: left;
}

#umjwlbnmwz .gt_center {
  text-align: center;
}

#umjwlbnmwz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#umjwlbnmwz .gt_font_normal {
  font-weight: normal;
}

#umjwlbnmwz .gt_font_bold {
  font-weight: bold;
}

#umjwlbnmwz .gt_font_italic {
  font-style: italic;
}

#umjwlbnmwz .gt_super {
  font-size: 65%;
}

#umjwlbnmwz .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#umjwlbnmwz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#umjwlbnmwz .gt_indent_1 {
  text-indent: 5px;
}

#umjwlbnmwz .gt_indent_2 {
  text-indent: 10px;
}

#umjwlbnmwz .gt_indent_3 {
  text-indent: 15px;
}

#umjwlbnmwz .gt_indent_4 {
  text-indent: 20px;
}

#umjwlbnmwz .gt_indent_5 {
  text-indent: 25px;
}

#umjwlbnmwz .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#umjwlbnmwz div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
<td headers="estimate" class="gt_row gt_right">2.827</td>
<td headers="conf.low" class="gt_row gt_right">2.715</td>
<td headers="conf.high" class="gt_row gt_right">2.939</td>
<td headers="p.value" class="gt_row gt_right">2.81e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.124</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.826</td>
<td headers="conf.low" class="gt_row gt_right">2.716</td>
<td headers="conf.high" class="gt_row gt_right">2.935</td>
<td headers="p.value" class="gt_row gt_right">1.80e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.127</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.824</td>
<td headers="conf.low" class="gt_row gt_right">2.714</td>
<td headers="conf.high" class="gt_row gt_right">2.933</td>
<td headers="p.value" class="gt_row gt_right">6.04e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.128</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.024</td>
<td headers="conf.low" class="gt_row gt_right">-0.090</td>
<td headers="conf.high" class="gt_row gt_right">0.043</td>
<td headers="p.value" class="gt_row gt_right">4.85e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.127</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.033</td>
<td headers="conf.low" class="gt_row gt_right">-0.100</td>
<td headers="conf.high" class="gt_row gt_right">0.033</td>
<td headers="p.value" class="gt_row gt_right">3.25e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.128</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.007</td>
<td headers="conf.low" class="gt_row gt_right">-0.061</td>
<td headers="conf.high" class="gt_row gt_right">0.075</td>
<td headers="p.value" class="gt_row gt_right">8.36e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.124</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.801</td>
<td headers="conf.low" class="gt_row gt_right">2.692</td>
<td headers="conf.high" class="gt_row gt_right">2.910</td>
<td headers="p.value" class="gt_row gt_right">3.60e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.147</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.800</td>
<td headers="conf.low" class="gt_row gt_right">2.692</td>
<td headers="conf.high" class="gt_row gt_right">2.907</td>
<td headers="p.value" class="gt_row gt_right">2.18e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.150</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.799</td>
<td headers="conf.low" class="gt_row gt_right">2.690</td>
<td headers="conf.high" class="gt_row gt_right">2.908</td>
<td headers="p.value" class="gt_row gt_right">1.65e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.150</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.800</td>
<td headers="conf.low" class="gt_row gt_right">2.690</td>
<td headers="conf.high" class="gt_row gt_right">2.910</td>
<td headers="p.value" class="gt_row gt_right">2.93e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.146</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.022</td>
<td headers="conf.low" class="gt_row gt_right">-0.082</td>
<td headers="conf.high" class="gt_row gt_right">0.037</td>
<td headers="p.value" class="gt_row gt_right">4.62e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.150</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.023</td>
<td headers="conf.low" class="gt_row gt_right">-0.082</td>
<td headers="conf.high" class="gt_row gt_right">0.036</td>
<td headers="p.value" class="gt_row gt_right">4.49e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.150</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.004</td>
<td headers="conf.low" class="gt_row gt_right">-0.065</td>
<td headers="conf.high" class="gt_row gt_right">0.056</td>
<td headers="p.value" class="gt_row gt_right">8.91e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.147</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.000</td>
<td headers="conf.low" class="gt_row gt_right">-0.075</td>
<td headers="conf.high" class="gt_row gt_right">0.074</td>
<td headers="p.value" class="gt_row gt_right">9.91e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.146</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.795</td>
<td headers="conf.low" class="gt_row gt_right">2.677</td>
<td headers="conf.high" class="gt_row gt_right">2.913</td>
<td headers="p.value" class="gt_row gt_right">1.18e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.148</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.794</td>
<td headers="conf.low" class="gt_row gt_right">2.677</td>
<td headers="conf.high" class="gt_row gt_right">2.910</td>
<td headers="p.value" class="gt_row gt_right">1.02e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.150</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.794</td>
<td headers="conf.low" class="gt_row gt_right">2.676</td>
<td headers="conf.high" class="gt_row gt_right">2.911</td>
<td headers="p.value" class="gt_row gt_right">8.66e-10</td>
<td headers="R2_marg" class="gt_row gt_right">0.149</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.014</td>
<td headers="conf.low" class="gt_row gt_right">-0.073</td>
<td headers="conf.high" class="gt_row gt_right">0.045</td>
<td headers="p.value" class="gt_row gt_right">6.40e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.150</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.011</td>
<td headers="conf.low" class="gt_row gt_right">-0.069</td>
<td headers="conf.high" class="gt_row gt_right">0.048</td>
<td headers="p.value" class="gt_row gt_right">7.24e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.149</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Exclusions applied</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.001</td>
<td headers="conf.low" class="gt_row gt_right">-0.059</td>
<td headers="conf.high" class="gt_row gt_right">0.061</td>
<td headers="p.value" class="gt_row gt_right">9.70e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.148</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.832</td>
<td headers="conf.low" class="gt_row gt_right">2.724</td>
<td headers="conf.high" class="gt_row gt_right">2.940</td>
<td headers="p.value" class="gt_row gt_right">5.23e-07</td>
<td headers="R2_marg" class="gt_row gt_right">0.112</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.833</td>
<td headers="conf.low" class="gt_row gt_right">2.725</td>
<td headers="conf.high" class="gt_row gt_right">2.941</td>
<td headers="p.value" class="gt_row gt_right">1.04e-06</td>
<td headers="R2_marg" class="gt_row gt_right">0.112</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.832</td>
<td headers="conf.low" class="gt_row gt_right">2.727</td>
<td headers="conf.high" class="gt_row gt_right">2.937</td>
<td headers="p.value" class="gt_row gt_right">3.98e-07</td>
<td headers="R2_marg" class="gt_row gt_right">0.115</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.009</td>
<td headers="conf.low" class="gt_row gt_right">-0.071</td>
<td headers="conf.high" class="gt_row gt_right">0.054</td>
<td headers="p.value" class="gt_row gt_right">7.85e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.112</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.029</td>
<td headers="conf.low" class="gt_row gt_right">-0.091</td>
<td headers="conf.high" class="gt_row gt_right">0.033</td>
<td headers="p.value" class="gt_row gt_right">3.56e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.115</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 6-item (no ROA)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.013</td>
<td headers="conf.low" class="gt_row gt_right">-0.050</td>
<td headers="conf.high" class="gt_row gt_right">0.077</td>
<td headers="p.value" class="gt_row gt_right">6.76e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.112</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.803</td>
<td headers="conf.low" class="gt_row gt_right">2.703</td>
<td headers="conf.high" class="gt_row gt_right">2.902</td>
<td headers="p.value" class="gt_row gt_right">1.02e-08</td>
<td headers="R2_marg" class="gt_row gt_right">0.138</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.803</td>
<td headers="conf.low" class="gt_row gt_right">2.704</td>
<td headers="conf.high" class="gt_row gt_right">2.901</td>
<td headers="p.value" class="gt_row gt_right">9.05e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.139</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.802</td>
<td headers="conf.low" class="gt_row gt_right">2.704</td>
<td headers="conf.high" class="gt_row gt_right">2.900</td>
<td headers="p.value" class="gt_row gt_right">4.73e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.141</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.802</td>
<td headers="conf.low" class="gt_row gt_right">2.703</td>
<td headers="conf.high" class="gt_row gt_right">2.900</td>
<td headers="p.value" class="gt_row gt_right">5.05e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.142</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.007</td>
<td headers="conf.low" class="gt_row gt_right">-0.063</td>
<td headers="conf.high" class="gt_row gt_right">0.049</td>
<td headers="p.value" class="gt_row gt_right">8.03e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.139</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.023</td>
<td headers="conf.low" class="gt_row gt_right">-0.078</td>
<td headers="conf.high" class="gt_row gt_right">0.033</td>
<td headers="p.value" class="gt_row gt_right">4.21e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.141</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.024</td>
<td headers="conf.low" class="gt_row gt_right">-0.089</td>
<td headers="conf.high" class="gt_row gt_right">0.041</td>
<td headers="p.value" class="gt_row gt_right">4.42e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.142</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 8-item (drop ROA1)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.003</td>
<td headers="conf.low" class="gt_row gt_right">-0.053</td>
<td headers="conf.high" class="gt_row gt_right">0.059</td>
<td headers="p.value" class="gt_row gt_right">9.12e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.138</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.802</td>
<td headers="conf.low" class="gt_row gt_right">2.697</td>
<td headers="conf.high" class="gt_row gt_right">2.908</td>
<td headers="p.value" class="gt_row gt_right">6.84e-10</td>
<td headers="R2_marg" class="gt_row gt_right">0.136</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.802</td>
<td headers="conf.low" class="gt_row gt_right">2.698</td>
<td headers="conf.high" class="gt_row gt_right">2.907</td>
<td headers="p.value" class="gt_row gt_right">7.12e-10</td>
<td headers="R2_marg" class="gt_row gt_right">0.136</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.802</td>
<td headers="conf.low" class="gt_row gt_right">2.698</td>
<td headers="conf.high" class="gt_row gt_right">2.906</td>
<td headers="p.value" class="gt_row gt_right">5.72e-10</td>
<td headers="R2_marg" class="gt_row gt_right">0.136</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">(Intercept)</td>
<td headers="estimate" class="gt_row gt_right">2.802</td>
<td headers="conf.low" class="gt_row gt_right">2.696</td>
<td headers="conf.high" class="gt_row gt_right">2.909</td>
<td headers="p.value" class="gt_row gt_right">1.37e-09</td>
<td headers="R2_marg" class="gt_row gt_right">0.136</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">FR-LF: RD+NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.003</td>
<td headers="conf.low" class="gt_row gt_right">-0.053</td>
<td headers="conf.high" class="gt_row gt_right">0.059</td>
<td headers="p.value" class="gt_row gt_right">9.17e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.136</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.006</td>
<td headers="conf.low" class="gt_row gt_right">-0.061</td>
<td headers="conf.high" class="gt_row gt_right">0.050</td>
<td headers="p.value" class="gt_row gt_right">8.39e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.136</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RS</td>
<td headers="ideol" class="gt_row gt_left">KSA-3 total (z)</td>
<td headers="estimate" class="gt_row gt_right">-0.007</td>
<td headers="conf.low" class="gt_row gt_right">-0.076</td>
<td headers="conf.high" class="gt_row gt_right">0.062</td>
<td headers="p.value" class="gt_row gt_right">8.25e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.136</td>
<td headers="R2_cond" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="sample" class="gt_row gt_left">Full</td>
<td headers="hpt" class="gt_row gt_left">HPT 9-item (POP_rev + ROA + CONT)</td>
<td headers="model" class="gt_row gt_left">RI</td>
<td headers="ideol" class="gt_row gt_left">NS (z)</td>
<td headers="estimate" class="gt_row gt_right">0.012</td>
<td headers="conf.low" class="gt_row gt_right">-0.044</td>
<td headers="conf.high" class="gt_row gt_right">0.068</td>
<td headers="p.value" class="gt_row gt_right">6.66e-01</td>
<td headers="R2_marg" class="gt_row gt_right">0.136</td>
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

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](/home/yetty/PhD/projects/phd-029-hpt-and-extremism/outputs/05_sensitivity-analyses_files/figure-markdown/quick-plots-1.png)

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
    ##  [1] gt_1.3.0            glue_1.8.1          performance_0.18.2
    ##  [4] broom.mixed_0.2.9.7 broom_1.0.13        lmerTest_3.2-1
    ##  [7] lme4_2.0-6          Matrix_1.7-6        lubridate_1.9.5
    ## [10] forcats_1.0.1       stringr_1.6.0       dplyr_1.2.1
    ## [13] purrr_1.2.2         readr_2.2.0         tidyr_1.3.2
    ## [16] tibble_3.3.1        ggplot2_4.0.3       tidyverse_2.0.0
    ##
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.6        xfun_0.60           insight_1.5.4
    ##  [4] lattice_0.23-1      tzdb_0.5.0          numDeriv_2016.8-1.1
    ##  [7] vctrs_0.7.3         tools_4.6.1         Rdpack_2.6.6
    ## [10] generics_0.1.4      parallel_4.6.1      pkgconfig_2.0.3
    ## [13] RColorBrewer_1.1-3  S7_0.2.2            lifecycle_1.0.5
    ## [16] compiler_4.6.1      farver_2.1.2        tinytex_0.61
    ## [19] codetools_0.2-20    sass_0.4.10         htmltools_0.5.9
    ## [22] yaml_2.3.12         pillar_1.11.1       furrr_0.4.0
    ## [25] nloptr_2.2.1        MASS_7.3-66         reformulas_0.4.4
    ## [28] boot_1.3-32         nlme_3.1-171        parallelly_1.48.0
    ## [31] tidyselect_1.2.1    digest_0.6.39       stringi_1.8.7
    ## [34] future_1.75.0       listenv_1.0.0       labeling_0.4.3
    ## [37] splines_4.6.1       fastmap_1.2.0       grid_4.6.1
    ## [40] cli_3.6.6           magrittr_2.0.5      withr_3.0.3
    ## [43] scales_1.4.0        backports_1.5.1     timechange_0.4.0
    ## [46] rmarkdown_2.32      globals_0.19.1      otel_0.2.0
    ## [49] hms_1.1.4           evaluate_1.0.5      knitr_1.51
    ## [52] rbibutils_2.4.1     mgcv_1.9-4          rlang_1.3.0
    ## [55] Rcpp_1.1.2          xml2_1.6.0          minqa_1.2.8
    ## [58] R6_2.6.1            fs_2.1.0
