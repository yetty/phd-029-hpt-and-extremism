# Project Instructions

## Overview

This is a quantitative research project for a **Registered Report (Stage 2)** manuscript titled _"Stress-Testing Historical Perspective Taking: Ideological Attitudes and Disciplinary Reasoning"_ by Juda Kaleta, targeting **Theory & Research in Social Education (TRSE)**.

The study tests whether a Historical Perspective Taking (HPT) instrument used in history education is contaminated by right-authoritarian ideological attitudes. The core question: do students with stronger right-authoritarian views score higher on HPT simply because the instrument conflates ideological agreement with disciplinary reasoning?

## Research Design

### Sample

- Czech secondary school students across 11 schools, multiple classes per school (multilevel structure)
- Data collected via Google Sheets (one per teacher/school)

### Key Constructs & Instruments

- **HPT (9 items)**: Historical Perspective Taking with three subscales:
  - **POP** (Presentism/Othering of People): reverse-scored so higher = more contextualised
  - **CONT** (Contextualisation)
  - **ROA** (Recognition of Author): used in sensitivity analyses only
  - Composite scores: `HPT_CTX6` (POP_rev + CONT), `HPT_TOT9` (POP_rev + CONT + ROA)
- **FR-LF mini (6 items)**: Right-authoritarianism with RD (Right-wing Docility) and NS (Nationalist Sentiment) facets
- **KSA-3 (9 items)**: Authoritarianism (aggression, submission, conventionalism)
- **KN (6 items)**: Historical knowledge (dichotomous correct/incorrect)
- **SDR-5 (5 items)**: Social desirability response bias (items 2-4 reversed)
- **Demographics**: gender, history grade, school metadata

### Hypotheses

- **H1**: Higher right-authoritarian attitudes (FR-LF, KSA) predict higher HPT scores (ideological contamination)
- **H2**: The H1 effect persists after controlling for historical knowledge and social desirability
- **Measurement tests**: DIF and multi-group CFA to detect ideological bias at item level

## Analysis Pipeline

### Data Flow

```
Google Sheets (11 sheets)
    -> pull_and_normalize_data_CONFIDENTAL.R
normalised_responses.RData / .RDS / .xlsx
    -> 01-07 .Rmd files (via Makefile)
outputs/ (PDF, Markdown, LaTeX reports)
```

### Numbered Analysis Reports (Rmd files)

1. **01_measurement-checks.Rmd** - Reliability (alpha, omega), CFA dimensionality, ICCs
2. **02_descriptives-and-zero-order.Rmd** - Distributions, correlations, school/class variation
3. **03_multilevel-models-hypothesis-tests.Rmd** - Main H1/H2 multilevel models (lme4/lmerTest)
4. **04_dif-and-mg-cfa-hpt-bias.Rmd** - DIF analysis and multi-group CFA for ideological contamination
5. **05_sensitivity-analyses.Rmd** - Robustness checks (scoring, operationalisation, exclusions, random slopes)
6. **06_appendix-tables-and-figures.Rmd** - Publication-ready tables and figures
7. **07_reproducibility-report.Rmd** - Session info, data provenance, file map

### Key R Scripts

- `pull_and_normalize_data_CONFIDENTAL.R` - Downloads and cleans raw data from Google Sheets
- `compute_instruments_validity.R` - Computes reliability coefficients for all instruments
- `factor_and_invariance.R` - Factor structure and measurement invariance tests
- `H1_ideology_elevates_HPT.R` - Quick H1 test without controls
- `multilevel_and_DIF.R` - Full multilevel models and DIF analysis
- `teacher_report.Rmd` - Parametrised individual teacher feedback reports

### Build System (Makefile)

- `make all` - Render all main reports (01-05) to PDF/MD/TeX
- `make codebook` - Build data codebook PDF from LaTeX
- `make teacher SCHOOL_ID="S10" TEACHER_NAME="Name"` - Build individual teacher report
- Each Rmd renders to PDF, Markdown, and LaTeX simultaneously

## Coding Conventions

- **Language**: R (tidyverse style), R Markdown for reports, LaTeX for codebook
- **Statistical packages**: lme4/lmerTest (multilevel models), lavaan (CFA), difR/lordif/mirt (DIF), psych (reliability)
- **Data format**: All Rmd files load `normalised_responses.RData` at the start
- **Variable naming**: z-standardised predictors suffixed `_z` (e.g., `FR_z`, `KSA_z`, `KN_z`, `SDR_z`)
- **HPT scoring**: POP items are reverse-scored (`5 - POP`) so that higher values always mean more contextualised/disciplinary reasoning
- **Multilevel structure**: Random intercept by class (students nested in classes nested in schools)
- **Output directory**: All generated reports go to `outputs/`

## Important Notes

- Files containing `CONFIDENTAL` or `CONFIDENTIAL` in the name are gitignored and must not be committed.
- The `.gitignore` also excludes `.gdoc`, `.gform`, and `.DS_Store` files.
- Teacher reports are confidential and should never be shared or committed.
- The manuscript follows APA 7th edition formatting conventions.
- The target journal is TRSE which uses a specific manuscript structure; the manuscript is written in American English.
