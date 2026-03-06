# Project Instructions

## Overview

This is a quantitative research project for a manuscript titled _"Stress-Testing Historical Perspective Taking: Ideological Attitudes and Disciplinary Reasoning in Czech Adolescents"_ by Juda Kaleta (Institute of History, Faculty of Arts, Charles University), targeting **Theory & Research in Social Education (TRSE)**.

Hypotheses, instruments, and analysis plan were preregistered on the Open Science Framework (https://osf.io/yng37/).

The study tests whether a Historical Perspective Taking (HPT) instrument used in history education is contaminated by right-authoritarian ideological attitudes. The core question: do students with stronger right-authoritarian views score higher on HPT simply because the instrument conflates ideological agreement with disciplinary reasoning?

## Status Tracking

Two files track project state. **Read both at the start of every session.**

- **`status.md`** — Short current-state snapshot (~30 lines). Contains 3-5 active items per phase and points to the latest plan file. Tells you "where are we right now."
- **`project_status.md`** — Running log (100-200 lines). Tracks overall project state across time. Past completed work is compacted to 1-2 line summaries to stay lean.

**Maintenance rules:**
1. After completing a task, update `status.md` to reflect the new current state.
2. After completing a milestone or phase, update `project_status.md`: move finished items to Completed Milestones (as a compact summary) and update Active Work.
3. Keep `status.md` under ~30 lines and `project_status.md` under ~200 lines.

## Research Design

### Sample

- 293 Czech secondary students (lower-secondary ISCED 2: n=87, 29.7%; upper-secondary ISCED 3: n=206, 70.3%) across 10 schools, 20 classrooms
- Gender: M=192 (65.5%), F=88 (30.0%), O=13 (4.4%)
- School types: public (90.8%), private (6.5%), church-affiliated (2.7%)
- Regions: Praha (42.7%), Královéhradecký (34.8%), Zlínský (11.3%), Jihomoravský (8.2%), Ústecký (3.1%)
- History grade: M=1.71, SD=0.92 (Czech scale: 1=excellent, 5=failing)
- Data collected via Google Sheets (one per teacher/school), de-identified at point of entry

### Key Constructs & Instruments

- **HPT (9 items)**: Historical Perspective Taking with three subscales:
  - **POP** (Presentism/Othering of People): reverse-scored so higher = more contextualised
  - **CONT** (Contextualisation)
  - **ROA** (Recognition of Author): used in sensitivity analyses only
  - Composite scores: `HPT_CTX6` (POP_rev + CONT), `HPT_TOT9` (POP_rev + CONT + ROA)
- **FR-LF mini (6 items)**: Right-authoritarianism with RD (Right-wing Dictatorship) and NS (National Socialist relativization) facets
- **KSA-3 (9 items)**: Authoritarianism (aggression, submission, conventionalism)
- **KN (6 items)**: Historical knowledge (dichotomous correct/incorrect)
- **SDR-5 (5 items)**: Social desirability response bias (items 2-4 reversed)
- **Demographics**: gender, history grade, school metadata

### Hypotheses

- **H1**: Higher right-authoritarian attitudes (FR-LF, KSA) predict higher HPT scores (ideological contamination via congruence pathway)
- **H2**: The H1 effect persists after controlling for historical knowledge and social desirability
- **Measurement tests**: DIF and multi-group CFA to detect ideological bias at item level
- **Result**: Neither hypothesis supported. Null ideology effects across all analyses. Historical knowledge is the only consistent predictor.

## File Structure

### Manuscript

- `trse_outputs/manuscript.md` — Main manuscript (Markdown source)
- `trse_outputs/manuscript.docx` — DOCX output (built via pandoc with T&F template)
- `templates/TF_Template_Word_Windows_2016.dotx` — Taylor & Francis Word template

Build command:
```bash
pandoc trse_outputs/manuscript.md -o trse_outputs/manuscript.docx --reference-doc=templates/TF_Template_Word_Windows_2016.dotx
```

### Manuscript Figures (trse_outputs/)

- `fig02_invariance_and_dif.R` → Figure 1 (MG-CFA loadings + DIF)
- `fig03_score_distributions.R` → Figure 2 (HPT/ideology/knowledge distributions)
- `fig04_coefficient_plot.R` → Figure 3 (multilevel model coefficients)
- `fig05_marginal_effects.R` → Figure 4 (marginal effects)
- `supplementary_analyses.R` — Additional models for appendix
- `tost_and_mundlak.R` — TOST equivalence tests + Mundlak decomposition

Note: Script filenames use the original numbering (fig02–fig05) but manuscript figures are numbered 1–4.

### Data

- `normalised_responses.RData` / `.RDS` / `.xlsx` — De-identified student responses (293 × 52 variables)
- `normalised_responses_codebook.tex` / `.pdf` — Full variable codebook

### Analysis Pipeline

```
Google Sheets (per teacher)
    -> pull_and_normalize_data_CONFIDENTAL.R
normalised_responses.RData / .RDS / .xlsx
    -> 01-07 .Rmd files (via Makefile)
outputs/ (PDF, Markdown, LaTeX reports)
    -> trse_outputs/ scripts (manuscript figures + supplementary analyses)
```

### Numbered Analysis Reports (Rmd files)

1. **01_measurement-checks.Rmd** — Reliability (α, ω), CFA dimensionality, ICCs, floor/ceiling
2. **02_descriptives-and-zero-order.Rmd** — Distributions, correlations, school/class variation
3. **03_multilevel-models-hypothesis-tests.Rmd** — Main H1/H2 multilevel models (lme4/lmerTest)
4. **04_dif-and-mg-cfa-hpt-bias.Rmd** — DIF analysis (mirt) and multi-group CFA (lavaan) for measurement invariance
5. **05_sensitivity-analyses.Rmd** — Robustness checks (scoring, operationalisation, exclusions, random slopes, attenuation correction)
6. **06_appendix-tables-and-figures.Rmd** — Publication-ready appendix tables and figures
7. **07_reproducibility-report.Rmd** — Session info, data provenance, file map

### Exploratory R Scripts

- `compute_instruments_validity.R` — Reliability coefficients for all instruments
- `factor_and_invariance.R` — Factor structure and measurement invariance tests
- `H1_ideology_elevates_HPT.R` — Quick H1 test without controls
- `multilevel_and_DIF.R` — Full multilevel models and DIF analysis
- `teacher_report.Rmd` — Parametrised individual teacher feedback reports (CONFIDENTIAL)

### Build System (Makefile)

- `make all` — Render all main reports (01–05) to PDF/MD/TeX
- `make codebook` — Build data codebook PDF from LaTeX
- Each Rmd renders to PDF, Markdown, and LaTeX simultaneously

### OSF Replication Package

`osf_storage/` contains the public replication package for the OSF project (https://osf.io/yng37/):

```
osf_storage/
├── README.md
├── data/           student_responses.{RDS,xlsx} + codebook.{pdf,tex}
├── scripts/        7 main .Rmd + 4 figure .R + 2 supplementary .R + 4 exploratory .R + Makefile
├── outputs/        6 pre-rendered analysis PDFs
├── figures/        4 manuscript figures (PDF + PNG)
└── instruments/    3 questionnaires (Czech-language .docx)
```

In osf_storage, data files are renamed to `student_responses.*` and scripts use `readRDS("student_responses.RDS")`. The R object inside is still named `normalised_responses`.

### Reviews

`reviews/` contains AI-generated review simulations used during manuscript preparation:
- `chatgpt20260220.md`, `chatgpt_v2.md`, `chatgpt_v3.md`, `chatgpt_v4.md`
- `gemini20260220.md`, `gemini_v2.md`, `gemini_v3.md`

### Checklists

- `checklist.md` — General academic manuscript quality audit (27 categories)
- `trse_checklist.md` — TRSE journal-specific compliance checklist

## Coding Conventions

- **Language**: R (tidyverse style), R Markdown for reports, LaTeX for codebook
- **Statistical packages**: lme4/lmerTest (multilevel models), lavaan (CFA, WLSMV), mirt (DIF, graded response model), psych (reliability)
- **Data format**: Main Rmd files load `normalised_responses.RData`; osf_storage scripts use `readRDS("student_responses.RDS")`
- **Variable naming**: z-standardised predictors suffixed `_z` (e.g., `FR_z`, `KSA_z`, `KN_z`, `SDR_z`)
- **HPT scoring**: POP items are reverse-scored (`5 - POP`) so that higher values always mean more contextualised/disciplinary reasoning
- **Multilevel structure**: Random intercepts for school and class (students nested in classes nested in schools)
- **Output directory**: All generated reports go to `outputs/`
- **Manuscript style**: APA 7th edition, American English, Taylor & Francis template

## Important Notes

- Files containing `CONFIDENTAL` or `CONFIDENTIAL` in the name are gitignored and must not be committed.
- The `.gitignore` also excludes `.gdoc`, `.gform`, and `.DS_Store` files.
- Teacher reports and individual teacher data are confidential and should never be shared or committed.
- The manuscript uses "Institute of History, Faculty of Arts, Charles University" as the author affiliation.
- Figures in the manuscript are numbered 1–4 (not matching the fig02–fig05 script filenames).
- The study was preregistered on OSF (not a Registered Report). Do not use "Stage 1," "Stage 2," or "Registered Report" language.
- The sample includes both lower-secondary (základní školy) and upper-secondary schools. Do not specify the type of upper-secondary schools (do not write "gymnázia").
