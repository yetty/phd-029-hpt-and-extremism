# OSF Replication Package

**Study:** Does Ideological Orientation Contaminate Historical Perspective Taking?
A Boundary-Condition Validation of the Hartmann & Hasselhorn HPT Instrument

**Author:** Juda Kaleta
**Affiliation:** Institute of History, Faculty of Arts, Charles University, Prague
**Preregistration:** https://osf.io/yng37/
**Manuscript target journal:** *Theory & Research in Social Education*

---

## Overview

This repository contains all data, analysis scripts, outputs, and instruments necessary to
reproduce the results reported in the manuscript. The study examines whether students'
right-authoritarian ideological attitudes constitute a source of construct-irrelevant variance
in a widely used Historical Perspective Taking (HPT) instrument (Hartmann & Hasselhorn, 2008),
using a sample of 293 Czech secondary students (20 classrooms, 10 schools).

The analytic approach combines:
- Multilevel models with school and classroom random intercepts (lme4/lmerTest)
- Formal TOST equivalence testing (SESOI = β ±0.20)
- Multi-group confirmatory factor analysis (MG-CFA; lavaan, WLSMV estimator)
- Differential Item Functioning analysis (DIF; mirt, graded response model)
- Mundlak within/between classroom ideology decomposition
- Sensitivity analyses across alternative scoring composites and exclusion rules

---

## Directory Structure

```
osf_storage/
├── README.md                        This file
├── data/                            De-identified student response data
├── scripts/                         All R and R Markdown analysis scripts
├── outputs/                         Pre-rendered PDF analysis reports
├── figures/                         High-resolution manuscript figures
└── instruments/                     Questionnaire and test materials
```

---

## data/

De-identified, processed student responses. No individual names, teacher names, or
school names are included. Schools are identified only by anonymised codes.

| File | Description |
|------|-------------|
| `student_responses.RData` | De-identified data in R's binary format. Load with `load("student_responses.RData")`. Object name: `normalised_responses`. |
| `student_responses.RDS` | Same data as a serialised R object. Load with `dat <- readRDS("student_responses.RDS")`. |
| `student_responses.xlsx` | Same data in Excel format for non-R users. |
| `codebook.pdf` | Full variable codebook: variable names, labels, response scales, scoring procedures, and descriptive statistics for all 50+ variables. |
| `codebook_source.tex` | LaTeX source file for the codebook (for reproducibility). Compile with `latexmk -pdf codebook_source.tex`. |

**Key variables (see codebook for full list):**

- `pop1`–`pop3`: HPT Perspective-on-the-Past subscale items (reverse-scored in analysis)
- `cont1`–`cont3`: HPT Contextualization subscale items
- `rd1`–`rd3`: FR-LF Right-wing Dictatorship endorsement facet
- `ns1`–`ns3`: FR-LF National Socialist relativization facet
- `a1`–`a3`, `u1`–`u3`, `k1`–`k3`: KSA-3 authoritarianism subscales (aggression, submission, conventionalism)
- `kn1`–`kn6`: Historical knowledge test items (0/1 scored)
- `sdr1`–`sdr5`: Social Desirability Rating items
- `school_id`, `classroom_label`: Anonymised clustering identifiers
- `school_level`: `lower_secondary` (základní škola, ISCED 2) or `upper_secondary` (gymnázium, ISCED 3)
- `school_type`: `public`, `private`, or `church`
- `gender`: `M`, `F`, `O`
- `history_grade`: Most recent history course grade (Czech scale: 1 = excellent, 5 = failing)

---

## scripts/

All scripts expect `student_responses.RData` (or `student_responses.RDS`) to be in the
**working directory** (or one level up, depending on the script — see each file header).
The simplest approach is to copy `data/student_responses.RData` to the same directory as
the scripts before running.

### Main analysis pipeline (R Markdown, numbered sequence)

Run these in order. Each produces a PDF and Markdown report (output goes to `../outputs/`
or wherever `output_dir` is set in the Makefile/render call).

| Script | Contents |
|--------|----------|
| `01_measurement_checks.Rmd` | Instrument reliability (α, ω), EFA/CFA factor structure, descriptive item statistics, floor/ceiling checks |
| `02_descriptives_and_zero_order_correlations.Rmd` | Sample descriptives, zero-order correlation matrix, group distribution plots |
| `03_multilevel_models_hypothesis_tests.Rmd` | Main hypothesis tests (H1, H2): 2-level random-intercept models predicting HPT from ideology, knowledge, SDR |
| `04_dif_and_mg_cfa_measurement_bias.Rmd` | DIF analysis (graded response model, Bonferroni α = .01) and MG-CFA scalar invariance tests across ideology groups |
| `05_sensitivity_analyses.Rmd` | Robustness checks: alternative composites, exclusion rules, random-slope models, fixed-effects models, attenuation correction |
| `06_appendix_tables_and_figures.Rmd` | Appendix tables and supplementary figures cited in manuscript |
| `07_reproducibility_report.Rmd` | Session info, package versions, random-seed confirmation |

**To run the full pipeline:**
```r
# From the scripts/ directory, with student_responses.RData present:
rmarkdown::render("01_measurement_checks.Rmd", output_dir = "../outputs")
rmarkdown::render("02_descriptives_and_zero_order_correlations.Rmd", output_dir = "../outputs")
# ... etc., or use the Makefile (see below)
```

Or using **make** (requires R and latexmk):
```bash
# From scripts/ directory, after copying student_responses.RData here:
make all
```

### Manuscript figure scripts

Standalone R scripts that regenerate the four main manuscript figures.
Each loads `student_responses.RData` from the working directory and saves output to `../figures/`.

| Script | Figure |
|--------|--------|
| `fig02_measurement_invariance_and_dif.R` | Fig. 2 — MG-CFA factor loadings and DIF test statistics by ideology group |
| `fig03_score_distributions.R` | Fig. 3 — Distribution of HPT subscale scores across ideology tertiles |
| `fig04_coefficient_plot.R` | Fig. 4 — Multilevel model coefficient plot (standardised β with 95% CIs) |
| `fig05_marginal_effects.R` | Fig. 5 — Marginal effects of knowledge on HPT across ideology levels |

### Supplementary analysis scripts

| Script | Contents |
|--------|----------|
| `supplementary_analyses.R` | Additional models reported in appendix: NS-only grouping, alternative ideology composites, ICC decomposition |
| `tost_equivalence_tests_and_mundlak.R` | Formal TOST equivalence tests (SESOI = β ±0.20) and Mundlak within/between classroom ideology decomposition |

### Exploratory/development scripts

These were used during analysis development and are included for full transparency.
They are not part of the primary reproducibility pipeline.

| Script | Contents |
|--------|----------|
| `compute_instruments_validity.R` | Early-stage instrument validity calculations |
| `factor_and_invariance.R` | Standalone factor analysis and invariance testing |
| `multilevel_and_DIF.R` | Standalone multilevel model and DIF development script |
| `H1_ideology_elevates_HPT.R` | Standalone test of Hypothesis 1 (congruence pathway) |

### Build file

| File | Contents |
|------|----------|
| `Makefile` | Automates rendering of all main Rmd reports to PDF. Run `make all` or `make codebook`. |

---

## outputs/

Pre-rendered PDF reports from the main analysis pipeline. These are the fully executed
outputs corresponding to the scripts above and can be read without running any code.

| File | Contents |
|------|----------|
| `01_measurement_checks.pdf` | Measurement validity, reliability, factor structure |
| `02_descriptives_and_zero_order_correlations.pdf` | Descriptive statistics and zero-order correlations |
| `03_multilevel_models_hypothesis_tests.pdf` | Main hypothesis tests |
| `04_dif_and_mg_cfa_measurement_bias.pdf` | DIF and MG-CFA results |
| `05_sensitivity_analyses.pdf` | Sensitivity and robustness analyses |
| `06_appendix_tables_and_figures.pdf` | Appendix tables and figures |

---

## figures/

High-resolution versions of the four main manuscript figures, in both PDF (vector) and
PNG (raster at 300 dpi) formats.

| Files | Description |
|-------|-------------|
| `fig02_measurement_invariance_and_dif.*` | MG-CFA loadings + DIF plot |
| `fig03_score_distributions.*` | HPT score distributions by ideology group |
| `fig04_coefficient_plot.*` | Multilevel model coefficients |
| `fig05_marginal_effects.*` | Marginal effects of knowledge |

---

## instruments/

The three questionnaires administered to students, in their Czech-language versions
as used in data collection.

| File | Description |
|------|-------------|
| `hpt_questionnaire.docx` | Historical Perspective Taking instrument (9 items: POP, ROA, CONT subscales; adapted from Hartmann & Hasselhorn, 2008) |
| `ideology_authoritarianism_sdr_questionnaires.docx` | FR-LF mini (6 items: RD + NS facets), KSA-3 (9 items: authoritarianism), and Social Desirability Rating (5 items) |
| `historical_knowledge_test.docx` | Historical knowledge test (6 items on the interwar period and Weimar Republic) |

---

## Software Requirements

All analyses were conducted in **R**. The following packages are required:

**Core pipeline:**
```
lme4, lmerTest       # multilevel models
lavaan               # confirmatory factor analysis (MG-CFA)
mirt                 # item response theory (DIF, graded response model)
psych                # reliability (α, ω), EFA
tidyverse            # data manipulation and plotting
janitor              # data cleaning
```

**Supplementary:**
```
TOSTER               # TOST equivalence tests (or manual computation as in scripts)
ggplot2              # figures
patchwork            # figure composition
knitr, rmarkdown     # report rendering
```

R version used: see `07_reproducibility_report.Rmd` output for exact session info.

---

## What Is Not Included

The following files are excluded from this package on confidentiality grounds:

- **Individual teacher response files** — raw Google Forms exports linked to specific teachers and schools by name. These cannot be shared without violating data protection agreements (GDPR, Regulation 2016/679/EU) and participant consent terms.
- **Individual teacher feedback reports** — personalised reports sent to participating teachers, which contain school-identifiable information.
- **Data collection script** (`pull_and_normalize_data_CONFIDENTAL.R`) — the script that retrieved raw data from Google Sheets and normalised it. It contains API credentials and school-identifiable mapping tables.
- **Teacher participation tracking spreadsheet** — contains teacher names, school names, and contact information.

The `data/student_responses.*` files are the fully de-identified, analysis-ready output
of that pipeline. They contain no individual names, no school names, and no information
that could identify participants.

---

## Ethical Statement

This study was conducted in accordance with the ethical requirements of Charles University,
Faculty of Arts. Research procedures were reviewed and approved by the ethics committee of
the Department of History and History Didactics prior to data collection. Participation was
voluntary; written informed consent was obtained from parents or legal guardians of all
participants under 18, and each student's assent was confirmed before administration.
Student responses were anonymised at the point of data entry.

---

## Citation

Kaleta, J. (in press). Does ideological orientation contaminate historical perspective taking?
A boundary-condition validation study. *Theory & Research in Social Education*.

Preregistration: https://osf.io/yng37/

---

## Contact

Juda Kaleta
Department of History and History Didactics
Faculty of Arts, Charles University
nám. Jana Palacha 2, 116 38 Prague 1, Czech Republic
juda.kaleta@ff.cuni.cz
