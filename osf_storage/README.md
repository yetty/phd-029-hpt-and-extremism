# Replication Package

## Study record

**Title:** Cross-Cultural Validation and Ideological Fairness of a Historical
Perspective Taking Instrument: Evidence from Czech Secondary Students

**Author:** Juda Kaleta

**Affiliation:** Institute of History, Faculty of Arts, Charles University,
Czech Republic

**Preprint:** <https://doi.org/10.31234/osf.io/hxngm_v2>

**Data and materials repository:** <https://doi.org/10.17605/OSF.IO/YNG37>

**Immutable preregistration:** <https://osf.io/zsngy>

This repository contains the de-identified analysis data, Czech-language
instrument battery, analysis code, documentation, and rendered outputs for the
preprint. The current public repository is the OSF project above. This package
is prepared for a planned archival migration to Zenodo; it is not a Zenodo
deposit and no migration or upload is made by this package.

## Contents

```
osf_storage/
|-- README.md
|-- instrument_adaptation_and_deviations.md
|-- supplementary_materials.{md,pdf}
|-- data/                         De-identified data and codebook
|-- figures/                      Retained standalone figures (PDF and PNG)
|-- instruments/                  Czech-language administered materials
|-- outputs/                      Rendered reports and revision outputs
`-- scripts/                      Reproducible analyses and Makefile
```

### Data

`data/student_responses.RDS` is the de-identified, analysis-ready R object;
`data/student_responses.xlsx` is the same data in an interoperable spreadsheet
format. `data/codebook.pdf` and `data/codebook_source.tex` document variables,
response scales, scoring, and descriptive statistics. School and classroom
identifiers are anonymised codes. `school_level` distinguishes lower-secondary
(ISCED 2) and upper-secondary (ISCED 3) education; it does not identify a
school.

The public data contain no student names, teacher names, school names, contact
details, credentials, or raw Google Forms exports. The confidential data-pull
script, teacher feedback, participation tracking, and identifiable source files
are deliberately excluded.

### Analysis and documentation reports

The numbered R Markdown reports comprise five analytic reports and two
documentation reports:

| Report | Role |
|---|---|
| `01_measurement_checks.Rmd` | Reliability and HPT factor-structure checks |
| `02_descriptives_and_zero_order_correlations.Rmd` | Descriptives, correlations, and nested ICC components |
| `03_multilevel_models_hypothesis_tests.Rmd` | Multilevel focal association models |
| `04_dif_and_mg_cfa_measurement_bias.Rmd` | GRM DIF and multi-group CFA analyses |
| `05_sensitivity_analyses.Rmd` | Exploratory robustness checks |
| `06_appendix_tables_and_figures.Rmd` | Documentation inventory for supplement tables and figures |
| `07_reproducibility_report.Rmd` | Session information, checksums, seed policy, and file map |

`scoring_helpers.R` defines the shared minimum-answer scoring rules.
`revision_reporting_analyses.R` produces the revision reliability,
participant-description, correlation-interval, knowledge-facility, and
relationship-figure outputs in `outputs/revision/`. The supplementary scripts
are `supplementary_analyses.R` and
`tost_equivalence_tests_and_mundlak.R`. The latter reports exploratory TOST
equivalence and Mundlak decomposition analyses.

The retained standalone figure scripts are
`fig02_measurement_invariance_and_dif.R`, `fig03_score_distributions.R`,
`fig04_coefficient_plot.R`, and `fig05_marginal_effects.R`. Development-stage
scripts remain for transparency but are not part of the numbered analytic
pipeline.

### DIF and supplementary material

The DIF analysis uses an all-item graded-response-model procedure in `mirt`.
Each item receives a joint omnibus likelihood-ratio test that releases its slope
and threshold constraints. The raw p-values use Bonferroni adjustment across
the nine HPT items (`p.adjust = "bonferroni"`; raw p-values multiplied by nine
and capped at one) and are evaluated against familywise alpha = .05 (equivalent
per-test alpha = .0056). It does not use an alpha of .01.

`supplementary_materials.md` and `supplementary_materials.pdf` contain Tables
S1-S5. `instrument_adaptation_and_deviations.md` records verified adaptation
facts without reconstructing wording changes, and directs readers to Table S5
for preregistration, instrument, and analysis-plan discrepancies.

### Mapping to manuscript and supplement elements

| Element | Source |
|---|---|
| Tables 1-2 and Table S2 | `01_measurement_checks.Rmd` |
| Table 3, Table 4, Table 7, Table S3, Table S3b, and Table S4b | `revision_reporting_analyses.R` |
| Table 5 | `04_dif_and_mg_cfa_measurement_bias.Rmd` (joint omnibus DIF tests) |
| Table 6 | `04_dif_and_mg_cfa_measurement_bias.Rmd` (MG-CFA invariance ladder) |
| Table S1 | `04_dif_and_mg_cfa_measurement_bias.Rmd`: constrained `mod_base` GRM extraction written to `outputs/table_s1_irt_parameters.csv` |
| Table S4 | `supplementary_materials.md`, based on the cited source-population documentation |
| Table S5 | `supplementary_materials.md` and `instrument_adaptation_and_deviations.md` |
| Figure 1 and revision CSV outputs | `revision_reporting_analyses.R` |
| Retained supporting figures | `fig02_measurement_invariance_and_dif.R` through `fig05_marginal_effects.R` |

### Rendered outputs

`outputs/` contains PDFs for reports 01-07 and
`table_s1_irt_parameters.csv`, the constrained-GRM source export for Table S1.
`outputs/revision/` contains the CSV files and PDF/PNG relationship figure
created by `revision_reporting_analyses.R`. The PDFs are supplied for
inspection; the scripts are the source of record.

## Reproducing the reports

Use R with the packages declared in the scripts, a LaTeX installation with
XeLaTeX, and GNU Make. From `osf_storage/scripts`, run:

```bash
make all
test ! -e student_responses.RDS
```

`make all` creates a temporary local symbolic link to
`../data/student_responses.RDS` while rendering and removes it on exit. It does
not copy data into `scripts/` and leaves no `student_responses.RDS` there. To
refresh the reviewer-requested outputs, use the same temporary link:

```bash
ln -s ../data/student_responses.RDS student_responses.RDS
Rscript --vanilla revision_reporting_analyses.R
rm -f student_responses.RDS
```

The principal packages are `lavaan`, `lme4`, `lmerTest`, `mirt`, `psych`,
`semTools`, `tidyverse`, `janitor`, `knitr`, and `rmarkdown`; individual
reports declare their additional packages. Report 07 records the actual R
session and available package versions at render time.

To rebuild the current PCI supplement from its Markdown source, run this from
the project root before copying both files into this directory:

```bash
make supplement
cp submissions/pci_psychology/supplementary_materials.{md,pdf} osf_storage/
```

## Ethics and access conditions

This study involved secondary school students completing questionnaires during
regular class periods. Under Czech law (Act No. 110/2019 Sb., on the processing
of personal data), ethics committee approval is not required for
non-interventional educational research involving anonymous questionnaires
where no personal data are collected. Participation was voluntary; students
were informed about the study's purpose and their right to decline without
consequences. All responses were de-identified at the point of data entry. No
personally identifiable information was collected or stored. The study followed
the principles of the Declaration of Helsinki for research involving human
participants.

## Citation

Kaleta, J. (2026). *Cross-Cultural Validation and Ideological Fairness of a
Historical Perspective Taking Instrument: Evidence from Czech Secondary
Students* [Preprint]. https://doi.org/10.31234/osf.io/hxngm_v2
