# Project Status Log: phd-029-hpt-and-extremism

**Title:** Stress-Testing Historical Perspective Taking: Ideological Attitudes and Disciplinary Reasoning in Czech Adolescents
**Target journals:** Social Psychology of Education (Paper A), Applied Measurement in Education (Paper B)
**Previous submissions:** TRSE (desk-rejected 2026-02-25), EJPE EUPE-D-26-00272 (desk-rejected 2026-03-18)
**Preregistration:** https://osf.io/yng37/
**Last updated:** 2026-03-23

---

## Completed Milestones

### Data Collection
- Collected N=293 responses from 20 classrooms across 10 Czech secondary schools (ISCED 2 and 3). Instruments: HPT (9 items), FR-LF mini (6 items), KSA-3 (9 items), historical knowledge (6 items), SDR-5 (5 items). Data de-identified at point of entry via Google Sheets, normalised with `pull_and_normalize_data_CONFIDENTAL.R`.

### Analysis Pipeline (7 Rmd Reports)
- **01 Measurement checks:** Reliability (alpha, omega), CFA dimensionality, ICCs, floor/ceiling effects for all instruments.
- **02 Descriptives and zero-order correlations:** Sample demographics, distributions, correlation matrix, school/class variation.
- **03 Multilevel models (H1/H2):** Two-level random-intercept models (lme4/lmerTest). Neither ideology predictor (FR-LF, KSA) significant. Historical knowledge is the only consistent predictor.
- **04 DIF and MG-CFA:** Differential item functioning (mirt, graded response model) and multi-group CFA scalar invariance (lavaan, WLSMV). No ideological measurement bias detected.
- **05 Sensitivity analyses:** Alternative composites, exclusion rules, random-slope models, fixed-effects models, attenuation correction. Results stable across all specifications.
- **06 Appendix tables and figures:** Publication-ready supplementary materials.
- **07 Reproducibility report:** Session info, package versions, random-seed confirmation.

### Supplementary Analyses
- TOST equivalence tests (SESOI = beta +/-0.20) confirm ideology effects are practically negligible. Mundlak within/between classroom ideology decomposition shows no contextual effects.

### Manuscript Drafting
- Full manuscript drafted in `trse_outputs/manuscript.md` with pandoc build to DOCX using T&F template. Four manuscript figures produced (MG-CFA/DIF, score distributions, coefficient plot, marginal effects) in PDF and PNG.

### OSF Replication Package
- Complete package in `osf_storage/`: de-identified data (RDS + XLSX), codebook (PDF + LaTeX source), all 7 analysis scripts, 4 figure scripts, 2 supplementary scripts, 4 exploratory scripts, Makefile, pre-rendered PDF outputs, high-resolution figures, Czech-language instruments. README documents full reproduction workflow.

### Quality Audit
- AI-assisted review simulations conducted (ChatGPT x4, Gemini x3) in `reviews/`. General manuscript quality audit (`checklist.md`, 27 categories) and TRSE-specific compliance audit (`trse_checklist.md`) completed.

---

### EJPE Submission Preparation
- Switched target from TRSE to EJPE. Converted manuscript from Markdown/DOCX to LaTeX using `sn-jnl.cls` (Springer Nature template with `sn-apa` style). Created blinded manuscript (`manuscript_blinded.tex`) and separate title page (`title_page.tex`). Upload package assembled in `ejpe_submission/UPLOAD_PACKAGE_EUPE/`.

### EJPE Submission (EUPE-D-26-00272)
- Submitted 2026-03-05 via Editorial Manager. Preview PDF revealed broken title page — `sn-jnl.cls` calls `\allowdisplaybreaks` at `\begin{document}` which requires `amsmath`, but `title_page.tex` didn't load it. Fixed by adding `\usepackage{amsmath}`. Merged two upload folders into single `UPLOAD_PACKAGE_EUPE/`.

---

### EJPE Desk Rejection (2026-03-18)
- Submission EUPE-D-26-00272 desk-rejected by Editor in Chief Cintia Rodríguez Garrido. Manuscript deemed outside EJPE aims and scopes. Editor noted EJPE only accepts one-time questionnaire studies with novel concepts/relationships. Suggested resubmitting to a Social Psychology journal. Rejection materials archived in `_archive/ejpe_2026-03/`.

---

### Two-Paper Reframe and Submission Preparation (2026-03-18 → 2026-03-19)
- Deep journal research with desk-reviewer simulations (`journal_target_analysis.md`). Identified SPE and AME as optimal targets.
- Split single manuscript into two papers targeting different audiences:
  - **Paper A** (SPE): Focuses on null ideology–HPT relationship (social psychology angle)
  - **Paper B** (AME): Focuses on measurement quality — DIF, invariance, bifactor models (psychometric angle)
- R analyses run to fill all [TO BE COMPUTED] placeholders in Paper B. Quality review and fixes applied to both papers.
- Paper A converted to Springer LaTeX (`sn-jnl.cls`), built to PDF with separate title page. Paper B built to anonymous and authored DOCX via `build_docx.py`.
- Cover letters written for both journals. Abstract and keywords extracted for SPE submission portal.
- Original `trse_outputs/` figure scripts and outputs archived to `_archive/`.

---

### AME Desk Rejection (2026-03-20)
- Paper B (HAME-2026-0087) desk-rejected by Joel Puchalla (Editorial Assistant). Reason: "AME does not publish studies on specific instruments." Suggested alternative: International Journal of Testing. Third desk rejection overall for phd-029 manuscripts.

### Paper A Submitted to SPE (~2026-03-19)
- Paper A submitted to Social Psychology of Education via Springer Editorial Manager. Awaiting decision.

---

## Current Work

**Phase:** Paper A under review at SPE. Paper B needs a new target journal after AME desk rejection.

- [ ] Wait for SPE decision on Paper A
- [ ] Find new target for Paper B (IJT suggested by AME; consider other measurement journals)
- [ ] Verify Decker2013 and Ripka2023 bib entries

---

## Upcoming

- Transfer project outputs to knowledge base (`/project-to-knowledge` workflow).
- Create literature note for preprint/publication in `knowledge/R/`.
