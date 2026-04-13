# TOP Disclosure Table

**Cross-Cultural Validation and Ideological Fairness of a
Historical Perspective Taking Instrument: Evidence from Czech
Secondary Students**

Juda Kaleta

Transparency and Openness Promotion (TOP) Guidelines disclosure
following PCI Psychology requirements.

| Standard | Level | Description / Location |
|----------|-------|------------------------|
| Citation | Level 2 | All references include DOIs where available; full bibliography in `references.bib` |
| Data transparency | Level 3 | De-identified data publicly available on OSF: <https://doi.org/10.17605/OSF.IO/YNG37> in both RDS and xlsx formats with a full variable codebook (PDF) |
| Analytic code transparency | Level 3 | All analysis scripts publicly available on OSF: <https://doi.org/10.17605/OSF.IO/YNG37> (7 R Markdown pipeline scripts, 4 figure R scripts, 2 supplementary analysis R scripts, 1 Makefile) |
| Research materials transparency | Level 3 | Complete instrument battery (Czech-language versions of HPT, FR-LF, KSA-3, SDR-5, and historical knowledge test) publicly available on OSF: <https://doi.org/10.17605/OSF.IO/YNG37> |
| Design transparency | Level 2 | Study design fully described in manuscript Method section (Sections 3.1--3.6); deviations from original instrument documented in OSF repository |
| Preregistration of studies | Level 2 | Study preregistered on OSF prior to data collection: <https://doi.org/10.17605/OSF.IO/YNG37> |
| Preregistration of analysis plan | Level 2 | Analysis plan (hypotheses, statistical models, sample size justification) preregistered on OSF: <https://doi.org/10.17605/OSF.IO/YNG37> |
| Replication | N/A | This is the first Czech adaptation of the HPT instrument; direct replication is not applicable |

## Notes

- **Data format:** The de-identified dataset is provided in two
  formats: `.RDS` (native R serialisation) and `.xlsx` (Excel)
  to ensure accessibility for users of different software
  environments.
- **Code functionality:** All R Markdown scripts render to PDF
  reports included in the `outputs/` directory. Scripts can be
  executed sequentially (01 through 07) or via the included
  Makefile (`make all`). A reproducibility report
  (`07_reproducibility_report.Rmd`) documents the exact R
  version and package versions used.
- **README documentation:** A detailed `README.md` in the OSF
  repository explains the directory structure, execution order,
  software requirements, and mapping of scripts to manuscript
  tables and figures.
