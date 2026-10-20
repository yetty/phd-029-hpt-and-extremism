# TOP Disclosure Table

**Cross-Cultural Validation and Ideological Fairness of a
Historical Perspective Taking Instrument: Evidence from Czech
Secondary Students**

Juda Kaleta

Transparency and Openness Promotion (TOP) Guidelines disclosure
following PCI Psychology requirements.

| Standard | Level | Description / Location |
|----------|-------|------------------------|
| Citation | Level 2 | All references include DOIs where available; full bibliography in `bibliography.bib` and project-local `extras.bib` |
| Data transparency | Level 3 | De-identified data are available on Zenodo: <https://doi.org/10.5281/zenodo.23494026> in both RDS and xlsx formats with a full variable codebook (PDF) |
| Analytic code transparency | Level 3 | All analysis scripts are available on Zenodo: <https://doi.org/10.5281/zenodo.23494026> (5 analytic R Markdown reports, 2 documentation reports, shared scoring and revision-reporting scripts, 4 figure R scripts, 2 supplementary analysis R scripts, and 1 Makefile) |
| Research materials transparency | Level 1 | Measures are described and cited in the manuscript; Zenodo provides adaptation and deviation documentation: <https://doi.org/10.5281/zenodo.23494026>. Third-party instrument files and item text are excluded pending documented redistribution permission. |
| Design transparency | Level 2 | Study design fully described in manuscript Method section (Sections 3.1--3.4); deviations from the original instrument documented on Zenodo |
| Preregistration of studies | Level 2 | Study preregistered on OSF prior to data collection: <https://osf.io/zsngy> |
| Preregistration of analysis plan | Level 2 | Analysis plan (hypotheses, statistical models, sample size justification) preregistered on OSF: <https://osf.io/zsngy> |
| Replication | N/A | This is a Czech adaptation of the HPT instrument; direct replication is not applicable |

## Notes

- **Data format:** The de-identified dataset is provided in two
  formats: `.RDS` (native R serialisation) and `.xlsx` (Excel)
  to ensure accessibility for users of different software
  environments.
- **Code functionality:** Five analytic R Markdown reports (01--05)
  and two documentation reports (06--07) render to PDF reports in
  the `outputs/` directory. Shared scoring and revision-reporting
  scripts reproduce the revision outputs. Scripts can be executed
  via the included Makefile (`make all`). The reproducibility report
  (`07_reproducibility_report.Rmd`) documents the R session, package
  versions, checksums, seed policy, and file map.
- **README documentation:** A detailed `README.md` in the Zenodo
  reproducibility package explains the directory structure, execution order,
  software requirements, and mapping of scripts to manuscript
  tables and figures.
