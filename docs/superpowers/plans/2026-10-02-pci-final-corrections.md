# PCI Final Corrections Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Correct the final PCI Psychology revision package, verify that the
statistical corrections do not alter the substantive findings, add the Turkish
HPT study, and refresh the replication package for later Zenodo deposit.

**Architecture:** Treat the current revised manuscript as the editing base.
First lock expected statistical and package invariants in read-only regression
checks, then correct the ICC and CFA pipelines, propagate verified outputs into
the manuscript and reviewer response, and finally rebuild every submission
artifact. Keep the existing title and conclusions unless rerun results require
a substantive change.

**Tech Stack:** R, R Markdown, lavaan, lme4, LaTeX, BibTeX, Pandoc, Git.

---

### Task 1: Lock final-package expectations

**Files:**
- Modify: `submissions/pci_psychology/test_revision_reporting_analyses.R`

- [ ] Add assertions for explicit score thresholds, robust/scaled CFA output,
  separate school and class-within-school ICCs, current preregistration and
  preprint identifiers, and required replication-package files.
- [ ] Run the test and confirm that the new assertions fail for the current
  CFA, ICC, identifier, or package state.

### Task 2: Correct CFA and ICC computation

**Files:**
- Modify: `01_measurement-checks.Rmd`
- Modify: `02_descriptives-and-zero-order.Rmd`
- Modify: `osf_storage/scripts/01_measurement_checks.Rmd`
- Modify: `osf_storage/scripts/02_descriptives_and_zero_order_correlations.Rmd`
- Modify: `submissions/pci_psychology/verify_statistics.R`

- [ ] Standardize single-group and multi-group categorical CFA reporting on
  robust/scaled WLSMV fit indices.
- [ ] Compute school, class-within-school, and total-cluster ICCs directly from
  one nested random-intercept model rather than relabelling an aggregate ICC.
- [ ] Rerun the focused verification and compare all corrected values with the
  existing substantive findings.
- [ ] Stop and report before changing conclusions if model preference, DIF,
  invariance, focal regression, or the direction/significance of primary
  findings changes materially.

### Task 3: Add and verify the Turkish HPT source

**Files:**
- Create: `/home/yetty/PhD/knowledge/R/Aktin_et_al2026Taking.md`
- Download: `/home/yetty/PhD/knowledge/L/<downloaded-pdf-name>.pdf`
- Regenerate: `/home/yetty/PhD/bibliography.bib`

- [ ] Download DOI `10.1080/13511610.2026.2630018` through the approved
  article-download workflow.
- [ ] Verify from the full text whether the study administers the nine-item
  instrument or instead uses Hartmann and Hasselhorn's categories as an
  analytical framework.
- [ ] Create and validate an R/ note with complete frontmatter, register it in
  `phd-reading`, regenerate the master bibliography, and run scoped metadata
  checks.

### Task 4: Correct and align manuscript-facing documents

**Files:**
- Modify: `submissions/pci_psychology/manuscript.tex`
- Modify: `submissions/pci_psychology/supplementary_materials.md`
- Modify: `submissions/pci_psychology/response_to_reviewers.md`
- Modify: `submissions/pci_psychology/cover_letter.md`
- Modify: `submissions/pci_psychology/reviews/revision_ledger.md`
- Modify: `submissions/pci_psychology/top_disclosure_table.md`

- [ ] Replace the OSF project DOI in the TOP preprint field with the verified
  PsyArXiv DOI, and use `zsngy` for the immutable preregistration.
- [ ] Correct the HPT fit-to-situation response anchors.
- [ ] Propagate corrected robust CFA and nested ICC values and interpretations.
- [ ] Label the abstract statistic as right-authoritarian attitudes.
- [ ] Add a bounded description of the Turkish qualitative application.
- [ ] Simplify DIF multiplicity and preregistration wording without implying
  that the registered BH procedure was implemented.
- [ ] Align every response-letter claim and location with the final manuscript.
- [ ] Apply only mechanical numeric, terminology, spelling, and reference-list
  corrections; retain the current title and substantive scope.

### Task 5: Refresh the future Zenodo replication package

**Files:**
- Modify: `osf_storage/README.md`
- Modify: `osf_storage/scripts/*` where synchronized source changed
- Modify: `osf_storage/outputs/*`
- Add: current supplement and revision scripts/outputs as required

- [ ] Correct the ethics statement in the public package to match the
  manuscript, as directed by the author.
- [ ] Replace OSF-project language with repository-neutral replication-package
  language suitable for the planned Zenodo migration while preserving current
  identifiers as provenance.
- [ ] Refresh scripts, outputs, supplement, title, affiliations, identifiers,
  execution order, and table/figure mappings.
- [ ] Ensure the package contains every file promised by the manuscript's data
  availability statement.

### Task 6: Rebuild, audit, and publish

**Files:**
- Modify: `status.md`
- Modify: `project_status.md`
- Regenerate: `submissions/pci_psychology/manuscript.pdf`
- Regenerate: `submissions/pci_psychology/preprint.pdf`
- Regenerate: `submissions/pci_psychology/supplementary_materials.pdf`
- Regenerate: `submissions/pci_psychology/manuscript_diff.tex`
- Regenerate: `submissions/pci_psychology/manuscript_diff.pdf`

- [ ] Run the full reporting and statistical verification suite.
- [ ] Rebuild clean manuscript, supplement, and redline artifacts.
- [ ] Verify references, PDF text, page locations, identifiers, ethics text,
  absence of unresolved citations/placeholders, and replication-package
  completeness.
- [ ] Run a final citation/reference verification against the knowledge base.
- [ ] Review explicit diffs and update status files with the verified state.
- [ ] Commit and push the project, then commit and push only the project pointer
  and regenerated root bibliography in the root repository.

## Self-review

- Every user-approved critical and major correction maps to a task.
- Statistical changes have an explicit stop-and-report gate for material
  outcome changes.
- The Turkish source workflow includes discovery, download, full-text
  verification, note creation, bibliography regeneration, and validation.
- The current title is explicitly preserved.
- The refreshed package is prepared for a later Zenodo migration without
  performing that migration in this task.
