# Minimal PCI Revision Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reduce the PCI Psychology revision to the smallest set of changes
that answers the recommender and reviewers while retaining corrections needed
for factual accuracy, statistical validity, and honest preregistration
disclosure.

**Architecture:** Use the submitted manuscript at commit `c952aa5` as the
comparison baseline and the current manuscript as the editing base. Retain
only request-mapped changes and critical audit corrections; compress requested
expansions that became new methodological programmes. Rebuild all submission
artifacts and regenerate the redline from the original submitted baseline.

**Tech Stack:** LaTeX, BibTeX, R, Pandoc, `latexdiff`, Git.

---

## File map

- Modify `submissions/pci_psychology/manuscript.tex`: restore the title and
  submitted framing while retaining minimal requested and critical changes.
- Modify `submissions/pci_psychology/supplementary_materials.md`: retain Tables
  S1-S5, remove the over-developed response-process protocol, and restore the
  submitted title.
- Modify `submissions/pci_psychology/response_to_reviewers.md`: describe only
  revisions actually retained and distinguish reviewer requests from critical
  audit corrections.
- Modify `submissions/pci_psychology/cover_letter.md`: restore the original
  title and avoid presenting the revision as a wholesale rewrite.
- Modify `submissions/pci_psychology/reviews/revision_ledger.md`: record the
  minimal-scope decision and the final disposition of each request.
- Modify `status.md` and `project_status.md`: replace the broad-rewrite account
  with the minimal-revision state.
- Regenerate `submissions/pci_psychology/manuscript.pdf`, `preprint.pdf`,
  `supplementary_materials.pdf`, `manuscript_diff.tex`, and
  `manuscript_diff.pdf`.

### Task 1: Restore the submitted identity and framing

- [ ] Restore the submitted title in the PDF metadata, manuscript title,
  supplement, response letter, and cover letter:

  `Cross-Cultural Validation and Ideological Fairness of a Historical
  Perspective Taking Instrument: Evidence from Czech Secondary Students`.

- [ ] Preserve the submitted section order and contribution structure unless
  a reviewer request or critical correction requires a local change.
- [ ] Do not perform a spelling or style sweep; preserve submitted wording in
  restored passages and follow project conventions only in new text.

### Task 2: Retain the recommender's five requested changes minimally

- [ ] Keep one dedicated content-validity limitation explaining that nine
  ratings of one Weimar scenario do not sample multiple HPT contexts; end with
  one sentence calling for scenarios across periods and places. Remove anchor
  item and scenario-by-person modelling prescriptions.
- [ ] Keep the bounded internal-structure statement in Section 4.1 and one
  future-research sentence on differential prediction of external outcomes.
- [ ] Keep construct names in Results and Table 7.
- [ ] Keep 95% confidence intervals in Table 7 and the exact pairwise sample
  sizes in Table S3b.
- [ ] Keep one Loevinger breadth-precision sentence, the focal reliability
  consequences, and supplementary reliability estimates; remove unnecessary
  main-text formula and bootstrap detail.

### Task 3: Retain each reviewer's requested changes minimally

- [ ] Keep a targeted abstract revision that states the main numerical
  findings and the qualified DIF/MG-CFA conclusion without changing the title
  or rewriting the paper's contribution.
- [ ] Keep the verified voting correction and Hartmann/Hasselhorn and
  Lee/Ashby author-date repairs.
- [ ] Keep a concise evidence map naming the verified German and Dutch work;
  remove unsupported superlatives and unsupported comparative-use claims.
- [ ] Reduce the Czech curriculum addition to subject status, relevant
  interwar content, the absence of an explicit HPT competence, and the limits
  of teacher-reported familiarity.
- [ ] Keep one concise construct-boundary paragraph explaining the congruence
  and integrative-complexity pathways and distinguishing contextual reasoning
  from moral or affective endorsement.
- [ ] Keep non-HPT reliability and age/language suitability in Tables S3-S4
  with only a concise main-text qualification.
- [ ] Keep the summary loading/reliability tables in the manuscript and their
  detailed versions in the supplement.
- [ ] Keep the corrected ideology and historical-knowledge descriptives, the
  explicit absence of defensible socioeconomic data, and the two-panel
  descriptive relationship figure.
- [ ] Keep one sentence crediting Huijgen's item-rating/think-aloud precedent
  and one sentence recommending response-process work. Remove Supplementary
  Method S1 and its detailed sampling/coding protocol.
- [ ] Remove the unsupported gender-moderation generalisation while retaining
  the design-specific gender limitation.
- [ ] Keep the explicit research-question structure and compact H1-H6
  disclosure required for an honest preregistration account.

### Task 4: Preserve critical audit corrections

- [ ] Do not restore the incorrect school design-effect calculation; retain
  one concise clustering limitation.
- [ ] Retain the omnibus joint-parameter description of DIF, raw-POP GRM note,
  unidimensional-model qualification, and unknown minimum detectable DIF.
- [ ] Remove the speculative inventory of six raw `p < .10` items; retain only
  the bounded result and a short anchor-contamination caveat.
- [ ] Retain the implication of the MG-CFA Heywood cases and do not claim
  scalar invariance. Remove secondary SRMR and modification-index exposition
  that is unnecessary once inadmissibility is stated.
- [ ] Retain the corrected interpretation of the positive NS-only delta CFI,
  but move cut-point and tie details out of the main narrative.
- [ ] Retain the distinction between the original PCA, latent classes, and the
  present correlated-factor CFA; do not restore a cross-national-replication
  claim.
- [ ] Retain corrected score reliability, midpoint proportions, missing-data
  rules, analysis-specific sample sizes, and the statement that unshared
  knowledge variance cannot be attributed solely to reasoning.
- [ ] Retain Table S5 and corrected TOP disclosures for omitted, substituted,
  and post-registration analyses.

### Task 5: Align submission documents

- [ ] Rewrite each response entry to name the smallest retained change and its
  exact section; remove claims about expansions that were rolled back.
- [ ] Keep the additional technical-audit response concise and factual.
- [ ] Update the location map after rebuilding.
- [ ] Record in the ledger that the title and unrelated editorial expansion
  were restored to the submitted version at the author's direction.
- [ ] Update project status files without claiming the revision is ready while
  reference propagation and human psychometric review remain open.

### Task 6: Verify and publish the checkpoint

- [ ] Run the reporting-analysis regression test and regenerate only outputs
  still cited by the minimal manuscript.
- [ ] Build the manuscript and supplement from clean auxiliary state.
- [ ] Run the reference-verifier agent on the final citation-bearing files.
- [ ] Extract PDF text and verify the submitted title, requested additions,
  critical qualifications, citation rendering, and absence of rolled-back
  over-expansions.
- [ ] Regenerate `manuscript_diff.tex` and `manuscript_diff.pdf` against
  `c952aa5`; verify that the redline is materially smaller than the previous
  revision redline.
- [ ] Inspect Git status and diffs, stage only the named project files, commit
  and push the subproject, then commit and push only the root submodule pointer.

## Self-review

- Every recommender request maps to Task 2.
- Every reviewer request maps to Task 3 or an explicit reasoned non-expansion.
- Every critical audit correction identified by the methodological review maps
  to Task 4.
- The plan explicitly restores the title and avoids unrelated stylistic work.
- No placeholder implementation steps remain.
