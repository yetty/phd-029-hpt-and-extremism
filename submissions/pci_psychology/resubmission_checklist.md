# PCI Psychology resubmission checklist

**Status:** Draft
**Created:** 2026-10-20
**Basis:** Full review of the revision package against the recommender
letter, both reviews, `response_to_reviewers.md`, `cover_letter.md`,
`reviews/revision_ledger.md`, the 2026-09-11 requirements review, the
submitted version (commit `60d4c5f`), the script outputs in
`revision_outputs/`, and the public OSF and PsyArXiv records.

Line numbers refer to `manuscript.tex` at commit `bd4c5f9`. They will shift
as edits are made; work top to bottom within each file or search for the
quoted text.

Order of work: A (decisions) -> B (Zenodo) -> C-F (text) -> G (citations)
-> H (letters and forms) -> I (rebuild) -> J (upload) -> K (housekeeping).
Zenodo comes early because its DOI must appear in the manuscript, TOP form,
README and cover letter before the final build.

---

## A. Decisions to make first

- [ ] **A1. Zenodo licence.** Choose licences for the package (suggested:
  CC BY 4.0 for data, documentation and instruments; MIT for code). Check
  that redistributing the Czech translations of FR-LF, KSA-3 and SDR-5 is
  compatible with the original instruments' terms.
- [ ] **A2. Fate of the OSF project files.** OSF project storage still holds
  the April package, whose data very likely contain the original class
  labels (`septima`, `G1.E`, `I3.A`; `septima` reveals the school type).
  Decide: delete the files from OSF storage and leave a pointer to Zenodo,
  or make the project private. OSF keeps previous file versions
  downloadable, so overwriting is not enough.
- [ ] **A3. Translation source.** Confirmed by the author: the Czech version
  was translated from the published **English** version. Decide the role
  of `Hartmann2008` (German dissertation): cite it only as the instrument's
  original development source, or drop it. Suggested: keep one mention as
  the original development source; cite the 2008 article everywhere else.
- [ ] **A4. Upper-secondary curriculum (R1-08).** Keep the current bounded
  disclosure (curriculum account covers only the lower-secondary 30%), or
  add an upper-secondary framework citation without naming the school
  type. Suggested: keep the disclosure, but shorten it (see E5).
- [ ] **A5. Ideology group means (R2-03).** Add tertile means of the HPT
  composite to the supplement, or explain in the response why only the
  continuous display is given. Suggested: add a three-row table to the
  supplement; the groups already exist from the DIF analysis.
- [ ] **A6. Wilschut and Schiphorst (2019).** Read and cite (where it fits
  in Section 2.1), or explain in the response why it is not cited.
- [ ] **A7. "We" or "I".** Single author: the manuscript and response use
  "we", the cover letter uses "I". Pick one for all three documents.

---

## B. Move the replication package from OSF to Zenodo

OSF has discontinued project functionality. The preregistration
(`https://osf.io/zsngy`, public, registered 2025-11-27) and the PsyArXiv
preprint (`hxngm`) are unaffected and keep their identifiers.

- [ ] **B1. Freeze the package contents.** Finish all analysis-affecting
  edits first (none of the items below should change statistics; if one
  does, rerun the pipeline before uploading).
- [ ] **B2. Create a Zenodo draft and reserve the DOI.** Use "Reserve DOI"
  so the DOI can go into the manuscript before publishing the record.
  Metadata:
  - [ ] Title (match the manuscript title), creator with ORCID
    0000-0003-0043-4039 and affiliation (Charles University, Faculty of
    Arts, Institute of History), resource type "Dataset" (or "Other" with
    a clear description), publication date, version `1.0`.
  - [ ] Description: what the package contains, execution order, and the
    mapping to manuscript tables and figures (reuse the README text).
  - [ ] Licence(s) from A1.
  - [ ] Related identifiers: preregistration `https://osf.io/zsngy`
    (relation: "is supplement to" or "references"); preprint
    `10.31234/osf.io/hxngm` (relation: "is supplement to"); the old OSF
    project `10.17605/OSF.IO/YNG37` (relation: "is new version of" or
    "is derived from"), for provenance.
  - [ ] Keywords as in the manuscript.
- [ ] **B3. Prepare the upload from `osf_storage/` only.** Never include
  repository-root files (`*CONFIDENTAL*`, teacher reports,
  `normalised_responses.*`, `Research Log.docx`, recruitment material).
  - [ ] Confirm the data files use the opaque `C01`-`C20` class codes and
    contain no other identifying field.
  - [ ] Confirm the package holds everything the manuscript promises:
    data (RDS, xlsx), codebook PDF, instruments, five analytic Rmd reports,
    two documentation reports, `scoring_helpers.R`,
    `revision_reporting_analyses.R`, four figure scripts, two
    supplementary scripts, Makefile, supplement (Tables S1-S5, S3b, S4b),
    `instrument_adaptation_and_deviations.md`, README.
  - [ ] Optionally rename the folder from `osf_storage/` to something
    repository-neutral (for example `replication_package/`); if so, update
    the Makefile, `test_revision_reporting_analyses.R` and
    `docs/study-and-pipeline.md` paths in the same commit.
  - [ ] Upload as a single zip plus the README at top level, or as loose
    files; keep the directory structure the README documents.
- [ ] **B4. Update `osf_storage/README.md`.** Replace the OSF project link
  with the Zenodo DOI; keep the OSF DOI only as provenance; keep the
  preregistration and preprint links. Remove "prepared for migration"
  language.
- [ ] **B5. Replace every OSF-project reference in the submission files.**
  - [ ] `manuscript.tex:331` "OSF repository
    (https://doi.org/10.17605/OSF.IO/YNG37)"
  - [ ] `manuscript.tex:657` "Table S1 in the OSF supplementary materials"
    (Table S1 is in the supplement that accompanies the manuscript; say so)
  - [ ] `manuscript.tex:725` "analysis scripts in the OSF data and code
    repository"
  - [ ] `manuscript.tex:1045-1057` Data Availability: cite the Zenodo DOI;
    delete "currently available at OSF and is prepared for a planned
    archival migration to Zenodo; it has not yet been migrated"
  - [ ] TOP form: `manuscript.tex:1135, 1151, 1183, 1202, 1210, 1229, 1239`
    (also delete the Zenodo-migration clause at 1202)
  - [ ] Keep `https://osf.io/zsngy` at 312, 347, 1056 and 1256; consider
    "preregistered (https://osf.io/zsngy)" instead of "preregistered on
    OSF" so the text does not imply the materials are on OSF
  - [ ] `cover_letter.md:42`
  - [ ] `top_disclosure_table.md:15-17, 36`
  - [ ] `test_revision_reporting_analyses.R:274, 277, 655` (assertions
    expect the OSF DOI; switch them to the Zenodo DOI)
  - [ ] `docs/study-and-pipeline.md`, `CLAUDE.md`, `project_status.md`
- [ ] **B6. Publish the Zenodo record** after the final rebuild (I3) so
  the archived PDFs and outputs match the submitted manuscript. Check that
  the DOI resolves and that every file downloads.
- [ ] **B7. Clean up OSF** according to A2. Add a short note or wiki line
  on the OSF project pointing to the Zenodo DOI, if OSF still allows it.
- [ ] **B8. Mention the move in the response letter** (one line in the
  META entry, see F1).

---

## C. Factual corrections (must fix)

- [ ] **C1. Translation source (325).** Revert to the submitted wording,
  which was correct: "Two bilingual Czech-English experts conducted
  independent forward translations from the published English version of
  the instrument \citep{Hartmann_Hasselhorn2008Historical}." Remove "from
  the German instrument reported in \citet{Hartmann2008}; wording was
  cross-checked against the published English validation article".
- [ ] **C2. Other German-source statements.** Make consistent with C1:
  - [ ] `manuscript.tex:330` "reverse-scoring of POP items from the German
    source" -> "from the original instrument"
  - [ ] `manuscript.tex:362` "Czech adaptation of the scenario-based HPT
    instrument from the German dissertation \citep{Hartmann2008}, with the
    published English account used as supporting validation evidence" ->
    "Czech adaptation of the Hartmann and Hasselhorn (2008) instrument"
    (plus, per A3, at most one mention that the instrument originates in
    Hartmann's (2008) dissertation)
  - [ ] `osf_storage/instrument_adaptation_and_deviations.md:5-9` (says
    "adapted from the German ... instrument ... reported in Hartmann
    (2008)")
  - [ ] Response R1-04 (see F4) and ledger R1-04 note
  - [ ] Check the supplement Table S4 row for HPT (line 117) still reads
    correctly ("German" there refers to the original population; fine)
- [ ] **C3. Invented example item (268).** "His decision was understandable
  given the political and economic turmoil of the Weimar Republic" is not
  an instrument item, and the task never says Hannes decided. Replace with
  a translated real CONT item, for example CONT2: "Hitler probably
  represents a strong leader to him. He will probably not think much about
  the dangers associated with the NSDAP." Adjust the next two sentences
  accordingly (a student who rates this as fitting Hannes's situation
  because they reconstructed the context vs. because it matches their own
  sympathies).
- [ ] **C4. Sample-size claim (347).** "the achieved N = 285--293 met that
  regression target" conflicts with the focal model n = 282. Replace with
  "the focal regression used n = 282 students, above the registered
  minimum of 250".
- [ ] **C5. "Registered one- and two-factor alternatives" (835).** The
  preregistration audit lists no registered CFA comparison. Verify against
  `https://osf.io/zsngy`; if not registered, delete "registered".
- [ ] **C6. TOST bound (800).** "+-0.20 ... corresponding to Cohen's
  threshold for a 'small' effect" is wrong for standardized coefficients
  (Cohen's small is r = .10; .20 is small for d). Replace with a neutral
  justification, for example "set post hoc at +-0.20 standardized units,
  which we treated as the smallest practically relevant effect", and keep
  the existing disclosure that this exceeds the .15 power target.
- [ ] **C7. Table 7 note (769).** Remove the definitions of FR-LF, KSA-3
  and SDR-5 (they no longer appear in the table); keep scale ranges if
  useful.

---

## D. Reviewer-coverage gaps

### D1. REC-01: content validity (the recommender's main concern)

- [ ] Split limitation 1 (953-957). Make **content validity** its own,
  first limitation; move "single country" into the sampling limitation
  (item 5).
- [ ] Use the terms "content validity" and "construct underrepresentation"
  \citep{Messick1995Validity} explicitly.
- [ ] Say what the scores license: claims about performance on the Weimar
  voting task, not about HPT as a general competence.
- [ ] Keep the existing points (nine ratings are not nine content samples;
  scenario-specific knowledge and moral salience) and the
  multi-scenario recommendation; add that the fairness result is equally
  scenario-bound.
- [ ] Carry one clause into 5.8 Conclusion ("for this scenario").
- [ ] Suggested wording (5-6 sentences, adapt freely):
  "*Content validity.* The instrument samples the HPT domain through a
  single scenario. Nine ratings of one Weimar voting dilemma provide
  repeated judgments about the same content, not nine independent samples
  of the domain, so the scores risk construct underrepresentation
  \citep{Messick1995Validity}. Scenario-specific knowledge, familiarity,
  and moral salience therefore cannot be separated from perspective
  taking itself. The present evidence supports interpretations of
  performance on this task, not of HPT as a general competence, and the
  fairness findings are bounded in the same way. Stronger content
  evidence would require parallel item sets applied to several scenarios
  that vary in period, place, political valence, and familiarity."
- [ ] Update response REC-01 to quote the new heading and location.

### D2. REC-05: reliability framing

- [ ] Read Loevinger (1954), "The attenuation paradox in test theory",
  *Psychological Bulletin*, 51(5), 493-504; create the R/ note; cite it
  (the recommender named the attenuation paradox; the current citation is
  Loevinger, 1957).
- [ ] Rewrite the 5.3 opening (849) to follow the recommender's framing:
  start with the breadth-precision trade-off and the attenuation paradox,
  not with "fall below conventional benchmarks". Keep one sentence
  acknowledging that precision is modest.
- [ ] Use the phrase "attenuation paradox" explicitly.
- [ ] Re-check the Loevinger paraphrase ("repeated versions of a narrow
  question") against the source; the 2026-09-11 audit flagged it.
- [ ] Add the consequence the recommender asked for: modest reliability
  attenuates validity coefficients, so the knowledge correlation is a
  lower bound **and the ideology nulls are also attenuated**, which
  limits the strength of the fairness evidence. One or two sentences.
- [ ] Rewrite response REC-05: drop "rather than an acceptable consequence
  of brevity" (it reads as contradicting the recommender); say you adopted
  his suggestion, cite the location, and state the validity consequence.

### D3. R1-02: protocol focus over pathways

- [ ] Covered by E3 (condense the mechanism paragraphs in 5.4-5.5).
- [ ] Remove "the instrument's resistance to attitudinal contamination"
  (921); it overstates the evidence.

### D4. Reviewer 1: smaller unanswered points

- [ ] Dutch grade result: R1 noted no significant correlation with school
  history grades in the Dutch work; here history grade correlated
  r = -.153. Add one sentence in 4.9 or 5.4 contrasting the two, and one
  line in the response.
- [ ] R1's "It is interesting that the issues with reliability are still
  present": acknowledge in response REC-05 or R1-06 (link to Huijgen et
  al., 2014, weaker reliability on transfer).
- [ ] Wilschut and Schiphorst (2019), per A6.

### D5. Reviewer 2

- [ ] R2-03: ideology group means per A5; update response R2-03 either
  way (one sentence on why the continuous display is primary).
- [ ] R2-01: label the two questions "RQ1" and "RQ2" in 2.5 (303-309) and
  reuse the labels in the Results roadmap (471-474), 4.9/4.10 and 5.1.

### D6. REC-02 and REC-03 polish

- [ ] REC-02: add "differential prediction of external criteria" as an
  explicit future-research item in 5.7 Recommendations (currently only in
  4.1 and protocol step 5).
- [ ] REC-03: Table 7 headers (753) "HPT" -> "HPT composite", "Context"
  -> "Contextualization".

---

## E. Simplifications

- [ ] **E1. Delete Table 2 (536-550).** It repeats the sentence above it;
  full loadings are in Table S2. Keep the prose sentence and point to S2.
- [ ] **E2. Delete Table 3 (557-571).** It repeats the 4.3 prose (the
  Mosier interval appears twice). Fold the two composite omegas into the
  prose and point to S3. Renumber Tables 4-7 to 2-5 and fix every
  reference (manuscript, supplement, response, ledger).
- [ ] **E3. Condense the mechanism discussion.** Merge 5.4 "cancelling
  pathways" (871-880) and the three 5.5 paragraphs (918-943) into one
  paragraph of about five sentences: the null may reflect a genuine null,
  cancelling pathways, format effects, or task-demand compliance; the
  present data cannot distinguish them; limitation 8 says how to test them.
- [ ] **E4. Remove revision-process wording from the manuscript.**
  - [ ] 855 "in the current reproducible analysis"
  - [ ] 668 "All three models converged and passed lavaan's
    post-estimation admissibility check; the diagonals of their
    model-implied residual-variance matrices were positive." -> "All three
    solutions were admissible."
  - [ ] Zenodo-migration clauses (covered by B5)
- [ ] **E5. Merge the two curriculum paragraphs in 2.2 (243-259)** into
  about three sentences; keep the lower-secondary boundary as one
  sentence.
- [ ] **E6. Shorten the composite label.** Define "primary six-item HPT
  composite (hereafter, HPT composite)" once in 3.3.1; use "HPT
  composite" in the other 13 places, and harmonise Table 4/7 and the
  Figure 1 caption.
- [ ] **E7. Reduce repeated caveats.** "Small extreme groups and unmodeled
  clustering" appears in the abstract, 4.7, 5.1, 5.4 and 5.8. Keep it in
  4.7 (full) and the abstract and 5.8 (short); cut it from 5.1 and 5.4.
- [ ] **E8. Shorten 4.8 (NS-only grouping)** to two sentences; move the
  fit values to a supplement table (the text currently says they are only
  "in the analysis scripts").
- [ ] **E9. Abstract wording (93-95).** "showed good fit for a correlated
  three-factor representation compatible with the Czech data" is
  redundant; use "showed good fit for a correlated three-factor model".
- [ ] **E10. 4.7 wording (668).** "Adding equality constraints did not
  worsen global fit" -> "Adding equality constraints did not meaningfully
  worsen fit" (metric CFI dropped by .001).

---

## F. Methods refinements (pre-empting a methods-specialist reviewer)

Neither reviewer felt able to assess the statistics, and Reviewer 2 asked
the recommender to find one who can. These are cheap fixes.

- [ ] **F-a. Protocol step 2 (909).** "Form extreme groups on that
  dimension" recommends dichotomising a continuous attitude. Add "or,
  preferably, model the attitude continuously (for example, in a MIMIC
  model)"; this also matches the preregistered plan.
- [ ] **F-b. 4.8 "tertile split".** The NS groups are 126/37/123 because
  of ties; say "tertile-based split (unequal because of tied scores)".
- [ ] **F-c. 4.8 criteria.** It reports Delta SRMR while the stated
  criteria are Delta CFI and Delta RMSEA; report Delta RMSEA as well.
- [ ] **F-d. 5.2 (842).** "under a near-saturated model" is inaccurate for
  df = 24; use "with three indicators per factor".
- [ ] **F-e. Ethics statement (1067).** Participants include 11-15 year
  olds. If school-principal approval or parental information/consent was
  obtained, say so in one sentence.

---

## G. Citation verification (repository rule)

- [ ] **G1.** Create or complete R/ notes (read) for citations that answer
  reviewers and currently have no note in the vault:
  `Huijgen_et_al2014Testing`, `Huijgen_et_al2017Toward`, `RVPZV2023`,
  Loevinger (1954, new), `Hartmann2008` (if kept per A3), and Wilschut
  and Schiphorst (2019) if cited per A6. `Endacott_Brooks2013An_Updated`
  is a stub marked "needs full read".
- [ ] **G2.** Also check notes for the other cited works without notes
  found in this review: `Barton2004`, `Evans2013`, `Duckitt2001`,
  `Suedfeld1977`, `Hays_et_al1989A_Five`, `Beierlein_et_al2014Die_Kurzskala`,
  `Decker_et_al2013Rechtsextremismus`, `Cohen1988`.
- [ ] **G3.** Run `reference-verifier` on `manuscript.tex` and
  `response_to_reviewers.md` (plan 2026-10-02 Task 6 is still open).
- [ ] **G4.** After C1-C2, confirm whether `Hartmann2008` is still cited;
  if not, it drops out of the reference list automatically.

---

## H. Response letter, cover letter, TOP form, supplement

### Response letter (`response_to_reviewers.md`)

- [ ] **F1 / META-01. Replace the narrative with a short table** of
  changes to reported results since the submitted version, followed by
  two or three sentences. Do not describe intermediate drafts (for
  example the bifactor removal and restoration; reviewers never saw it).
  Suggested rows:

  | Item | Submitted version | Revised version |
  |---|---|---|
  | Three-factor CFA fit | CFI .999, RMSEA .011 (unscaled) | CFI .975, RMSEA .039 (scaled WLSMV); described as good, not near-perfect |
  | Bifactor vs. three-factor | Delta CFI .001, "no meaningful improvement" | Delta CFI +.018; fit favours bifactor; structural claims softened |
  | MG-CFA admissibility | Heywood cases in Low group | Solutions admissible; Heywood claim withdrawn |
  | Focal FR-LF coefficient | beta = .012, p = .863 | beta = .016, 95% CI [-.117, .149], p = .817, n = 282 (one scoring rule applied throughout); singular fit disclosed |
  | Design-effect argument | DEFF 1.56/2.16 used to justify inference | Removed (it used classroom size with school ICCs) |
  | ICCs | Single value | School / class-within-school / total |
  | DIF description | Uniform/non-uniform | Omnibus joint test; raw POP in GRM disclosed |
  | Modification index | Read as loading non-invariance | Read as POP2 cross-loading diagnostic only |
  | Authoritarianism-presentism | Not reported | Reported as exploratory, unadjusted |
  | Preregistration disclosure | TOP: one deviation (TOST bound) | Table S5 lists omitted registered analyses (MIMIC, moderation, covariates, one-tailed tests, BH correction) and post-registration additions |
  | Repository | OSF project | Zenodo (OSF project functionality discontinued) |

- [ ] **F2.** Call out the preregistration-disclosure correction openly in
  one or two sentences; it corrects a statement in the submitted TOP form.
- [ ] **F3.** REC-01 and REC-05: rewrite per D1 and D2.
- [ ] **F4.** R1-04: rewrite. The Czech instrument was translated from the
  English article; the German dissertation is cited only as the original
  development source (or not at all, per A3). Mention that the method
  description was corrected (the earlier revision draft is irrelevant to
  reviewers; compare against the submitted text, which already said
  English).
- [ ] **F5.** R1-02: point to the condensed mechanism paragraph (E3).
- [ ] **F6.** Add replies for D4 (Dutch grade result, reliability remark,
  Wilschut and Schiphorst) and D5 (group means, RQ labels).
- [ ] **F7.** Add page and line references from the final clean PDF to
  every reply (the PDF has line numbers); update the location map (it
  lists Tables S1-S5 but S3b and S4b also exist; table numbers change if
  E1-E2 are done).
- [ ] **F8.** Apply the A7 pronoun decision.

### Cover letter (`cover_letter.md`)

- [ ] Replace the OSF DOI with the Zenodo DOI (B5).
- [ ] Shorten the results paragraph; it repeats the abstract's caveats.
- [ ] Mention the preregistration-disclosure correction in one sentence.
- [ ] Apply A7.

### TOP form (manuscript appendix) and `top_disclosure_table.md`

- [ ] **Preprint DOI (1118).** Use the unversioned DOI
  `https://doi.org/10.31234/osf.io/hxngm` (resolves to the latest version)
  or the new `hxngm_v3` after J1; `hxngm_v2` is the April submission.
- [ ] Repository links per B5.
- [ ] Update `test_revision_reporting_analyses.R:201, 272` for the new
  preprint DOI.

### Supplement

- [ ] Add the NS-only fit table (E8) and ideology group means (A5) if
  adopted.
- [ ] Check that all table cross-references match after E1-E2.
- [ ] Sync `supplementary_materials.md/.pdf` into `osf_storage/` (or the
  renamed package folder).

---

## I. Rebuild and verify

- [ ] **I1.** Run `test_revision_reporting_analyses.R` and
  `verify_statistics.R`; all pass with the new DOIs.
- [ ] **I2.** Confirm no statistic changed (none of the edits above should
  change one). If anything changes, rerun the pipeline and update every
  location.
- [ ] **I3.** Rebuild `manuscript.pdf`, `preprint.pdf`,
  `supplementary_materials.pdf`, `manuscript_diff.tex/.pdf`
  (`build_preprint.sh`, `build_redline.sh`; the redline must compare
  against the submitted version `60d4c5f`).
- [ ] **I4.** Inspect the PDFs: no `??` or `(?)`, tables fit, renumbered
  tables and figure referenced correctly, Figure 1 legible in greyscale,
  line numbers present, references list complete (Loevinger 1954 present).
- [ ] **I5.** Search the built text for leftover strings: `OSF.IO/YNG37`
  (outside provenance), `Zenodo; it has not`, `German instrument`,
  `German dissertation`, `current reproducible`, `hxngm_v2`,
  `understandable given`, `285--293`, `Cohen`'s "small".
- [ ] **I6.** Read the final clean PDF once end to end.
- [ ] **I7.** Optional: targeted human psychometric review (still open in
  `status.md`).

---

## J. Post and resubmit

- [ ] **J1.** Upload the revised `preprint.pdf` to PsyArXiv as a new
  version of `hxngm` (v3); update title or abstract metadata there if
  changed; link the Zenodo DOI in the PsyArXiv record.
- [ ] **J2.** Publish the Zenodo record (B6) if not done.
- [ ] **J3.** On the PCI Psychology site, submit the revision: link to the
  new preprint version, the response letter (as text or PDF), the
  tracked-changes PDF (`manuscript_diff.pdf`), and the cover letter.
- [ ] **J4.** Check the PCI submission form fields for repository links
  (data, code, materials) and update them to Zenodo.

---

## K. Housekeeping

- [ ] Update `status.md` and `project_status.md` (stage, Zenodo DOI,
  PsyArXiv v3, resubmission date).
- [ ] Update project `CLAUDE.md`: current title, repository (Zenodo),
  desk-rejection list.
- [ ] Tick the validation boxes in `reviews/revision_ledger.md` or mark it
  superseded by this checklist.
- [ ] Mark `docs/superpowers/plans/2026-10-02-pci-final-corrections.md`
  Task 6 citation step done after G3.
- [ ] Commit in the submodule, push, then update the pointer in the root
  repository and push.
- [ ] Set this checklist's `Status:` to `Shipped` after resubmission.
