# PCI Psychology resubmission checklist

**Status:** Approved
**Created:** 2026-10-20; decisions A1-A7 recorded 2026-10-20
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

## Progress (2026-10-20)

Remaining items are tracked in `final_resubmission_checklist.md`; this file
stays the record of the A-K work already done.

Sections A-F, H (except F7) and most of B are done and pushed. Open items:

- **Blocked on Juda:** G1/G2 reading and R/ notes; Zotero export of
  `Wilschut_Schiphorst2019One` (still renders as "(?)") and of Loevinger
  (1954) (TODO comment in 5.3); Zotero casing fixes for Beierlein et al.
  (2014) and Decker et al. (2013) (Language = German) and Aktin et al.
  (2026) ("Turkish", "November"). `extras.bib` keys stay as they are
  (decided 2026-10-20).
- G3 run 2026-10-20: six claims reworded. Still open: 13 `extras.bib`
  keys are not in Zotero (Chen2007, Cheung2002, Finch2005, Flora2004,
  Hambleton2005, ITC2017, Jost2003, Kahan2017, Duckitt2001, Suedfeld1977,
  Evans2013, Shemilt1984, Barton2004); 35 cited works lack an annotated R/
  note; `references.bib` is unused and stale.
- **Then:** final rebuild (I3-I4), page and line references in the
  response letter (F7), I6 read-through, J1 PsyArXiv v3, switch the Zenodo
  preprint identifier to `hxngm_v3`, B6 publish, J3-J4, K.
- The build now uses Biber with biblatex-apa because the root
  `bibliography.bib` is a Better BibLaTeX export; the 2026-10-20 rebuild in
  commit `cb95472` had every citation as "n.d.".
- The unversioned preprint DOI does not resolve; the TOP form, README and
  tests use <https://osf.io/preprints/psyarxiv/hxngm>.
- B3 deviation from A1: third-party instrument files and item text are
  excluded from the Zenodo package pending redistribution permission.
- B7: OSF storage confirmed empty via the API; the pointer note on the OSF
  project is not yet confirmed.
- Done 2026-10-20: the MG-CFA is now described as the registered
  tertile-group H4 sensitivity model, extended from the CONT items to the
  full three-factor model (3.4, 4.7, 5.4, 5.8, TOP form, Table S5, META-01).
- Zotero (2026-10-20): Loevinger1954Attenuation added; Beierlein and
  Decker titles and language fixed; Aktin proper nouns protected;
  Wilschut_Schiphorst2019One was already in collection R. All need a
  Zotero desktop sync and Better BibTeX export to reach bibliography.bib.

---

## A. Decisions (made 2026-10-20)

- [x] **A1. Zenodo licence:** CC BY 4.0 for data, documentation and
  instruments; MIT for code. Zenodo takes one licence field: set CC BY 4.0
  there and state the split in the README and description (B2, B4).
  Still check that redistributing the Czech translations of FR-LF, KSA-3
  and SDR-5 is compatible with the original instruments' terms (B3).
- [x] **A2. Old OSF files:** delete all files from OSF project storage and
  leave a note pointing to the Zenodo DOI (B7). The April data very likely
  contain the original class labels (`septima`, `G1.E`, `I3.A`), and OSF
  keeps old file versions downloadable, so overwriting is not enough. The
  project page, its DOI and the preregistration stay.
- [x] **A3. Translation source and dissertation:** the Czech version was
  translated from the published **English** article. Cite
  `Hartmann_Hasselhorn2008Historical` everywhere; mention `Hartmann2008`
  (German dissertation) once, as the instrument's original development
  source (C1, C2).
- [x] **A4. Upper-secondary curriculum (R1-08):** keep the disclosure that
  the documentary account covers only the lower-secondary subsample;
  shorten Section 2.2 (E5). No new sources.
- [x] **A5. Ideology group means (R2-03):** add a three-row supplement
  table (Low / Middle / High composite-ideology tertile: n, mean HPT
  composite, 95% CI), computed in `revision_reporting_analyses.R` (D5).
- [x] **A6. Wilschut and Schiphorst (2019):** cite, using the author's
  assessment in `docs/reviews/2026-10-20-wilschut-schiphorst-2019.md`
  (citation key `Wilschut_Schiphorst2019One`). Placements (a) Section 2.1
  and (b) Section 2.3 are required; (c) and (d) are optional (D4, C3, G1,
  F6). Use it for its conceptual critique and as a second transfer
  failure, not as strong psychometric evidence, and not for the
  cognitive-versus-affective claim.
- [x] **A7. Voice:** "I" in the manuscript, response letter and cover
  letter (D7, F8).

---

## B. Move the replication package from OSF to Zenodo

OSF has discontinued project functionality. The preregistration
(`https://osf.io/zsngy`, public, registered 2025-11-27) and the PsyArXiv
preprint (`hxngm`) are unaffected and keep their identifiers.

- [ ] **B1. Freeze the package contents.** Finish all analysis-affecting
  edits first (none of the items below should change statistics; if one
  does, rerun the pipeline before uploading).
- [x] **B2. Create a Zenodo draft and reserve the DOI.** Use "Reserve DOI"
  so the DOI can go into the manuscript before publishing the record.
  Metadata:
  - [x] Title (match the manuscript title), creator with ORCID
    0000-0003-0043-4039 and affiliation (Charles University, Faculty of
    Arts, Institute of History), resource type "Dataset" (or "Other" with
    a clear description), publication date, version `1.0`.
  - [x] Description: what the package contains, execution order, and the
    mapping to manuscript tables and figures (reuse the README text).
  - [x] Licence field: CC BY 4.0; state in the description that code is
    MIT-licensed (A1). Add a `LICENSE` file (MIT) to `scripts/` and a
    licence section to the README.
  - [x] Related identifiers: preregistration `https://osf.io/zsngy`
    (relation: "is supplement to" or "references"); preprint
    `10.31234/osf.io/hxngm` (relation: "is supplement to"); the old OSF
    project `10.17605/OSF.IO/YNG37` (relation: "is new version of" or
    "is derived from"), for provenance.
  - [x] Keywords as in the manuscript.
- [x] **B3. Prepare the upload from `osf_storage/` only.** Never include
  repository-root files (`*CONFIDENTAL*`, teacher reports,
  `normalised_responses.*`, `Research Log.docx`, recruitment material).
  - [x] Confirm the data files use the opaque `C01`-`C20` class codes and
    contain no other identifying field.
  - [x] Confirm the package holds everything the manuscript promises:
    data (RDS, xlsx), codebook PDF, instruments, five analytic Rmd reports,
    two documentation reports, `scoring_helpers.R`,
    `revision_reporting_analyses.R`, four figure scripts, two
    supplementary scripts, Makefile, supplement (Tables S1-S5, S3b, S4b),
    `instrument_adaptation_and_deviations.md`, README.
  - [ ] Optionally rename the folder from `osf_storage/` to something
    repository-neutral (for example `replication_package/`); if so, update
    the Makefile, `test_revision_reporting_analyses.R` and
    `docs/study-and-pipeline.md` paths in the same commit.
  - [x] Upload as a single zip plus the README at top level, or as loose
    files; keep the directory structure the README documents.
- [x] **B4. Update `osf_storage/README.md`.** Replace the OSF project link
  with the Zenodo DOI; keep the OSF DOI only as provenance; keep the
  preregistration and preprint links. Remove "prepared for migration"
  language.
- [x] **B5. Replace every OSF-project reference in the submission files.**
  - [x] `manuscript.tex:331` "OSF repository
    (https://doi.org/10.17605/OSF.IO/YNG37)"
  - [x] `manuscript.tex:657` "Table S1 in the OSF supplementary materials"
    (Table S1 is in the supplement that accompanies the manuscript; say so)
  - [x] `manuscript.tex:725` "analysis scripts in the OSF data and code
    repository"
  - [x] `manuscript.tex:1045-1057` Data Availability: cite the Zenodo DOI;
    delete "currently available at OSF and is prepared for a planned
    archival migration to Zenodo; it has not yet been migrated"
  - [x] TOP form: `manuscript.tex:1135, 1151, 1183, 1202, 1210, 1229, 1239`
    (also delete the Zenodo-migration clause at 1202)
  - [x] Keep `https://osf.io/zsngy` at 312, 347, 1056 and 1256; consider
    "preregistered (https://osf.io/zsngy)" instead of "preregistered on
    OSF" so the text does not imply the materials are on OSF
  - [x] `cover_letter.md:42`
  - [x] `top_disclosure_table.md:15-17, 36`
  - [x] `test_revision_reporting_analyses.R:274, 277, 655` (assertions
    expect the OSF DOI; switch them to the Zenodo DOI)
  - [x] `docs/study-and-pipeline.md`, `CLAUDE.md`, `project_status.md`
- [ ] **B6. Publish the Zenodo record** after the final rebuild (I3) so
  the archived PDFs and outputs match the submitted manuscript. Check that
  the DOI resolves and that every file downloads.
- [ ] **B7. Clean up OSF** (A2): delete every file in OSF project storage
  (including `preprint.pdf`, `supplementary_materials.md` and the data
  folder), then confirm through the API that storage is empty
  (`https://api.osf.io/v2/nodes/yng37/files/osfstorage/`). Add a short
  description or wiki note on the project pointing to the Zenodo DOI, if
  OSF still allows edits; otherwise put the pointer in the Zenodo record
  only. Do not touch the registration `zsngy`.
- [x] **B8. Mention the move in the response letter** (one line in the
  META entry, see F1).

---

## C. Factual corrections (must fix)

- [x] **C1. Translation source (325).** Revert to the submitted wording,
  which was correct: "Two bilingual Czech-English experts conducted
  independent forward translations from the published English version of
  the instrument \citep{Hartmann_Hasselhorn2008Historical}." Remove "from
  the German instrument reported in \citet{Hartmann2008}; wording was
  cross-checked against the published English validation article".
- [x] **C2. Other German-source statements.** Make consistent with C1:
  - [x] `manuscript.tex:330` "reverse-scoring of POP items from the German
    source" -> "from the original instrument"
  - [x] `manuscript.tex:362` "Czech adaptation of the scenario-based HPT
    instrument from the German dissertation \citep{Hartmann2008}, with the
    published English account used as supporting validation evidence" ->
    "Czech adaptation of the Hartmann and Hasselhorn (2008) instrument"
  - [x] Per A3, keep exactly one mention of `Hartmann2008`, as the
    instrument's original development source. Suggested place: 3.1, for
    example "The instrument was developed in Hartmann's (2008) dissertation
    and published by \citet{Hartmann_Hasselhorn2008Historical}; the Czech
    version was translated from the published English article."
  - [x] `osf_storage/instrument_adaptation_and_deviations.md:5-9` (says
    "adapted from the German ... instrument ... reported in Hartmann
    (2008)")
  - [x] Response R1-04 (see F4) and ledger R1-04 note
  - [x] Check the supplement Table S4 row for HPT (line 117) still reads
    correctly ("German" there refers to the original population; fine)
- [x] **C3. Invented example item (268).** "His decision was understandable
  given the political and economic turmoil of the Weimar Republic" is not
  an instrument item, and the task never says Hannes decided. Replace the
  example with the real option alignment, citing Wilschut and Schiphorst
  (A6, placement b): every CONT option describes Hannes as likely to vote
  for the NSDAP and every POP option describes him as rejecting it, so the
  POP-CONT continuum coincides with the direction of the political choice.
  Draft wording:
  "In the Weimar scenario, every contextualization option describes Hannes
  as likely to vote for the NSDAP and every present-oriented option
  describes him as rejecting it, so the POP--CONT continuum coincides with
  the direction of the political choice
  \citep{Wilschut_Schiphorst2019One}. A student can therefore reach a high
  score either by reconstructing the 1930 context or by finding an
  anti-democratic vote unobjectionable."
  Optionally quote one real item as illustration, for example CONT2
  ("Hitler probably represents a strong leader to him. He will probably
  not think much about the dangers associated with the NSDAP."). Keep the
  manuscript's exact item-stem wording ("could vote for an anti-democratic
  party such as the NSDAP"), not Wilschut and Schiphorst's paraphrase
  ("whether or not to vote for Hitler"). Check the following sentences in
  2.3 still read correctly.
- [x] **C4. Sample-size claim (347).** "the achieved N = 285--293 met that
  regression target" conflicts with the focal model n = 282. Replace with
  "the focal regression used n = 282 students, above the registered
  minimum of 250".
- [x] **C5. "Registered one- and two-factor alternatives" (835).** The
  preregistration audit lists no registered CFA comparison. Verify against
  `https://osf.io/zsngy`; if not registered, delete "registered".
- [x] **C6. TOST bound (800).** "+-0.20 ... corresponding to Cohen's
  threshold for a 'small' effect" is wrong for standardized coefficients
  (Cohen's small is r = .10; .20 is small for d). Replace with a neutral
  justification, for example "set post hoc at +-0.20 standardized units,
  which we treated as the smallest practically relevant effect", and keep
  the existing disclosure that this exceeds the .15 power target.
- [x] **C7. Table 7 note (769).** Remove the definitions of FR-LF, KSA-3
  and SDR-5 (they no longer appear in the table); keep scale ranges if
  useful.

---

## D. Reviewer-coverage gaps

### D1. REC-01: content validity (the recommender's main concern)

- [x] Split limitation 1 (953-957). Make **content validity** its own,
  first limitation; move "single country" into the sampling limitation
  (item 5).
- [x] Use the terms "content validity" and "construct underrepresentation"
  \citep{Messick1995Validity} explicitly.
- [x] Say what the scores license: claims about performance on the Weimar
  voting task, not about HPT as a general competence.
- [x] Keep the existing points (nine ratings are not nine content samples;
  scenario-specific knowledge and moral salience) and the
  multi-scenario recommendation; add that the fairness result is equally
  scenario-bound.
- [x] Carry one clause into 5.8 Conclusion ("for this scenario").
- [x] Suggested wording (5-6 sentences, adapt freely):
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
- [x] Update response REC-01 to quote the new heading and location.

### D2. REC-05: reliability framing

- [x] Read Loevinger (1954), "The attenuation paradox in test theory",
  *Psychological Bulletin*, 51(5), 493-504; create the R/ note; cite it
  (the recommender named the attenuation paradox; the current citation is
  Loevinger, 1957).
  Done 2026-10-20: cited in 5.3 for the paradox only (pp. 493-494, 504);
  PDF in vault `L/Loevinger1954Attenuation.pdf`. The R/ note is still to
  be written.
- [x] Rewrite the 5.3 opening (849) to follow the recommender's framing:
  start with the breadth-precision trade-off and the attenuation paradox,
  not with "fall below conventional benchmarks". Keep one sentence
  acknowledging that precision is modest.
- [x] Use the phrase "attenuation paradox" explicitly.
- [x] Re-check the Loevinger paraphrase ("repeated versions of a narrow
  question") against the source; the 2026-09-11 audit flagged it.
- [x] Add the consequence the recommender asked for: modest reliability
  attenuates validity coefficients, so the knowledge correlation is a
  lower bound **and the ideology nulls are also attenuated**, which
  limits the strength of the fairness evidence. One or two sentences.
- [x] Rewrite response REC-05: drop "rather than an acceptable consequence
  of brevity" (it reads as contradicting the recommender); say you adopted
  his suggestion, cite the location, and state the validity consequence.

### D3. R1-02: protocol focus over pathways

- [x] Covered by E3 (condense the mechanism paragraphs in 5.4-5.5).
- [x] Remove "the instrument's resistance to attitudinal contamination"
  (921); it overstates the evidence.

### D4. Reviewer 1: smaller unanswered points

- [x] Dutch grade result: R1 noted no significant correlation with school
  history grades in the Dutch work; here history grade correlated
  r = -.153. Add one sentence in 4.9 or 5.4 contrasting the two, and one
  line in the response.
- [x] R1's "It is interesting that the issues with reliability are still
  present": acknowledge in response REC-05 or R1-06 (link to Huijgen et
  al., 2014, weaker reliability on transfer).
- [x] Wilschut and Schiphorst (2019), per A6 and the author's assessment
  in `docs/reviews/2026-10-20-wilschut-schiphorst-2019.md`:
  - [x] (a) Section 2.1, after the Huijgen et al. (2014) sentence (around
    217): "\citet{Wilschut_Schiphorst2019One} adapted the format to two new
    scenarios (a nineteenth-century strike and an eleventh-century crusade)
    with 69 Dutch Grade 10 students; principal components did not recover
    the three response categories, and total-score reliability was low
    ($\alpha$ = .22 and .50)."
  - [x] (b) Section 2.3 congruence pathway: see C3.
  - [x] (c) Optional, 5.3 or 5.6, one clause: Czech reliabilities are
    modest but higher than in the Dutch new-content adaptation, consistent
    with the structure depending on the original scenario; supports the
    multi-scenario recommendation in D1.
  - [x] (d) Optional, 5.7 or limitation 8: alongside Huijgen et al.
    (2017), closed ratings can miss change that written explanations
    detect; supports R1's think-aloud suggestion.
  - [ ] Optional for R1-03: W&S as the rationalist counterpoint to
    Endacott and Brooks (2013) in the construct-boundary paragraph.
  - [x] Do not cite W&S for "cognitive rather than affective" HPT.

### D5. Reviewer 2

- [x] R2-03 (A5): add the tertile group-means output to
  `revision_reporting_analyses.R` (Low / Middle / High composite-ideology
  tertile: n, mean HPT composite, 95% CI); add a regression test in
  `test_revision_reporting_analyses.R`; write it to `revision_outputs/`;
  add it as a new supplement table; add one sentence after Figure 1
  pointing to it; mention it in response R2-03, together with one sentence
  on why the continuous display stays primary (tertiles are arbitrary
  cut-points).
- [x] R2-01: label the two questions "RQ1" and "RQ2" in 2.5 (303-309) and
  reuse the labels in the Results roadmap (471-474), 4.9/4.10 and 5.1.

### D6. REC-02 and REC-03 polish

- [x] REC-02: add "differential prediction of external criteria" as an
  explicit future-research item in 5.7 Recommendations (currently only in
  4.1 and protocol step 5).
- [x] REC-03: Table 7 headers (753) "HPT" -> "HPT composite", "Context"
  -> "Contextualization".

### D7. Voice (A7)

- [x] Change the six "we" sentences in `manuscript.tex` to "I": 155 "we
  conducted", 177 "We follow", 408 "We computed", 525 "We therefore
  retain", 709 "we repeated", 849 "we recommend". Re-search for
  `we`/`our`/`us` as whole words after the other edits.
- [x] Same in `response_to_reviewers.md` (12 occurrences); the supplement
  has none.

---

## E. Simplifications

- [x] **E1. Delete Table 2 (536-550).** It repeats the sentence above it;
  full loadings are in Table S2. Keep the prose sentence and point to S2.
- [x] **E2. Delete Table 3 (557-571).** It repeats the 4.3 prose (the
  Mosier interval appears twice). Fold the two composite omegas into the
  prose and point to S3. Renumber Tables 4-7 to 2-5 and fix every
  reference (manuscript, supplement, response, ledger).
- [x] **E3. Condense the mechanism discussion.** Merge 5.4 "cancelling
  pathways" (871-880) and the three 5.5 paragraphs (918-943) into one
  paragraph of about five sentences: the null may reflect a genuine null,
  cancelling pathways, format effects, or task-demand compliance; the
  present data cannot distinguish them; limitation 8 says how to test them.
- [x] **E4. Remove revision-process wording from the manuscript.**
  - [x] 855 "in the current reproducible analysis"
  - [x] 668 "All three models converged and passed lavaan's
    post-estimation admissibility check; the diagonals of their
    model-implied residual-variance matrices were positive." -> "All three
    solutions were admissible."
  - [x] Zenodo-migration clauses (covered by B5)
- [x] **E5. Merge the two curriculum paragraphs in 2.2 (243-259)** into
  about three sentences; keep the lower-secondary boundary as one
  sentence (A4: no new sources).
- [x] **E6. Shorten the composite label.** Define "primary six-item HPT
  composite (hereafter, HPT composite)" once in 3.3.1; use "HPT
  composite" in the other 13 places, and harmonise Table 4/7 and the
  Figure 1 caption.
- [x] **E7. Reduce repeated caveats.** "Small extreme groups and unmodeled
  clustering" appears in the abstract, 4.7, 5.1, 5.4 and 5.8. Keep it in
  4.7 (full) and the abstract and 5.8 (short); cut it from 5.1 and 5.4.
- [x] **E8. Shorten 4.8 (NS-only grouping)** to two sentences; move the
  fit values to a supplement table (the text currently says they are only
  "in the analysis scripts").
- [x] **E9. Abstract wording (93-95).** "showed good fit for a correlated
  three-factor representation compatible with the Czech data" is
  redundant; use "showed good fit for a correlated three-factor model".
- [x] **E10. 4.7 wording (668).** "Adding equality constraints did not
  worsen global fit" -> "Adding equality constraints did not meaningfully
  worsen fit" (metric CFI dropped by .001).

---

## F. Methods refinements (pre-empting a methods-specialist reviewer)

Neither reviewer felt able to assess the statistics, and Reviewer 2 asked
the recommender to find one who can. These are cheap fixes.

- [x] **F-a. Protocol step 2 (909).** "Form extreme groups on that
  dimension" recommends dichotomising a continuous attitude. Add "or,
  preferably, model the attitude continuously (for example, in a MIMIC
  model)"; this also matches the preregistered plan.
- [x] **F-b. 4.8 "tertile split".** The NS groups are 126/37/123 because
  of ties; say "tertile-based split (unequal because of tied scores)".
- [x] **F-c. 4.8 criteria.** It reports Delta SRMR while the stated
  criteria are Delta CFI and Delta RMSEA; report Delta RMSEA as well.
- [x] **F-d. 5.2 (842).** "under a near-saturated model" is inaccurate for
  df = 24; use "with three indicators per factor".
- [x] **F-e. Ethics statement (1067).** Participants include 11-15 year
  olds. If school-principal approval or parental information/consent was
  obtained, say so in one sentence.

---

## G. Citation verification (repository rule)

- [ ] **G1.** Create or complete R/ notes (read) for citations that answer
  reviewers and currently have no note in the vault:
  `Huijgen_et_al2014Testing`, `Huijgen_et_al2017Toward`, `RVPZV2023`,
  Loevinger (1954, new), `Hartmann2008` (kept as one mention per A3), and
  `Wilschut_Schiphorst2019One` (PDF in vault `L/`; write the note from
  your own reading, using the assessment in
  `docs/reviews/2026-10-20-wilschut-schiphorst-2019.md` as a starting
  point). Make sure each new entry reaches `bibliography.bib` via Zotero.
  `Endacott_Brooks2013An_Updated`
  is a stub marked "needs full read".
- [ ] **G2.** Also check notes for the other cited works without notes
  found in this review: `Barton2004`, `Evans2013`, `Duckitt2001`,
  `Suedfeld1977`, `Hays_et_al1989A_Five`, `Beierlein_et_al2014Die_Kurzskala`,
  `Decker_et_al2013Rechtsextremismus`, `Cohen1988`.
- [x] **G3.** Run `reference-verifier` on `manuscript.tex` and
  `response_to_reviewers.md` (plan 2026-10-02 Task 6 is still open).
- [x] **G4.** After C1-C2, confirm whether `Hartmann2008` is still cited;
  if not, it drops out of the reference list automatically.

---

## H. Response letter, cover letter, TOP form, supplement

### Response letter (`response_to_reviewers.md`)

- [x] **F1 / META-01. Replace the narrative with a short table** of
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

- [x] **F2.** Call out the preregistration-disclosure correction openly in
  one or two sentences; it corrects a statement in the submitted TOP form.
- [x] **F3.** REC-01 and REC-05: rewrite per D1 and D2.
- [x] **F4.** R1-04: rewrite. The Czech instrument was translated from the
  English article; the German dissertation is cited only as the original
  development source (or not at all, per A3). Mention that the method
  description was corrected (the earlier revision draft is irrelevant to
  reviewers; compare against the submitted text, which already said
  English).
- [x] **F5.** R1-02: point to the condensed mechanism paragraph (E3).
- [x] **F6.** Add replies for D4 and D5:
  - [x] R1-06: Wilschut and Schiphorst (2019) now cited in 2.1 as a second
    transfer study with low reliability, and in 2.3 for the alignment of
    option content with the voting decision.
  - [x] R1 reliability remark: "As Reviewer 1 notes, reliability problems
    recur; Wilschut and Schiphorst (2019) and Huijgen et al. (2014) report
    weaker reliability when the format is moved to new content, and
    Section 5.3 now places the Czech estimates against these."
  - [x] R1 Dutch grade remark: here history grade correlated r = -.15.
  - [x] R2-03: group-means table (A5); R2-01: RQ labels.
- [ ] **F7.** Add page and line references from the final clean PDF to
  every reply (the PDF has line numbers); update the location map (it
  lists Tables S1-S5 but S3b and S4b also exist; table numbers change if
  E1-E2 are done).
- [x] **F8.** Use "I" throughout (A7).

### Cover letter (`cover_letter.md`)

- [x] Replace the OSF DOI with the Zenodo DOI (B5).
- [x] Shorten the results paragraph; it repeats the abstract's caveats.
- [x] Mention the preregistration-disclosure correction in one sentence.
- [x] Keep "I" (A7; already the case).

### TOP form (manuscript appendix) and `top_disclosure_table.md`

- [x] **Preprint DOI (1118).** Use the unversioned DOI
  `https://doi.org/10.31234/osf.io/hxngm` (resolves to the latest version)
  or the new `hxngm_v3` after J1; `hxngm_v2` is the April submission.
- [x] Repository links per B5.
- [x] Update `test_revision_reporting_analyses.R:201, 272` for the new
  preprint DOI.

### Supplement

- [x] Add the NS-only fit table (E8) and ideology group means (A5) if
  adopted.
- [x] Check that all table cross-references match after E1-E2.
- [x] Sync `supplementary_materials.md/.pdf` into `osf_storage/` (or the
  renamed package folder).

---

## I. Rebuild and verify

- [x] **I1.** Run `test_revision_reporting_analyses.R` and
  `verify_statistics.R`; all pass with the new DOIs.
- [x] **I2.** Confirm no statistic changed (none of the edits above should
  change one). If anything changes, rerun the pipeline and update every
  location.
- [ ] **I3.** Rebuild `manuscript.pdf`, `preprint.pdf`,
  `supplementary_materials.pdf`, `manuscript_diff.tex/.pdf`
  (`build_preprint.sh`, `build_redline.sh`; the redline must compare
  against the submitted version `60d4c5f`).
- [ ] **I4.** Inspect the PDFs: no `??` or `(?)`, tables fit, renumbered
  tables and figure referenced correctly, Figure 1 legible in greyscale,
  line numbers present, references list complete (Loevinger 1954 present).
- [x] **I5.** Search the built text for leftover strings: `OSF.IO/YNG37`
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
- [x] Update project `CLAUDE.md`: current title, repository (Zenodo),
  desk-rejection list.
- [x] Tick the validation boxes in `reviews/revision_ledger.md` or mark it
  superseded by this checklist.
- [ ] Mark `docs/superpowers/plans/2026-10-02-pci-final-corrections.md`
  Task 6 citation step done after G3.
- [ ] Commit in the submodule, push, then update the pointer in the root
  repository and push.
- [ ] Set this checklist's `Status:` to `Shipped` after resubmission.
