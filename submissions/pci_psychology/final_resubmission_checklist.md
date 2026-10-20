# PCI Psychology: final resubmission checklist

**Status:** Approved
**Created:** 2026-10-20
**Basis:** Review of `resubmission_checklist.md` against the repository state
at commit `8d5e9c4`, the built PDFs, the test suite and the public Zenodo,
OSF and PsyArXiv records. This list covers only what is still open; the
earlier checklist stays the record of the A-K work already done.

Order: section 1 (easy fixes, Claude) -> section 2 (Juda) -> section 3 (final
build, Claude) -> section 4 (publication and submission, Juda with Claude).
Section 3 has to wait for section 2 because the Zotero fixes change the
reference list.

Already verified, no action needed: no leftover strings (old OSF DOI,
"German instrument/dissertation", `hxngm_v2`, invented example item,
"285--293", "we/our"); no `(?)`, `??` or "n.d." in the PDF; both test
scripts pass; `manuscript.pdf` and `preprint.pdf` are current and identical;
the redline baseline `c952aa5` has the same manuscript text as the submitted
`60d4c5f`; OSF project storage is empty.

---

## 1. Easy fixes (Claude, no input needed)

- [ ] **1.1 Table 1 width.** Table 1 (CFA fit indices, `manuscript.tex`
  around line 492) runs about 1.2 cm into the right margin. Shorten the
  bifactor row label (for example "Bifactor (post-registration)") or set
  the table in `\small`.
- [ ] **1.2 Table 5 width.** Table 5 (correlations, around line 713) runs
  about 1.6 cm into the right margin. Narrow the first column or wrap the
  column headers so the table fits the text width.
- [ ] **1.3 `status.md`.** "Zenodo hosts the public reproducibility
  package" is not true until the record is published; say the DOI is
  reserved and the record is unpublished. Update Next Steps to point to
  this checklist.
- [ ] **1.4 `project_status.md`.** Same correction: line 183 marks the move
  to Zenodo as done; change it to "DOI reserved, publication pending".
- [ ] **1.5 Old plan.** Mark the citation step of Task 6 in
  `docs/superpowers/plans/2026-10-02-pci-final-corrections.md` as done
  (reference-verifier ran 2026-10-20; results are in
  `resubmission_checklist.md`).
- [ ] **1.6 Old checklist.** Add one line to `resubmission_checklist.md`
  pointing to this file for the remaining items.
- [ ] **1.7 Optional, R1-03.** Cite Wilschut and Schiphorst (2019) as the
  rationalist counterpoint to Endacott and Brooks (2013) in the
  construct-boundary paragraph, and add one sentence to the R1-03 reply.
  Only if the R/ note in 2.1 supports it.
- [ ] **1.8 Commit and push** each fix in the submodule, then the root
  pointer.

---

## 2. Juda: reading, Zotero and decisions

- [ ] **2.1 R/ notes for works cited in the replies** (repository rule;
  former G1). Write from your own reading:
  - [ ] `Huijgen_et_al2014Testing`
  - [ ] `Huijgen_et_al2017Toward`
  - [ ] `RVPZV2023`
  - [ ] `Loevinger1954Attenuation` (PDF in vault `L/`; cited in 5.3 for the
    attenuation paradox, pp. 493-494, 504)
  - [ ] `Hartmann2008` (one mention, as the instrument's development source)
  - [ ] `Wilschut_Schiphorst2019One` (PDF in vault `L/`; start from
    `docs/reviews/2026-10-20-wilschut-schiphorst-2019.md`)
  - [ ] `Endacott_Brooks2013An_Updated` (existing stub marked "needs full
    read")
- [ ] **2.2 R/ notes for the other cited works without notes** (former G2):
  `Barton2004`, `Evans2013`, `Duckitt2001`, `Suedfeld1977`,
  `Hays_et_al1989A_Five`, `Beierlein_et_al2014Die_Kurzskala`,
  `Decker_et_al2013Rechtsextremismus`, `Cohen1988`. Decide whether this
  must happen before resubmission or can follow.
- [ ] **2.3 Zotero: reference-list order.** Loevinger (1957) is listed before
  Loevinger (1954), and Hartmann and Hasselhorn (2008) before Hartmann
  (2008). The probable cause is a `sortname` value in the Extra field of
  `Loevinger1957Objective` and `Hartmann2008`; remove it from both.
- [ ] **2.4 Zotero: Wilschut and Schiphorst title.** The title starts with a
  straight apostrophe, which renders as a closing quotation mark
  ("'One has to ..."). Use an opening single quotation mark at the start
  (or drop the quotation marks around the quoted phrase).
- [ ] **2.5 Zotero sync and Better BibLaTeX export** so 2.3-2.4 reach
  `bibliography.bib`. Tell Claude when it is done.
- [x] **2.6 Decide on the response-letter locations.** Decided 2026-10-20:
  every reply gets page and line numbers, filled in automatically (3.3).

---

## 3. Final build and verification (Claude, after section 2)

- [ ] **3.1 Rerun the tests:** `test_revision_reporting_analyses.R` and
  `verify_statistics.R` from the project root.
- [ ] **3.2 Rebuild** `manuscript.pdf`, `preprint.pdf` (`build_preprint.sh`)
  and `manuscript_diff.tex/.pdf` (`build_redline.sh`, baseline `c952aa5`).
  Check the Biber log for new warnings (two invalid-ISBN warnings are
  known and harmless).
- [ ] **3.3 Response letter locations** (per 2.6). Can be prepared in
  section 1, before the final build:
  - [ ] Put invisible `\linelabel{...}` markers (from `lineno`) at each
    revised passage in `manuscript.tex`, one per reply.
  - [ ] Put matching placeholders (for example `{{loc:rec01}}`) in
    `response_to_reviewers.md`.
  - [ ] Write a small script that reads the page and line numbers from
    `manuscript.aux` after the build and fills them in as "p. 12,
    lines 340-352"; fail on any unresolved label.
  - [ ] Run it after the final build and check a sample of locations
    against the PDF by eye.
- [ ] **3.4 Inspect the PDFs:** no `(?)`, `??` or "n.d."; Tables 1 and 5
  inside the margins; reference list in APA order (Loevinger 1954 before
  1957, Hartmann 2008 before Hartmann and Hasselhorn 2008); Wilschut title
  quotation marks correct; Figure 1 legible in greyscale.
- [ ] **3.5 Rebuild the Zenodo release bundle.** The bundle in
  `.zenodo-release/` (10:48) predates the last README (11:14) and supplement
  (11:19) changes. Rebuild it from the final package and check the
  manifest, file count and checksums.
- [ ] **3.6 Replace the files in the Zenodo draft** with the rebuilt bundle.
  The draft stays unpublished.
- [ ] **3.7 Commit and push** the rebuilt files (submodule, then root
  pointer).

---

## 4. Publication and submission (Juda, with Claude where possible)

- [ ] **4.1 Read the final clean PDF end to end** (former I6).
- [ ] **4.2 Optional:** targeted human psychometric review (former I7).
- [ ] **4.3 Approve Zenodo publication (Gate C)** and publish record
  23494026. Then check that
  <https://doi.org/10.5281/zenodo.23494026> resolves and every file
  downloads. This must happen before 4.5 because the manuscript, TOP form
  and cover letter describe the package as available.
- [ ] **4.4 Post PsyArXiv version 3** of `hxngm` with the final
  `preprint.pdf`; check the title and abstract metadata; link the Zenodo
  DOI in the PsyArXiv record.
- [ ] **4.5 Update the Zenodo related identifier** for the preprint to
  `hxngm_v3` (Zenodo allows metadata edits after publication).
- [ ] **4.6 Submit the revision on PCI Psychology:** link the PsyArXiv v3
  version; upload `response_to_reviewers.md` (as text or PDF),
  `manuscript_diff.pdf` and the cover letter.
- [ ] **4.7 Check the PCI form fields** for data, code and materials links
  and set them to the Zenodo DOI.

---

## 5. Housekeeping (Claude, after submission)

- [ ] Update `status.md` and `project_status.md`: stage "Revision
  submitted", resubmission date, PsyArXiv v3, Zenodo published.
- [ ] Set `Status: Shipped` on this checklist and on
  `resubmission_checklist.md`.
- [ ] Commit and push (submodule, then root pointer); no dirty submodule.
