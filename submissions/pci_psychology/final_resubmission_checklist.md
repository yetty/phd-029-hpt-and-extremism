# PCI Psychology: final resubmission checklist

**Status:** Shipped (revision submitted 2026-10-21)
**Created:** 2026-10-20
**Basis:** Review of `resubmission_checklist.md` against the repository state
at commit `8d5e9c4`, the built PDFs, the test suite and the public Zenodo,
OSF and PsyArXiv records. This list covers only what is still open; the
earlier checklist stays the record of the A-K work already done.

Order: section 1 (easy fixes, Claude) -> section 2 (Juda) -> section 3 (final
build, Claude) -> section 4 (publication and submission, Juda with Claude).
Section 3 had to wait for section 2 because the Zotero fixes change the
reference list; section 2 is complete (2026-10-20), so section 3 can run.

Already verified, no action needed: no leftover strings (old OSF DOI,
"German instrument/dissertation", `hxngm_v2`, invented example item,
"285--293", "we/our"); no `(?)`, `??` or "n.d." in the PDF; both test
scripts pass; `manuscript.pdf` and `preprint.pdf` are current and identical;
the redline baseline `c952aa5` has the same manuscript text as the submitted
`60d4c5f`; OSF project storage is empty.

---

## 1. Easy fixes (Claude, no input needed)

- [x] **1.1 Table 1 width.** Table 1 (CFA fit indices, `manuscript.tex`
  around line 492) runs about 1.2 cm into the right margin. Shorten the
  bifactor row label (for example "Bifactor (post-registration)") or set
  the table in `\small`. Done: label shortened to "Bifactor
  (post-registration)"; the Note still says the model is orthogonal.
- [x] **1.2 Table 5 width.** Table 5 (correlations, around line 713) runs
  about 1.6 cm into the right margin. Narrow the first column or wrap the
  column headers so the table fits the text width. Done: numbered rows
  with column headers 1-7 (APA style); checked in the rendered PDF.
- [x] **1.3 `status.md`.** "Zenodo hosts the public reproducibility
  package" is not true until the record is published; say the DOI is
  reserved and the record is unpublished. Update Next Steps to point to
  this checklist.
- [x] **1.4 `project_status.md`.** Same correction: line 183 marks the move
  to Zenodo as done; change it to "DOI reserved, publication pending".
- [x] **1.5 Old plan.** Mark the citation step of Task 6 in
  `docs/superpowers/plans/2026-10-02-pci-final-corrections.md` as done
  (reference-verifier ran 2026-10-20; results are in
  `resubmission_checklist.md`).
- [x] **1.6 Old checklist.** Add one line to `resubmission_checklist.md`
  pointing to this file for the remaining items.
- [x] **1.7 Optional, R1-03.** Cite Wilschut and Schiphorst (2019) as the
  rationalist counterpoint to Endacott and Brooks (2013) in the
  construct-boundary paragraph, and add one sentence to the R1-03 reply.
  Only if the R/ note in 2.1 supports it. Not done: dropped with the R/
  requirement (2.1-2.2); the paper already cites Wilschut and Schiphorst
  (2019) elsewhere.
- [x] **1.8 Commit and push** each fix in the submodule, then the root
  pointer.

---

## 2. Juda: reading, Zotero and decisions

- [x] **2.1-2.2 R/ notes (former G1/G2): requirement removed.** Decided
  2026-10-20: R/ notes for the cited works are not a condition for this
  resubmission. Notes for `Loevinger1954Attenuation`,
  `Wilschut_Schiphorst2019One` and the other works without notes can be
  written later.
- [x] **2.3 Zotero: reference-list order.** Loevinger (1957) is listed before
  Loevinger (1954), and Hartmann and Hasselhorn (2008) before Hartmann
  (2008). The probable cause is a `sortname` value in the Extra field of
  `Loevinger1957Objective` and `Hartmann2008`; remove it from both.
  Checked 2026-10-20 after the re-export: Loevinger is fixed. Hartmann is
  still wrong because `sortname = {Hartmann}` is on
  `Hartmann_Hasselhorn2008`, not on `Hartmann2008`; remove it from the Extra
  field of `Hartmann_Hasselhorn2008` and export again.
  Done 2026-10-20 (root `8f145e72`, `sortname` dropped from all items):
  Hartmann 2008 now precedes Hartmann and Hasselhorn 2008; first-author
  order of the whole list checked.
- [x] **2.4 Zotero: Wilschut and Schiphorst title.** The title starts with a
  straight apostrophe, which renders as a closing quotation mark
  ("'One has to ..."). Use an opening single quotation mark at the start
  (or drop the quotation marks around the quoted phrase).
  Checked 2026-10-20: renders with an opening quotation mark.
- [x] **2.5 Zotero sync and Better BibLaTeX export** so 2.3-2.4 reach
  `bibliography.bib`. Tell Claude when it is done.
- [x] **2.6 Decide on the response-letter locations.** Decided 2026-10-20:
  every reply gets page and line numbers, filled in automatically (3.3).

---

## 3. Final build and verification (Claude, after section 2)

- [x] **3.1 Rerun the tests:** `test_revision_reporting_analyses.R` and
  `verify_statistics.R` from the project root.
  Done 2026-10-20: both pass.
- [x] **3.2 Rebuild** `manuscript.pdf`, `preprint.pdf` (`build_preprint.sh`)
  and `manuscript_diff.tex/.pdf` (`build_redline.sh`, baseline `c952aa5`).
  Check the Biber log for new warnings (two invalid-ISBN warnings are
  known and harmless).
  Done 2026-10-20: only the two known ISBN warnings; manuscript and
  preprint identical; 34 letter locations filled.
- [x] **3.3 Response letter locations** (per 2.6). Can be prepared in
  section 1, before the final build:
  - [x] Put invisible `\linelabel{...}` markers (from `lineno`) at each
    revised passage in `manuscript.tex`, one per reply.
  - [x] Put matching placeholders (for example `{{loc:rec01}}`) in
    `response_to_reviewers.md`.
  - [x] Write a small script that reads the page and line numbers from
    `manuscript.aux` after the build and fills them in as "p. 12,
    lines 340-352"; fail on any unresolved label.
  - [x] Run it after the final build and check a sample of locations
    against the PDF by eye.
  Done 2026-10-20: 34 `\Loc`/`\LocEnd` markers in `manuscript.tex`;
  placeholders live in `response_to_reviewers_template.md` (edit that file);
  `build_preprint.sh` runs `fill_response_locations.py`, which writes
  `response_to_reviewers.md`. PDF text unchanged by the markers; 16 marker
  lines checked against the PDF. Rerun automatically by the 3.2 build.
- [x] **3.4 Inspect the PDFs:** no `(?)`, `??` or "n.d."; Tables 1 and 5
  inside the margins; reference list in APA order (Loevinger 1954 before
  1957, Hartmann 2008 before Hartmann and Hasselhorn 2008); Wilschut title
  quotation marks correct; Figure 1 legible in greyscale.
  Done 2026-10-20: all checks pass in `manuscript.pdf`, the redline and
  the supplement.
- [x] **3.5 Rebuild the Zenodo release bundle.** The bundle in
  `.zenodo-release/` (10:48) predates the last README (11:14) and supplement
  (11:19) changes. Rebuild it from the final package and check the
  manifest, file count and checksums.
  Done 2026-10-20: rebuilt (deterministic); 55 files plus manifest, all
  SHA-256 checksums match; README and supplement current; no
  confidential files.
- [x] **3.6 Replace the files in the Zenodo draft** with the rebuilt bundle.
  The draft stays unpublished.
  Done 2026-10-20: old draft files deleted, new bundle uploaded,
  `zenodo_release.py verify` passes (zip MD5 `df640dc6...`); record
  23494026 is still an unpublished draft.
- [x] **3.7 Commit and push** the rebuilt files (submodule, then root
  pointer).

---

## 4. Publication and submission (Juda, with Claude where possible)

- [x] **4.1 Read the final clean PDF end to end** (former I6). Done
  2026-10-20.
- [ ] **4.2 Optional:** targeted human psychometric review (former I7).
- [x] **4.3 Approve Zenodo publication (Gate C)** and publish record
  23494026. Then check that
  <https://doi.org/10.5281/zenodo.23494026> resolves and every file
  downloads. This must happen before 4.5 because the manuscript, TOP form
  and cover letter describe the package as available.
  Done 2026-10-20 (approved by Juda): published at
  <https://zenodo.org/records/23494026>. Publishing first failed because
  the metadata had no `publisher`; added "Zenodo" (now required by
  `validate_metadata`), so the bundle was rebuilt and uploaded again before
  publishing. DOI resolves; both files download; the zip is byte-identical
  to the local bundle and all 55 checksums match. Also fixed the tool's
  `verify` for published records (Accept header).
- [x] **4.4 Post PsyArXiv version 3** of `hxngm` with the final
  `preprint.pdf`; check the title and abstract metadata; link the Zenodo
  DOI in the PsyArXiv record. Uploaded 2026-10-20
  (<https://osf.io/preprints/psyarxiv/hxngm_v3>), waiting for moderator
  approval; DOI `10.31234/osf.io/hxngm_v3` not yet registered (404), so
  4.5 and 4.6 wait for approval.
- [x] **4.5 Update the Zenodo related identifier** for the preprint to
  `hxngm_v3` (Zenodo allows metadata edits after publication).
  Done 2026-10-21 after moderator approval (v3 DOI resolves; Crossref
  posted 2026-10-20): metadata-only edit of the live record, still version
  1.0, files unchanged (`verify` passes). `osf_storage/zenodo_metadata.json`
  deliberately still says `hxngm_v2`: it is inside the published zip, and
  changing it would break `verify`. Update it with the next package version.
- [x] **4.6 Submit the revision on PCI Psychology:** link the PsyArXiv v3
  version; upload `response_to_reviewers.md` (as text or PDF),
  `manuscript_diff.pdf` and the cover letter.
- [x] **4.7 Check the PCI form fields** for data, code and materials links
  and set them to the Zenodo DOI.
  Done 2026-10-21: revision submitted by Juda.

---

## 5. Housekeeping (Claude, after submission)

- [x] Update `status.md` and `project_status.md`: stage "Revision
  submitted", resubmission date, PsyArXiv v3, Zenodo published.
- [x] Set `Status: Shipped` on this checklist and on
  `resubmission_checklist.md`.
- [x] Commit and push (submodule, then root pointer); no dirty submodule.
