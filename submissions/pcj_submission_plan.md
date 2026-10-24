# Peer Community Journal: submission plan

**Status:** Draft, not started
**Created:** 2026-10-24
**Basis:** PCI Psychology recommendation of PsyArXiv v3 and the PCJ author
instructions (`../../../sources/Publications/2026-10-24-pcj-author-instructions.txt`,
<https://peercommunityjournal.org/page/instructions/>, read 2026-10-24).

## The recommendation

- Hopwood, C. (2026). Recommendation of a Czech translation of an
  historical perspective taking instrument. *Peer Community in
  Psychology*, 100475. <https://doi.org/10.24072/pci.psych.100475>
- Page: <https://psych.peercommunityin.org/articles/rec?id=475>
- Recommended version: PsyArXiv v3,
  <https://doi.org/10.31234/osf.io/hxngm_v3>
- Posted 2026-10-22, validated 2026-10-23. Reviewers: Ryan Strickler and
  one anonymous reviewer. One revision round; author reply DOI
  <https://doi.org/10.24072/pci.psych.100475.ar1>.
- The recommendation text names the single historical scenario as the
  main limitation; no further changes were requested.

## How PCJ works after a recommendation

- PCJ publishes PCI-recommended preprints with no further review and no
  fees (Diamond OA, CC BY 4.0, author keeps copyright). Nothing has to be
  re-reviewed; PCJ only checks the format, metadata and references and
  sends proofs.
- No deadline after the recommendation is stated.
- Submission: <https://peercommunityjournal.org/submit>, sign-in with
  ORCID (0000-0003-0043-4039), a short form plus file uploads.
- Authors format the article themselves. If that is not possible, PCJ
  formats it on request ("If you cannot format your article yourself,
  just ask us").
- PCJ uses AI checks for format and references; a person reviews the
  proof.

## What to upload (PCJ presubmission checklist)

1. PDF in PCJ format, built from source (not the PsyArXiv PDF).
2. Source file: `.tex` (or doc/docx/odt).
3. BibTeX export of only the cited references.
4. One PNG or JPEG image for the article page, with rights owned (for
   example Figure 1, made by our script).
5. A permanent identifier for data and code: Zenodo
   <https://doi.org/10.5281/zenodo.23494026> (already meets the
   versioned-repository, README and reproducibility rules).

## Gaps between the current manuscript and PCJ format

- [ ] **Template.** Move `pci_psychology/manuscript.tex` (pandoc article
  class, biblatex-apa) into the PCJ LaTeX class: OSF
  <https://osf.io/kmwfv> or the Overleaf template "PCI and Peer Community
  Journal". Use the class option `mode=plain` and leave the metadata
  fields empty.
- [ ] **No front matter.** Text starts with the Introduction: no title
  page, abstract, keywords, headers, footers, line or page numbers. PCJ
  adds the cover page from the form metadata (title and abstract from
  `pci_title_abstract.txt`).
- [ ] **Recommendation sentence** in Acknowledgements: "Preprint version 3
  of this article has been peer-reviewed and recommended by Peer
  Community In Psychology (https://doi.org/10.24072/pci.psych.100475;
  Hopwood, 2026)." Add the Hopwood (2026) recommendation to Zotero so it
  appears in the reference list.
- [ ] **Mandatory section names:** "Acknowledgements", "Funding",
  "Conflict of interest disclosure", "Data, script, code, and
  supplementary information availability", "References". Rename the
  current "Data Availability Statement" accordingly; keep Ethics, CRediT
  and AI use (PCJ requires AI disclosure in methods or acknowledgements).
- [ ] **Zenodo in the references.** PCJ wants the data and code record in
  the reference list and cited in the availability section. Today it is
  only a bare URL. Add the Zenodo record (and the OSF preregistration) to
  Zotero and cite them.
- [ ] **Remove the TOP checklist appendix.** It is a PCI Psychology
  submission requirement, not part of the article.
- [ ] **Supplement.** Stays in the Zenodo package; cite it from the
  availability section (already done).
- [ ] **References:** APA 7, alphabetical, DOIs as full
  `https://doi.org/...` URLs, every citation in the list and back.
  Re-run `reference-verifier` after the move to the new template.
- [ ] **Figures and tables** within the margins; figures as PNG/JPG (no
  SVG/EMF/WMF).
- [ ] **Language.** PCJ has no spelling rule; keep American English and
  APA 7 as in the recommended version.

## Order of work

Agent instructions with a final double check:
`../docs/pcj-submission-brief.md`.

1. Build the PCJ version in `submissions/pcj/` (copy of the recommended
   v3 text; only the format changes above, no content changes).
2. Zotero: add Hopwood (2026), Zenodo record, OSF preregistration.
3. Build the PDF, check it against the checklist above, run
   `reference-verifier`, export the BibTeX of cited references.
4. Optional: post the formatted text as PsyArXiv v4 (not required by PCJ;
   the recommended version stays v3).
5. Submit at <https://peercommunityjournal.org/submit> (Juda, ORCID
   login), then check the proofs.
6. After publication: add the PCJ DOI to Zenodo (related identifier),
   PsyArXiv, ORCID, `areas/Publications.md` and the doctoral-study
   record.
