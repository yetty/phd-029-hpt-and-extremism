# Brief: prepare the Peer Community Journal submission

Instructions for an agent working on its own. Start it from the project
folder with "Follow docs/pcj-submission-brief.md".

## Context

- The manuscript was recommended by PCI Psychology on 2026-10-22:
  Hopwood, C. (2026). Recommendation of a Czech translation of an
  historical perspective taking instrument. *Peer Community in
  Psychology*, 100475. <https://doi.org/10.24072/pci.psych.100475>
  (page: <https://psych.peercommunityin.org/articles/rec?id=475>).
- Recommended version: PsyArXiv v3,
  <https://doi.org/10.31234/osf.io/hxngm_v3>. Its source is
  `submissions/pci_psychology/manuscript.tex` (with `bibliography.bib`,
  `extras.bib`, `revision_outputs/`), built by `build_preprint.sh`.
- Peer Community Journal (PCJ) publishes PCI-recommended preprints with no
  further review. The author formats the article and uploads it.
- Read first:
  - `README.md` (project hub) and `CLAUDE.md` (project rules)
  - `submissions/pcj_submission_plan.md` (gaps and order of work)
  - `../../sources/Publications/2026-10-24-pcj-author-instructions.txt`
    (the PCJ rules, saved 2026-10-24; section 4 is the formatting part,
    section 5 the presubmission checklist). If anything looks outdated,
    compare it with <https://peercommunityjournal.org/page/instructions/>.
  - The root `CLAUDE.md` hard rules (commits, `bibliography.bib`,
    `reference-verifier`, 80-column Markdown).

## Goal

A complete PCJ upload package in `submissions/pcj/`, so the only thing
left for Juda is to log in with ORCID, fill the form and upload. Do
everything else that the submission needs. Do not submit anything
yourself and do not log in anywhere on Juda's behalf.

## Hard limits

- **Do not change the content.** The text, numbers, tables and figures
  stay as in the recommended v3. Only the format and the parts PCJ
  requires (recommendation sentence, section names, data and code
  reference) change. If a PCJ rule seems to need a content change, stop
  and list it as an open question instead.
- Do not edit `submissions/pci_psychology/`; it is the record of what PCI
  recommended. Copy what you need.
- Never edit the root `bibliography.bib`. New references belong in
  Zotero. If you cannot reach Zotero, put the missing entries in
  `submissions/pcj/extras.bib` (as `pci_psychology/extras.bib` already
  does) and list them for Juda to add to Zotero.
- Project rules still apply: never write "Stage 1/2" or "Registered
  Report"; do not name the upper-secondary school types; never copy files
  with `CONFIDENTAL`/`CONFIDENTIAL` in the name or teacher reports.

## Steps

1. **Template.** Get the PCJ LaTeX package (OSF <https://osf.io/kmwfv>,
   or the Overleaf template "PCI and Peer Community Journal"). Put the
   class files in `submissions/pcj/` and record the template's source and
   version in `submissions/pcj/README.md`.
2. **Move the text.** Create `submissions/pcj/manuscript.tex` from the
   PCJ sample file with the class option `mode=plain` and the metadata
   fields (names, affiliations, emails) left empty. Paste the body of the
   v3 manuscript, starting with the Introduction. Leave out the title
   page, abstract, keywords, running heads, line numbers and page
   numbers; PCJ adds these.
3. **Back matter.** PCJ requires these sections, with these names:
   "Acknowledgements", "Funding", "Conflict of interest disclosure",
   "Data, script, code, and supplementary information availability",
   "References". Rename the v3 "Data Availability Statement" to that last
   name. Keep the Ethics statement, CRediT and AI use sections with
   their current wording.
4. **Recommendation sentence.** Add to Acknowledgements: "Preprint
   version 3 of this article has been peer-reviewed and recommended by
   Peer Community In Psychology
   (https://doi.org/10.24072/pci.psych.100475; Hopwood, 2026)." Hopwood
   (2026) must appear in the reference list.
5. **Data and code reference.** Cite the Zenodo package
   (<https://doi.org/10.5281/zenodo.23494026>) as a reference in the
   availability section and list it under References. Do the same for
   the OSF preregistration (<https://osf.io/zsngy/>). Check on Zenodo
   that the record title, version and authors in the entry match the
   record.
6. **Remove the TOP checklist appendix.** It was a PCI submission form,
   not part of the article.
7. **References.** APA 7, alphabetical, citations as (Smith & Doe, 2010)
   or (Smith et al., 2010), and a full `https://doi.org/...` link at the
   end of every reference that has a DOI. Every in-text citation must be
   in the list and every listed item must be cited. Keep the bibliography
   tooling that works for the v3 build (biblatex-apa with Biber; see the
   BibTeXu note on the Ripka editor name in the project history) unless
   the PCJ class forces a change.
8. **Figures and tables.** Everything inside the margins (landscape pages
   if needed). Figures as PDF, PNG or JPG; no SVG, EMF or WMF.
9. **Build.** Write `submissions/pcj/build.sh` modelled on
   `pci_psychology/build_preprint.sh` and build `manuscript.pdf`. Fix
   every LaTeX error and overfull box that shows in the PDF.
10. **BibTeX export.** Write `submissions/pcj/references.bib` with only the
    references cited in the article (for example from Biber's
    `--output-format=bibtex` tool mode or a script that filters the full
    file by the cited keys). Check it has the same entries as the
    printed reference list.
11. **Article-page image.** Export one figure made by our own scripts
    (preferably the descriptive relationships figure) as
    `submissions/pcj/article_image.png`, at least 1200 px wide.
12. **Form text.** Write `submissions/pcj/form_fields.md` with what Juda
    will paste into the PCJ form: title, abstract and keywords (from
    `pci_psychology/pci_title_abstract.txt`), author, affiliation (exact
    wording from the root `CLAUDE.md`), ORCID, email, recommendation DOI,
    preprint DOI and version, Zenodo DOI, and the list of files to
    upload.
13. **Run `reference-verifier`** on the PCJ manuscript and fix what it
    finds within the limits above.
14. **Double check** (next section). Repeat steps 9-13 until it passes.
15. **Record it.** Update `submissions/pcj_submission_plan.md` (tick what
    is done, status "Ready to submit") and the hub `README.md` (Next
    steps and a Log line). Commit and push only the paths you changed,
    following the root `CLAUDE.md` (project submodule first, then the
    pointer in the root repo).

## Double check before you finish

Check each item against the built files, not from memory. Write the
result to `submissions/pcj/final_check.md` as a checklist with one line of
evidence per item (a page number, a grep result, a command output).

**Same content as the recommended version**
- [ ] Compare the body text of `pcj/manuscript.tex` with
  `pci_psychology/manuscript.tex` (for example `latexdiff` or a word diff
  after stripping markup). The only differences are the format changes,
  the recommendation sentence, the renamed section, the new references
  and the removed TOP appendix.
- [ ] All numbers, tables and figures are identical to v3.

**PCJ format (instructions section 4)**
- [ ] Built with the PCJ class and `mode=plain`; metadata fields empty.
- [ ] Text starts with the Introduction; no title page, abstract,
  headers, footers, line numbers or page numbers.
- [ ] All five mandatory sections are present with the exact names.
- [ ] Recommendation sentence in Acknowledgements, with the correct DOI
  and Hopwood (2026) in the references.
- [ ] Tables and figures inside the margins; no SVG, EMF or WMF.
- [ ] References in APA 7, alphabetical, with full DOI links; citations
  and list match in both directions.

**PCJ presubmission checklist (instructions section 5)**
- [ ] Correctly formatted PDF, built from source (not the PsyArXiv PDF).
- [ ] Source `.tex` file.
- [ ] BibTeX file with only the cited references.
- [ ] PNG or JPEG article image that we own.
- [ ] A permanent identifier for data, scripts and code (Zenodo DOI),
  with a README in the record that explains how to use them.
- [ ] Data that cannot be shared (third-party instrument files and item
  text) are explained in the availability section.
- [ ] The scripts in the Zenodo package reproduce the numbers in the
  article (the project's regression suite and `verify_statistics.R` pass,
  or the existing reproducibility report says so).
- [ ] The Zenodo record is cited in the availability section and listed
  in the references.

**PCI and PCJ editorial policy (instructions section 2)**
- [ ] Sample size justification is in the Methods.
- [ ] Preregistration cited, deviations disclosed (Table S5).
- [ ] Ethics: Declaration of Helsinki and consent procedure stated.
- [ ] Conflict of interest: no financial conflicts; non-financial ones
  declared.
- [ ] Funding stated.
- [ ] AI use disclosed in the methods or acknowledgements; no
  AI-generated figures.
- [ ] Permission or credit for any previously published material
  (instrument items excluded; cited sources credited).
- [ ] Single author; affiliation as in the root `CLAUDE.md` (Ústav
  historie, never "Ústav českých dějin").

**Project rules**
- [ ] No "Stage 1/2" or "Registered Report"; no named upper-secondary
  school types; no confidential files in `submissions/pcj/`.
- [ ] `reference-verifier` run with no unresolved findings.

## Final report

End with a short summary for Juda: what is in `submissions/pcj/`, what
`final_check.md` found, any item that did not pass or needed a decision,
any references to add to Zotero, and the remaining manual steps (ORCID
login at <https://peercommunityjournal.org/submit>, fill the form from
`form_fields.md`, upload the files, approve the proofs).
