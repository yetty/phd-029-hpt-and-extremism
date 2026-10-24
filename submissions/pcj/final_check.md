# PCJ package: final check

**Date:** 2026-10-24. Checked against the built files (`manuscript.pdf`, 24
pages; `manuscript.tex`; `references.bib`), per the double check in
`../../docs/pcj-submission-brief.md`. `[x]` passed, `[ ]` open (see the
end).

## Same content as the recommended version

- [x] Body text identical to `../pci_psychology/manuscript.tex`:
  `python check_content.py ../pci_psychology/manuscript.tex manuscript.tex`
  gives 8062 vs 8052 body words; the only differences are the five
  hand-typed "Table N." labels (now `\caption`, numbered by the class) and
  the order of `\begin{scriptsize}`. Back matter: Data availability +2
  words (the Zenodo and OSF citations) and "S1--S5" -> "S1--S7" (open
  item 2), Acknowledgements +18 words (the
  recommendation sentence); Conflict, Ethics, Funding, CRediT and AI use
  have 0 differences. The TOP checklist appendix is gone (`grep -c "TOP
  Checklist" manuscript.tex` = 0). Side effect of the new OSF reference:
  the one in-text citation of Kaleta2025Sticking now reads "Kaleta, 2025a"
  (APA disambiguation).
- [x] Numbers, tables and figures identical to v3: all table cells and
  in-text numbers are inside the word diff above (0 differences). Figure 1
  is the v3 file with its Helvetica font embedded by Ghostscript; renders
  at 150 dpi differ in 0 pixels by more than 32/255.

## PCJ format (instructions section 4)

- [x] PCJ class with `mode=plain` (`manuscript.tex:9`); `\title{}`,
  `\author{}`, `\address{}`, `\email{}` empty (lines 65-68).
- [x] Starts with the Introduction (p. 1, first line "1. Introduction"); no
  title page, abstract or keywords (`pdftotext` grep: none); no headers,
  footers or page numbers (0 of 24 pages have a bare number in the first
  or last line; pages 1 and 14 inspected as images); no line numbers
  (`lineno` not loaded).
- [x] Five mandatory sections with the exact names: Acknowledgements,
  Funding, Conflict of interest disclosure, Data, script, code, and
  supplementary information availability, References (all p. 20).
  Ethics Statement, Author Contributions (CRediT) and AI Use Disclosure
  kept with their v3 wording (p. 19).
- [x] Recommendation sentence in Acknowledgements (p. 20) with
  `https://doi.org/10.24072/pci.psych.100475` and "Hopwood, 2026"; Hopwood
  (2026) is in the reference list. **But** the DOI does not resolve yet:
  doi.org answered "DOI Not Found" on 2026-10-24 (see open items).
- [x] Tables and figures inside the margins: `build.sh` fails on any
  `Overfull` box and passed; Table 5 (p. 14) and Figure 1 (p. 15) checked
  as images. Figure is vector PDF; `find . -iname '*.svg' -o -iname
  '*.emf' -o -iname '*.wmf'` = 0.
- [x] References APA 7 (biblatex-apa), alphabetical, full
  `https://doi.org/...` links: 39 of 52 entries have a DOI and all print
  it. Citations and list match both ways: 52 cited keys (`.bcf`) = 52
  printed entries (`.bbl`) = 52 entries in `references.bib`; no undefined
  citations; 0 "n.d.". 13 entries print without a DOI: two may have one
  (ITC 2017, Seixas et al. 2015; open items), the rest are books, a
  working paper, a curriculum document and Teaching History articles
  with no DOI found on Crossref.

## PCJ presubmission checklist (instructions section 5)

- [x] PDF built from source with the PCJ class (`./build.sh`), not the
  PsyArXiv PDF.
- [x] Source `manuscript.tex` (with `figures/` and the class files).
- [x] `references.bib`: only the 52 cited entries, plain BibTeX field names
  (`bib2bibtex.py`), same keys as the printed list (see above).
- [x] `article_image.png`: PNG, 2160 x 1080 px, Figure 1 made by our
  `revision_reporting_analyses.R` (identical hash to the Zenodo copy
  `outputs/revision/revision_relationships.png`).
- [x] Permanent identifier: Zenodo 10.5281/zenodo.23494026, version 1.0,
  CC BY 4.0; record title, version and author (Kaleta, Juda, ORCID
  0000-0003-0043-4039) match the `Kaleta2026Package` entry (Zenodo API,
  2026-10-24). `MANIFEST-sha256.txt` lists `replication_package/README.md`.
- [x] Data that cannot be shared are explained in the availability section
  (p. 20: third-party instrument files and item text excluded pending
  redistribution permission).
- [x] Scripts reproduce the article numbers:
  `Rscript submissions/pci_psychology/test_revision_reporting_analyses.R`
  printed "revision reporting analysis helper tests passed" (exit 0);
  `verify_statistics.R` exit 0 and reproduced the DIF chi-squares of
  Table 3 (10.46, 9.01, 9.72, 9.59, 3.13, 8.25, 1.51, 3.60, 9.63), the TOST
  90% CI [-0.0958, 0.1271], the NS-only deltas (+.002/-.004, +.015/-.013)
  and the largest MI 11.22. The package also has
  `07_reproducibility_report`.
- [x] Zenodo record cited in the availability section (p. 20, "Kaleta,
  2026") and listed in the references; the OSF preregistration too
  ("Kaleta, 2025b").

## PCI and PCJ editorial policy (instructions section 2)

- [x] Sample size justification in Methods: Section 3.2 (p. 6), a priori
  power analysis, 80% power for beta >= .15, n = 282 vs minimum 250.
- [x] Preregistration cited and deviations disclosed: Section 2.5 (p. 5)
  and the availability section point to Table S5.
- [x] Ethics: Declaration of Helsinki and the consent procedure (principal
  permission, voluntary student participation, no parental consent,
  de-identified data) in the Ethics Statement (p. 19).
- [x] Conflict of interest: no financial conflicts, no non-financial ones
  (p. 20).
- [x] Funding stated (p. 20).
- [x] AI use disclosed (AI Use Disclosure, p. 19); the only figure is made
  by an R script, not by AI.
- [x] No previously published figures or tables reproduced; instrument
  items not included; all sources cited.
- [x] Single author; affiliation for the form in `form_fields.md`: Charles
  University, Faculty of Arts, Institute of History (Ústav historie).

## Project rules

- [x] `grep -rIil 'registered report\|stage 1\|stage 2\|gymn' .` in
  `submissions/pcj/` and in the PDF text: no hits. No file with
  CONFID(E)NTIAL or teacher report in `submissions/pcj/`.
- [ ] `reference-verifier` run (2026-10-24): all 53 keys resolve and no
  drift; the formatting fixes it found are applied via the sourcemap in
  `manuscript.tex`. **Unresolved** (none can be fixed without changing
  content or reading the sources):
  - reading evidence: 14 cited works have no R/ note, 22 only an archived
    stub, 3 a note without annotations (e.g. Wilschut & Schiphorst 2019,
    Loevinger 1954, Huijgen et al. 2014/2017, the extras-only methods
    references). These are the same citations as in the recommended v3.
  - content flags: Suedfeld & Tetlock (1977) for authoritarianism and
    integrative complexity (no note; title does not suggest it); AERA et
    al. (2014) for "invariance testing becomes essential" (no Ch. 3 note);
    Chapman (2021) only partly supports "knowledge-dependent competence".
  - year decisions (would change in-text citations): Smith et al. is
    Cognition and Instruction 37(1), the 2019 issue (cited as 2018);
    VanSledright prints 2013 while the key says 2014.

## Open items for Juda

1. Recommendation DOI 10.24072/pci.psych.100475 does not resolve at
   doi.org yet. Probably also the cause of the "error 500" on the PCJ form
   (`../pcj_error500_report_draft.txt`), which takes this DOI. Check it
   resolves before trying again or sending the report.
2. Done 2026-10-24 (Juda's decision): availability section now says
   "Tables S1--S7" (v3 said S1--S5, but the body cites S6 and S7 and the
   supplement has S1-S7). The one deliberate content change from v3.
3. Zotero: add Hopwood (2026), the Zenodo record and the OSF
   preregistration (now in `extras.bib`), and fix: Ripka et al. (2024)
   editor "Åström Elmersjö, Henrik"; Barton & Levstik (2004) DOI
   10.4324/9781410610508; Messick (1995) stray final period; Lee & Shemilt
   (2003) no issue number; Seixas & Morton (2013) and Wineburg (2001)
   subtitles; Wilschut & Schiphorst (2019) opening quote; Badham et al.
   (2025) article number 1616879. Then remove the sourcemap.
4. Decide: ITC (2017) could cite the International Journal of Testing
   version (https://doi.org/10.1080/15305058.2017.1398166) instead of the
   web PDF; Seixas et al. (2015) chapter DOI (book
   10.4324/9781315779539); Kaleta (2025a) has no pages/article number;
   Smith et al. year; VanSledright year.
5. Reference-verifier reading gaps and content flags above (pre-existing,
   outside the format scope).
