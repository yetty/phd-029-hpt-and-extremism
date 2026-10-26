# Peer Community Journal upload package

PCJ version of the article recommended by PCI Psychology (Hopwood, 2026,
<https://doi.org/10.24072/pci.psych.100475>), built from PsyArXiv v3
(<https://doi.org/10.31234/osf.io/hxngm_v3>, source
`../pci_psychology/manuscript.tex`). Content unchanged except the AI
disclosure (moved into Acknowledgements, as PCJ asks) and Tables S1--S7; see
`final_check.md`. Agent brief: `../../docs/pcj-submission-brief.md`.

## Files to upload

- `manuscript.pdf` - article in PCJ format (`mode=plain`, starts with the
  Introduction, no cover page, headers, footers or line/page numbers)
- `manuscript.tex` - the editable source. Self-contained: it needs only
  `references.bib` and the PCJ class files (Figure 1 is TikZ code inside
  the file). Build: pdflatex, biber, pdflatex, pdflatex.
- `references.bib` - the 52 cited references only, the file
  `manuscript.tex` reads
- `article_image.png` - article-page image, 2160 x 1080 px; Figure 1 made by
  our script `revision_reporting_analyses.R` (also in the Zenodo package)
- Form text: `form_fields.md`

## Build

`./build.sh` runs pdfLaTeX + Biber (biblatex-apa, as in v3) and fails on
LaTeX errors, undefined citations or overfull boxes. `check_content.py`
diffs the text against v3.

- `references.bib` is the only bibliography source. It was made once
  (2026-10-26) by Biber in tool mode from the root `bibliography.bib`
  (Zotero), `extras.bib` and the biblatex sourcemap fixes then in
  `manuscript.tex`, tidied by `clean_bib.py` (see its docstring). It keeps
  biblatex fields (`langid`, `date`, `urldate`, ...): without them
  biblatex-apa lowercases German titles and drops retrieval dates. Reference
  fixes now go into both Zotero and `references.bib`.
- `extras.bib`: entries not in Zotero (copied from `../pci_psychology/` plus
  Hopwood 2026, the Zenodo record and the OSF preregistration), kept for the
  Zotero import.
- Figure 1 was drawn by tikzDevice from `revision_reporting_analyses.R`
  re-run up to the figure (same data, same random seed: the PDF from that
  run renders 0 pixels different from the v3 figure at 150 dpi), with
  Helvetica metrics at 10 pt, as in the R figure. The only edits: the
  axis label "-2" set as a true minus; the R PDF device had printed every
  hyphen in the figure text as a minus.

## Template

- PCJ LaTeX package "Template_for_both_PCI_and_PCJ" by Centre Mersenne,
  linked from the OSF file "Templates for PCJ - links.docx"
  (<https://osf.io/kmwfv>, version 8, modified 2026-04-17), which points to
  Dropbox `Template_for_both_PCI_and_PCJ.zip`. Downloaded 2026-10-24,
  zip SHA-256
  `fcc128286c8d1923ad14206a8a676dd4f893f9c468ec79944e86b57fdec541a2`.
- Files: `cedram.cls`, `cedram-*.sty`, `cedram-*.clo`, `pcj.bbx`, `pcj.cbx`,
  `cdr@preprint@id.cfg`, `last_cdr@preprint@id.cfg` (unmodified) and
  `TEMPLATE_README.txt` (the package README). `PCJ-sample.tex` (last
  changed 2026-04-13) is not kept: `manuscript.tex` was started from it.
  `cedram-PCJ.sty` is dated 2025-09-15, the other class files 2024-03-01.
- The template's own `pcj` biblatex style is not used: PCJ asks for APA 7,
  so the v3 setup (biblatex-apa with Biber) is kept.
