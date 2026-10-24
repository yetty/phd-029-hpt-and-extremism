# Peer Community Journal upload package

PCJ version of the article recommended by PCI Psychology (Hopwood, 2026,
<https://doi.org/10.24072/pci.psych.100475>), built from PsyArXiv v3
(<https://doi.org/10.31234/osf.io/hxngm_v3>, source
`../pci_psychology/manuscript.tex`). Content unchanged; see
`final_check.md`. Agent brief: `../../docs/pcj-submission-brief.md`.

## Files to upload

- `manuscript.pdf` - article in PCJ format (`mode=plain`, starts with the
  Introduction, no cover page, headers, footers or line/page numbers)
- `manuscript.tex` - LaTeX source (needs `figures/revision_relationships.pdf`
  and the class files below)
- `references.bib` - BibTeX export of the 52 cited references only
- `article_image.png` - article-page image, 2160 x 1080 px; Figure 1 made by
  our script `revision_reporting_analyses.R` (also in the Zenodo package)
- Form text: `form_fields.md`

## Build

`./build.sh` runs pdfLaTeX + Biber (biblatex-apa, as in v3), fails on LaTeX
errors, undefined citations or overfull boxes, then writes `references.bib`
(Biber tool mode, converted to plain BibTeX field names by
`bib2bibtex.py`). `check_content.py` diffs the text against v3.

- `bibliography.bib` is a relative symlink to the root `bibliography.bib`
  (Zotero export; never edit it).
- `extras.bib`: entries not in Zotero (copied from `../pci_psychology/` plus
  Hopwood 2026, the Zenodo record and the OSF preregistration).
- `figures/revision_relationships.pdf` is the v3 figure
  (`../pci_psychology/revision_outputs/`) passed through Ghostscript
  (`pdfwrite -dEmbedAllFonts=true -dPDFSETTINGS=/prepress`) so that
  Helvetica is embedded; renders identical at 150 dpi.
- `manuscript.tex` holds a biblatex sourcemap with two fixes that belong in
  Zotero (editor of Ripka et al. 2024; DOI of Barton & Levstik 2004).

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
