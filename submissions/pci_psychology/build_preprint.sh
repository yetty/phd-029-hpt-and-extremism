#!/bin/bash
# Build preprint PDF from LaTeX source
# Three XeLaTeX passes with Biber for bibliography resolution. Biber reads
# the Better BibLaTeX export in bibliography.bib (biblatex-apa style).

set -euo pipefail
cd "$(dirname "$0")"

xelatex -interaction=nonstopmode manuscript.tex > /dev/null 2>&1
biber --quiet manuscript
xelatex -interaction=nonstopmode manuscript.tex > /dev/null 2>&1
xelatex -interaction=nonstopmode manuscript.tex > /dev/null 2>&1

# Rename output for upload
cp manuscript.pdf preprint.pdf

echo "Built: submissions/pci_psychology/preprint.pdf"

# Fill page and line locations into the response letter from the line labels.
"${PYTHON:-../../../../.venv/bin/python}" fill_response_locations.py manuscript.aux

# PDF versions of the letters for the PCI upload.
for letter in response_to_reviewers cover_letter; do
  pandoc -f markdown+autolink_bare_uris "$letter.md" -o "$letter.pdf" \
    --pdf-engine=xelatex -V geometry:margin=2.5cm \
    -V mainfont="TeX Gyre Pagella" -V fontsize=11pt -V colorlinks=true \
    -V header-includes='\usepackage{xurl}'
done
echo "Built: response_to_reviewers.pdf, cover_letter.pdf"

# Clean aux files
rm -f manuscript.aux manuscript.log manuscript.out manuscript.toc manuscript.bbl manuscript.blg manuscript.bcf manuscript.run.xml
