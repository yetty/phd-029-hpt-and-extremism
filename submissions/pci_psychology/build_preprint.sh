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

# Clean aux files
rm -f manuscript.aux manuscript.log manuscript.out manuscript.toc manuscript.bbl manuscript.blg manuscript.bcf manuscript.run.xml
