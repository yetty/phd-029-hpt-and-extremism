#!/bin/bash
# Build the Peer Community Journal PDF from LaTeX source.
# pdfLaTeX (the PCJ cedram class loads inputenc/fontenc itself) with Biber
# for biblatex-apa, as in ../pci_psychology/build_preprint.sh. Then export
# the cited references as BibTeX for the PCJ upload.

set -euo pipefail
cd "$(dirname "$0")"

pdflatex -interaction=nonstopmode manuscript.tex > /dev/null 2>&1 || true
biber --quiet manuscript
pdflatex -interaction=nonstopmode manuscript.tex > /dev/null 2>&1 || true
pdflatex -interaction=nonstopmode manuscript.tex > /dev/null 2>&1 || true

# Fail on LaTeX errors, undefined citations or references, overfull boxes.
if grep -E '^!|Citation .* undefined|Reference .* undefined|Overfull' \
  manuscript.log; then
  echo "Build problems: see manuscript.log" >&2
  exit 1
fi
grep -E 'WARN|ERROR' manuscript.blg >&2 || true
echo "Built: submissions/pcj/manuscript.pdf"

# BibTeX export of only the cited entries (Biber tool mode on the .bcf).
biber --quiet --output-format=bibtex --output-resolve --output-legacy-dates \
  --output-file=references.bib manuscript.bcf
"${PYTHON:-../../../../.venv/bin/python}" bib2bibtex.py references.bib
echo "Exported: submissions/pcj/references.bib"

# Clean aux files (keep the .log for checks).
rm -f manuscript.aux manuscript.out manuscript.toc manuscript.bbl \
  manuscript.blg manuscript.bcf manuscript.run.xml manuscript.cdrsom \
  manuscript.xmpdata
