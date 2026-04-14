#!/bin/bash
# Build preprint PDF from LaTeX source
# Three xelatex passes with bibtex for bibliography resolution

set -euo pipefail
cd "$(dirname "$0")"

xelatex -interaction=nonstopmode manuscript.tex > /dev/null 2>&1
bibtex manuscript > /dev/null 2>&1
xelatex -interaction=nonstopmode manuscript.tex > /dev/null 2>&1
xelatex -interaction=nonstopmode manuscript.tex > /dev/null 2>&1

# Rename output for upload
cp manuscript.pdf preprint.pdf

echo "Built: submissions/pci_psychology/preprint.pdf"

# Clean aux files
rm -f manuscript.aux manuscript.log manuscript.out manuscript.toc manuscript.bbl manuscript.blg
