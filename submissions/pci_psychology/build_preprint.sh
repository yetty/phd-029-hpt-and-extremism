#!/bin/bash
# Build preprint PDF from LaTeX source
# Three XeLaTeX passes with BibTeXu for bibliography resolution. BibTeXu is
# required to preserve UTF-8 names from bibliography.bib with apacite.

set -euo pipefail
cd "$(dirname "$0")"

run_bibtexu() {
  local stem="$1"
  if bibtexu "$stem" > "${stem}.blg" 2>&1; then
    return
  fi

  # BibTeXu exits 1 for apacite's known unsupported-entry warnings. Accept
  # those warnings only when it produced a bibliography and no fatal message.
  if test -s "${stem}.bbl" && ! grep -Eqi \
    "Error--|Fatal|I couldn't open|I found no" "${stem}.blg"; then
    printf 'BibTeXu returned warnings; using the generated bibliography.\n' >&2
    return
  fi

  cat "${stem}.blg" >&2
  return 1
}

xelatex -interaction=nonstopmode manuscript.tex > /dev/null 2>&1
run_bibtexu manuscript
xelatex -interaction=nonstopmode manuscript.tex > /dev/null 2>&1
xelatex -interaction=nonstopmode manuscript.tex > /dev/null 2>&1

# Rename output for upload
cp manuscript.pdf preprint.pdf

echo "Built: submissions/pci_psychology/preprint.pdf"

# Clean aux files
rm -f manuscript.aux manuscript.log manuscript.out manuscript.toc manuscript.bbl manuscript.blg
