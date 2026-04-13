#!/bin/bash
# Build preprint PDF for PsyArXiv upload
# Combines manuscript + TOP disclosure table into a single PDF
# with line numbers, APA-style formatting

set -euo pipefail
cd "$(dirname "$0")"

# Combine manuscript + TOP table into one source
cat manuscript.md > preprint_combined.md
echo -e "\n\n\\newpage\n" >> preprint_combined.md
cat top_disclosure_table.md >> preprint_combined.md

pandoc preprint_combined.md \
  -o preprint.pdf \
  --pdf-engine=xelatex \
  --citeproc \
  --bibliography=references.bib \
  --csl=/home/yetty/Zotero/styles/apa.csl \
  -V geometry:margin=1in \
  -V fontsize=12pt \
  -V linestretch=2 \
  -V mainfont="TeX Gyre Pagella" \
  -V header-includes='\usepackage{lineno}\linenumbers' \
  -V header-includes='\usepackage{longtable}' \
  -V header-includes='\usepackage{booktabs}' \
  --wrap=none

echo "Built: submissions/pci_psychology/preprint.pdf"

# Clean up
rm -f preprint_combined.md
