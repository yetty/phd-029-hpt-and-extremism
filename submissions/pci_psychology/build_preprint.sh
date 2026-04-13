#!/bin/bash
# Build preprint PDF for PsyArXiv upload
# Combines manuscript + TOP disclosure table into a single PDF
# with line numbers, APA-style formatting

set -euo pipefail
cd "$(dirname "$0")"

# Combine manuscript + TOP table into one source
# Insert explicit refs div so citeproc places bibliography before
# the TOP table, and use raw LaTeX for page break
cat manuscript.md > preprint_combined.md

# Replace the bare "## References" with a refs div
# so pandoc/citeproc knows where to place the bibliography
sed -i 's/^## References$/## References\n\n::: {#refs}\n:::\n/' preprint_combined.md

# Append page break and TOP table
cat >> preprint_combined.md <<'ENDOFAPPENDIX'

```{=latex}
\newpage
```

ENDOFAPPENDIX
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
