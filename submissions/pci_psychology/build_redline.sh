#!/bin/bash
# Build a c952aa5-baseline redline with UTF-8-safe BibTeXu bibliography output.
# --graphics-markup=none avoids latexdiff table markup that breaks longtable.

set -euo pipefail

baseline="${1:-c952aa5f7a0aee2c6799615bc66fd8f356b28a4a}"
project_root="$(cd "$(dirname "$0")/../.." && pwd)"
submission_dir="$project_root/submissions/pci_psychology"
baseline_tex="/tmp/opencode/phd-029-manuscript-${baseline:0:7}.tex"

git -C "$project_root" show \
  "${baseline}:submissions/pci_psychology/manuscript.tex" > "$baseline_tex"

latexdiff --graphics-markup=none "$baseline_tex" \
  "$submission_dir/manuscript.tex" > "$submission_dir/manuscript_diff.tex"
sed -i "1i% Redline baseline: $baseline" "$submission_dir/manuscript_diff.tex"
# Keep the generated source clean for git diff --check before compiling it.
sed -i 's/[[:space:]]\+$//' "$submission_dir/manuscript_diff.tex"

cd "$submission_dir"
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

xelatex -interaction=nonstopmode manuscript_diff.tex > /dev/null 2>&1
run_bibtexu manuscript_diff
xelatex -interaction=nonstopmode manuscript_diff.tex > /dev/null 2>&1
xelatex -interaction=nonstopmode manuscript_diff.tex > /dev/null 2>&1

rm -f manuscript_diff.aux manuscript_diff.log manuscript_diff.out \
  manuscript_diff.bbl manuscript_diff.blg

echo "Built: submissions/pci_psychology/manuscript_diff.pdf"
