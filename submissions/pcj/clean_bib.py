"""Tidy Biber's export of the cited entries into references.bib.

references.bib was made once (2026-10-26) from the Zotero export
(../../../../bibliography.bib), extras.bib and the sourcemap fixes then in
manuscript.tex:

    biber --output-format=bibtex --output-resolve \
      --output-file=raw.bib manuscript.bcf
    python clean_bib.py raw.bib references.bib

It keeps the biblatex fields (langid, date, urldate, titleaddon, eid, ...),
which the PCJ template's biblatex setup reads and which biblatex-apa needs to
print the same reference list. It only drops local fields, lowercases the
names, and writes typographic punctuation as TeX ligatures.

Usage: python clean_bib.py raw.bib references.bib
"""
import re
import sys

DROP = {"abstract", "keywords", "ids", "shortjournal"}
TYPO = {"–": "--", "—": "---", "‘": "`", "’": "'",
        "“": "``", "”": "''"}

src, dst = sys.argv[1], sys.argv[2]
text = open(src, encoding="utf-8").read()
out = []
for entry in re.split(r"\n(?=@)", text.strip()):
    head, rest = entry.split("\n", 1)
    m = re.match(r"@(\w+)\{(.*),$", head)
    etype, key = m.group(1).lower(), m.group(2)
    fields = re.findall(r"^\s+([A-Z]+) = (.*?),\n(?=\s+[A-Z]+ = |\}$)",
                        rest + "\n", flags=re.M | re.S)
    body = ["  %s = %s," % (name.lower(), value)
            for name, value in fields if name.lower() not in DROP]
    out.append("@%s{%s,\n%s\n}" % (etype, key, "\n".join(body)))

result = "\n\n".join(out) + "\n"
for char, tex in TYPO.items():
    result = result.replace(char, tex)
open(dst, "w", encoding="utf-8").write(result)
print("%d entries" % len(out))
