"""Turn Biber's biblatex-style export into plain BibTeX for the PCJ upload.

Biber's --output-format=bibtex keeps biblatex names (journaltitle, location,
@dataset, ...). PCJ asks for a BibTeX export, so rename fields and entry
types to their BibTeX equivalents and drop fields that only matter locally.

Usage: python bib2bibtex.py references.bib
"""
import re
import sys

TYPES = {"inbook": "incollection", "dataset": "misc", "online": "misc",
         "report": "techreport"}
FIELDS = {"journaltitle": "journal", "location": "address",
          "eid": "pages"}
DROP = {"abstract", "keywords", "ids", "langid", "shortjournal",
        "shorttitle", "urldate", "eprint", "eprinttype", "issn"}

path = sys.argv[1]
text = open(path, encoding="utf-8").read()
out = []
for entry in re.split(r"\n(?=@)", text.strip()):
    head, *lines = entry.split("\n")
    m = re.match(r"@(\w+)\{(.*),$", head)
    etype, key = m.group(1).lower(), m.group(2)
    fields = {}
    for line in lines:
        f = re.match(r"\s+([A-Z]+) = (.*),$", line)
        if f:
            fields[f.group(1).lower()] = f.group(2)
    if etype == "thesis" and fields.get("type") == "{phdthesis}":
        etype = "phdthesis"
        fields["school"] = fields.pop("institution")
        del fields["type"]
    if etype == "report":
        fields["number"] = fields.pop("eid", fields.get("number"))
    if "date" in fields:
        year = re.match(r"\{(\d{4})", fields.pop("date")).group(1)
        fields.setdefault("year", "{%s}" % year)
    if "titleaddon" in fields:
        fields["note"] = fields.pop("titleaddon")
    if "version" in fields and etype == "dataset":
        fields["note"] = "{Version %s}" % fields.pop("version").strip("{}")
    if etype in ("dataset", "online", "misc"):
        source = fields.get("publisher") or fields.get("organization")
        if source:
            fields.setdefault("howpublished", source)
    etype = TYPES.get(etype, etype)
    body = []
    for name, value in fields.items():
        if name in DROP:
            continue
        body.append("  %s = %s," % (FIELDS.get(name, name), value))
    out.append("@%s{%s,\n%s\n}" % (etype, key, "\n".join(body)))

open(path, "w", encoding="utf-8").write("\n\n".join(out) + "\n")
print("%d entries" % len(out))
