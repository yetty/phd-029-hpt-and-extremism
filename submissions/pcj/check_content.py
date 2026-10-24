"""Word-level diff of the PCJ manuscript against the recommended v3 source.

Strips LaTeX markup from both files (commands, braces, comments, line-number
markers) and prints every differing run of words. The body (Introduction to
Conclusion) is compared as one block; the back-matter sections are compared
one by one, because PCJ wants them renamed and in a different order.

Usage: python check_content.py ../pci_psychology/manuscript.tex manuscript.tex
"""
import difflib
import re
import sys

# v3 heading -> PCJ heading
BACK = {
    "Data Availability Statement":
        "Data, script, code, and supplementary information availability",
    "Conflict of Interest Disclosure": "Conflict of interest disclosure",
    "Ethics Statement": "Ethics Statement",
    "Funding": "Funding",
    "Author Contributions (CRediT)": "Author Contributions (CRediT)",
    "Acknowledgements": "Acknowledgements",
    "AI Use Disclosure": "AI Use Disclosure",
}
HEAD = re.compile(r"\\(?:sub)?section\*?\{([^}]*(?:\([^)]*\))?)\}")


def strip(t):
    t = re.sub(r"(?<!\\)%.*", "", t)
    t = re.sub(r"\\Loc(?:End)?\{[^}]*\}", "", t)
    t = re.sub(r"\\url\{([^}]*)\}", r"\1", t)
    t = re.sub(r"\\(?:label|hypertarget|includegraphics)(?:\[[^]]*\])?\{[^}]*\}", "", t)
    t = re.sub(r"\\begin\{longtable\}\[\]\{[^\n]*\}", "", t)
    t = re.sub(r"\\[a-zA-Z@]+\*?", " ", t)
    t = re.sub(r"[{}\[\]$&\\~]", " ", t)
    t = re.sub(r"\(\s+", "(", t)
    t = re.sub(r"\s+([).;,])", r"\1", t)
    return t.split()


def split(path):
    """Return (body words, {heading: words}) for one manuscript."""
    t = open(path, encoding="utf-8").read()
    t = t[t.index("\\begin{document}"):]
    t = t.split("\\printbibliography")[0]
    heads = [(m.start(), m.group(1)) for m in HEAD.finditer(t)]
    parts, body_end = {}, None
    for n, (pos, name) in enumerate(heads):
        end = heads[n + 1][0] if n + 1 < len(heads) else len(t)
        if name in BACK or name in BACK.values():
            body_end = pos if body_end is None else body_end
            parts[name] = strip(t[pos:end])[len(name.split()):]
    start = t.index("1. Introduction")
    return strip(t[start:body_end]), parts


def diff(label, old, new):
    sm = difflib.SequenceMatcher(None, old, new, autojunk=False)
    ops = [o for o in sm.get_opcodes() if o[0] != "equal"]
    print("\n== %s: %d vs %d words, %d difference(s)"
          % (label, len(old), len(new), len(ops)))
    for op, i1, i2, j1, j2 in ops:
        print("  [%s] - %s" % (op, " ".join(old[i1:i2])))
        print("  %s  + %s" % (" " * len(op), " ".join(new[j1:j2])))


old_body, old_back = split(sys.argv[1])
new_body, new_back = split(sys.argv[2])
diff("Body (1. Introduction to 5.8 Conclusion)", old_body, new_body)
for v3, pcj in BACK.items():
    diff("%s -> %s" % (v3, pcj), old_back.get(v3, []), new_back.get(pcj, []))
extra = set(new_back) - set(BACK.values())
print("\nPCJ sections not in v3:", sorted(extra) or "none")
