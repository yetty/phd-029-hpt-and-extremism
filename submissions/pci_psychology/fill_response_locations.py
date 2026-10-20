#!/usr/bin/env python3
"""Fill page and line locations into the response letter.

Reads the \\Loc/\\LocEnd line labels that lineno writes to manuscript.aux and
replaces each {{loc:NAME}} placeholder in response_to_reviewers_template.md
with "p. 12, lines 340--352". Writes response_to_reviewers.md. Fails on any
placeholder without both labels.

Usage: fill_response_locations.py [manuscript.aux]
"""

import re
import sys
import textwrap
from pathlib import Path

HERE = Path(__file__).resolve().parent
TEMPLATE = HERE / "response_to_reviewers_template.md"
OUTPUT = HERE / "response_to_reviewers.md"
WIDTH = 79

LABEL = re.compile(r"\\newlabel\{loc:([^}]+)\}\{\{(\d+)\}\{(\d+)\}")
PLACEHOLDER = re.compile(r"\{\{loc:([^}]+)\}\}")
HEADER = re.compile(r"\A<!--.*?-->\n\n", re.S)


def read_labels(aux):
    labels = {}
    for m in LABEL.finditer(aux.read_text(encoding="utf-8")):
        labels[m.group(1)] = (int(m.group(2)), int(m.group(3)))
    return labels


def location(name, labels):
    start, end = labels.get(name), labels.get(name + ":end")
    if start is None or end is None:
        return None
    (line1, page1), (line2, page2) = start, end
    pages = f"p. {page1}" if page1 == page2 else f"pp. {page1}--{page2}"
    lines = f"line {line1}" if line1 == line2 else f"lines {line1}--{line2}"
    return f"{pages}, {lines}"


def rewrap(paragraph):
    """Rewrap a plain prose paragraph; leave headings, lists and tables."""
    if paragraph.startswith(("#", "|", "- ", "* ")):
        return paragraph
    return textwrap.fill(" ".join(paragraph.split()), WIDTH,
                         break_long_words=False, break_on_hyphens=False)


def fill(text, labels):
    missing = sorted({n for n in PLACEHOLDER.findall(text)
                      if location(n, labels) is None})
    if missing:
        sys.exit("unresolved locations: " + ", ".join(missing))
    paragraphs = []
    for paragraph in text.split("\n\n"):
        if PLACEHOLDER.search(paragraph):
            paragraph = rewrap(PLACEHOLDER.sub(
                lambda m: location(m.group(1), labels), paragraph))
        paragraphs.append(paragraph)
    return "\n\n".join(paragraphs)


def main():
    aux = Path(sys.argv[1]) if len(sys.argv) > 1 else HERE / "manuscript.aux"
    labels = read_labels(aux)
    template = HEADER.sub("", TEMPLATE.read_text(encoding="utf-8"))
    used = set(PLACEHOLDER.findall(template))
    unused = sorted({n.removesuffix(":end") for n in labels} - used)
    if unused:
        print("warning: manuscript markers not cited in the letter: "
              + ", ".join(unused), file=sys.stderr)
    OUTPUT.write_text(fill(template, labels), encoding="utf-8")
    print(f"Filled {len(used)} locations: {OUTPUT.name}")


if __name__ == "__main__":
    main()
