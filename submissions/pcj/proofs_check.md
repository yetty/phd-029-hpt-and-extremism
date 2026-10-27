# PCJ proofs check (#831, received 2026-10-27)

Compared the PCJ proofs (`10_24072_pcjournal_831_proofs.pdf`, 21 pp.)
and the Word source (`831V2.docx`) with the submitted
`manuscript.pdf`. Method: word-level diff of the extracted text, a
multiset comparison of every number, a font check for italics and
superscripts, link-target extraction, and visual checks of the cover,
Tables 1-5 and Figure 1.

Result: the wording and every number match the submission. All
problems are conversion artefacts or older typos.

## A. Errors introduced by PCJ conversion (fix in docx)

| # | Page | Proof | Should be |
|---|------|-------|-----------|
| A1 | 10, 11 | `χ2` in Table 1 and Table 3 headers and Table 3 note: superscript lost | χ² |
| A2 | 19 | Badham et al. DOI shown as `...16168 79`; link target `...16168%2079` (broken) | https://doi.org/10.3389/feduc.2025.1616879 |
| A3 | 19 | Breakstone DOI split: `.2014.965860` on an unindented new line; link points to `https://doi.org/10.1080/00933104` (broken) | https://doi.org/10.1080/00933104.2014.965860 |
| A4 | 6 | "developed in Hartmann (2008)'s dissertation" | "Hartmann's (2008) dissertation" |
| A5 | 3 | "substan-tive concept deployment" (hard hyphen mid-line) | substantive |
| A6 | 6 | "mi-nor wording adjustments" | minor |
| A7 | 7 | "Fragebogen Recht-sextremer Einstellungen" | Rechtsextremer |
| A8 | 20 | "sec-ondary school students'" (Huijgen 2014) | secondary |
| A9 | 21 | "A. K. Dick-inson" (Shemilt 1984) | Dickinson |
| A10 | 4, 21 | "Ministerstvo školství mládeže a tělovýchovy" (text and reference) | Ministerstvo školství, mládeže a tělovýchovy |
| A11 | 6 | "(e.g. think-aloud interviews)" | (e.g., think-aloud ...) |
| A12 | 12 | `procedure", not "no DIF present."` (comma moved outside the closing quote) | `procedure," not` |
| A13 | 14 | "at or above β =.20" | β = .20 |
| A14 | 13 | Table 5 note "* p <.05" | p < .05 |
| A15 | 4 | Heading "2.2 Cross-Cultural Test Adaptation": no period, unlike all other headings | 2.2. |
| A16 | 7, 10, 11, 13, 15 | Statistical symbols set upright: SD (p7); p (p10, 4.2); α twice (Table 3 note); p (p13, history grade); β, SE, p, β, p (p13, 4.10); ω (p15) | italic |
| A17 | 12 | Table 4 "Note." not italic (other tables italic) | *Note.* |
| A18 | 13 | Table 5 row 3: "(rev.)" wraps and nearly touches row 4 | widen column or shorten label |

Note: A5-A9 are hard hyphens in the docx. The docx also has
"under-representation" (p16) as a hard hyphen; in this PDF it falls at a
line end, so it looks fine, but it will break if the text reflows (source:
"underrepresentation").

## B. Front page / metadata (tell PCJ; they regenerate it)

- B1 "Published 2026-10-20" is earlier than the recommendation
  (2026-10-22); presumably a placeholder, but confirm.
- B2 No keywords on the cover; the form had six keywords.
- B3 Abstract uses hyphen-minus and upright *r* ("r = -.05, 95% CI
  [-.16, .07]"); should be italic *r* and true minus signs (−.05, −.16).
- Names, ORCID, both affiliations, correspondence email, PCI
  recommendation DOI and abstract text: correct.

## C. Older errors also in the submitted PDF (optional, fix now)

- C1 p17, limitation 8: "cognitive- interview" and "option- matching"
  (stray space; source line-break in `manuscript.tex`) -> "cognitive-interview", "option-matching".
- C2 p11, Table 3 note: "p_raw", "p_adj" with literal underscores ->
  *p*<sub>raw</sub>, *p*<sub>adj</sub>.
- C3 p19: "The wiley international handbook" -> Wiley; p21: "canadian
  social studies" -> Canadian (style lowercased protected nouns).
- C4 p20: Kaleta (2025a) "22(1), 14." -> "22(1), Article 14." (APA 7).
- C5 p13, heading 4.10 "Ideology-HPT" uses a hyphen; body text uses
  "ideology--HPT" (en dash).

## D. Changes that are acceptable (no action)

- PCJ re-sorted "van Boxtel" and "van Drie" under B and D (Dutch
  convention); the submission sorted them under V.
- Data-availability statement now cites the preregistration as
  https://doi.org/10.17605/OSF.IO/ZSNGY (same record).
- Section headings gained a final period ("2.1."); body text reflowed to
  Arial; figure image is intact and legible.
