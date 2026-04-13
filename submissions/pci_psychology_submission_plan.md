# PCI Psychology Submission Plan — Paper B

**Created:** 2026-04-13
**Status:** Draft
**Source manuscript:** `_archive/ijt_2026-04/manuscript.md`
(IJT version, 5,907 words excl. abstract/tables/references)

---

## Overview

PCI Psychology uses a two-stage process: (1) deposit a preprint
on a public server (PsyArXiv recommended), then (2) submit the
preprint link to PCI Psychology for evaluation. If recommended,
the paper can be published in Peer Community Journal (Diamond OA,
WoS + Scopus, IF 2.0, no fees).

The IJT manuscript is close to submission-ready. PCI Psychology
uses **format-free submission** (APA 7 is acceptable), so the
core text requires minimal reformatting. The main work is:
adding required transparency materials, adjusting framing from
"testing journal" to "quantitative psychology" audience, posting
the preprint, and completing the submission form.

---

## Phase 1: Manuscript Adaptation

### 1.1 Reframe for Quantitative Psychology audience

The IJT version frames contributions for a "testing/measurement"
audience. PCI Psychology's Quantitative Psychology section covers
psychometrics, measurement models, and test fairness — similar
but broader. Key adjustments:

- [ ] **Title**: Keep as-is. "Cross-Cultural Validation and
  Ideological Fairness" is self-explanatory and
  audience-neutral. No change needed.

- [ ] **Abstract**: Review for journal-specific language. Remove
  any IJT-specific phrasing. The current abstract is clean —
  likely needs only minor wording tweaks (e.g., remove
  "transferable protocol" if it reads as too niche).

- [ ] **Introduction framing**: The intro currently says "For a
  measurement audience, the question is whether..." (line 66).
  Soften to a general statement — PCI readers span educational
  psychology, quantitative psychology, and social psychology.
  Replace "measurement audience" with "researchers using
  scenario-based assessments" or similar.

- [ ] **Contribution statement** (end of Section 1, lines 68–81):
  Review the three listed contributions. They are already
  general enough. No major rewrite expected, but check that
  they don't reference "testing journals" or "the measurement
  community" in a way that sounds like a journal-fit argument.

- [ ] **Discussion Section 5.5** ("Implications for Assessment
  Practice", lines 406–438): Currently frames the CIV protocol
  as generalizable to "any scenario-based assessment." This is
  the paper's strongest selling point for PCI Psychology. Ensure
  it reads as a contribution to quantitative/educational
  psychology, not just to measurement journals. The four-domain
  examples (moral reasoning, science literacy, civic education,
  HPT) are excellent — keep them.

- [ ] **Discussion Section 5.8** (Conclusion, lines 448–451):
  "For the measurement community" → "For researchers and
  assessment developers" or similar broadening.

- [ ] **Remove all traces of IJT/AME framing**: Search for any
  references to "International Journal of Testing", "Applied
  Measurement in Education", or "as suggested by the editorial
  office." These should not appear in the manuscript (they were
  only in the cover letter, but double-check).

### 1.2 Add PCI-required sections and statements

- [ ] **Conflict of Interest statement** — replace the current
  "The author reports there are no competing interests to
  declare" with PCI's exact required wording:

  > The authors declare that they comply with the PCI rule of
  > having no financial conflicts of interest in relation to the
  > content of the article, and have no non-financial conflicts
  > of interest.

  (Use "author" singular if single-author paper is acceptable
  with this wording; check PCI template.)

- [ ] **CRediT author contribution statement** — add a new
  section after Funding. For a single-author paper:

  > **Author Contributions (CRediT)**
  > Juda Kaleta: Conceptualization, Methodology, Software,
  > Formal analysis, Investigation, Data curation, Writing —
  > original draft, Writing — review & editing, Visualization,
  > Project administration.

- [ ] **TOP Disclosure Table** — download the PCI template
  (Word or Google Doc version) and complete it. Append as the
  final page of the preprint. The table requires:
  - Citation standard (all refs have DOIs where available — ✓)
  - Data transparency (OSF: https://osf.io/yng37/ — ✓)
  - Analytic code transparency (OSF scripts — ✓)
  - Materials transparency (OSF instruments — ✓)
  - Design transparency (preregistered on OSF — ✓)
  - Preregistration of studies (yes, OSF — ✓)
  - Preregistration of analysis plan (yes, OSF — ✓)
  - Replication (first adaptation — N/A or "not applicable")

  Most rows should be straightforward given the existing OSF
  package. Key: provide direct URLs in each row, not just
  "available upon request."

- [ ] **Data Availability Statement** — the existing statement
  is good but verify it mentions:
  - The OSF URL with DOI (not just https://osf.io/yng37/)
  - That data are in machine-readable formats (RDS, xlsx)
  - That README files are included
  - That analysis scripts are annotated and functional

- [ ] **Line numbering** — add continuous line numbers to the
  manuscript. This is a formatting requirement at the preprint/
  PDF level, not in the Markdown source. Handle when building
  the PDF/DOCX for PsyArXiv.

### 1.3 Manuscript cleanup

- [ ] **Remove journal-specific metadata**: Delete the "Running
  head:" line (APA 7 dropped running heads; PCI doesn't need
  them). Remove "Submission date: April 3, 2026" or update it.

- [ ] **Remove Biographical Note** section — PCI doesn't require
  it and it's unusual for preprints. Author info is in the
  header.

- [ ] **ORCID**: Already present in the author block (✓). Good.

- [ ] **Funding section**: Already present ("no specific
  grant..."). Keep as-is (✓).

- [ ] **Ethics statement**: Already present and adequate (✓).
  PCI requires affirmation of Helsinki compliance for human
  subjects — the current statement covers voluntary
  participation, de-identification, and non-interventional
  nature.

- [ ] **References**: Check that all citations include DOIs
  where available. PCI requires this. Run a spot-check on 5–10
  key references.

- [ ] **Figures/Tables**: May be embedded or appended — current
  in-text placement is fine. Ensure all figures are available
  as separate PNG/PDF files for the preprint.

---

## Phase 2: OSF Repository Preparation

The OSF replication package (`osf_storage/`) already exists and
is well-structured. Verify it meets PCI's stricter requirements:

- [ ] **README quality**: Read `osf_storage/README.md` and check
  it explains: purpose of each directory, how to run scripts,
  required R packages, expected runtime, and how results map to
  manuscript claims.

- [ ] **Script functionality**: PCI reviewers may actually
  execute scripts. Verify:
  - Scripts use relative paths (not absolute)
  - File names in scripts match actual file names
  - Required packages are listed or installed in scripts
  - Comments are in English
  - Scripts produce output matching manuscript tables/figures

- [ ] **Machine-readable formats**: Data is in RDS + xlsx (✓).
  Codebook is PDF — acceptable but consider adding a
  machine-readable version (CSV data dictionary) if easy.

- [ ] **OSF DOI**: Check whether the OSF project has a DOI
  assigned. PCI requires a permanent identifier. If not,
  register one on OSF (Settings → DOI → Create DOI). A plain
  osf.io URL may suffice but a DOI is safer.

---

## Phase 3: PsyArXiv Preprint

PCI Psychology requires the manuscript to be publicly available
on a preprint server before submission.

- [ ] **Create PsyArXiv account** (if not already existing) at
  https://psyarxiv.com/ (uses OSF credentials).

- [ ] **Build preprint PDF/DOCX**: Convert the adapted manuscript
  to a clean PDF with:
  - Line numbers (continuous)
  - Embedded tables and figures
  - TOP disclosure table as final page(s)
  - All author metadata (name, affiliation, email, ORCID)

- [ ] **Upload to PsyArXiv**:
  - Title, authors, abstract
  - Link to OSF project for data/code
  - Select appropriate subject tags (e.g., Quantitative
    Psychology, Educational Assessment, Psychometrics)
  - License: CC-BY recommended for PCI compatibility

- [ ] **Wait for PsyArXiv moderation** (usually 1–3 days).
  Record the preprint DOI once assigned.

---

## Phase 4: PCI Psychology Submission

Once the preprint is live on PsyArXiv:

- [ ] **Register/log in** at
  https://psych.peercommunityin.org/

- [ ] **Fill submission form** with:
  - Preprint URL (PsyArXiv DOI link)
  - Title, authors, abstract
  - Version number (v1)
  - Data/code/materials locations (OSF URL + DOI)
  - Funding information
  - Up to 3 **thematic fields** — recommend:
    1. Quantitative Psychology
    2. Educational Psychology
    3. Social Psychology (if available; otherwise
       Developmental Psychology)

- [ ] **Submission icon**: Upload an original PNG/JPG image.
  Must NOT be AI-generated. Options:
  - One of the manuscript figures (e.g., the measurement
    invariance + DIF figure, or the coefficient plot)
  - A photograph of the original instrument or research
    setting
  - A diagram created by the author

- [ ] **Suggest recommenders** (5–10 people with PCI accounts
  or who could be invited). Ideal profile: expertise in
  psychometrics, DIF/measurement invariance, educational
  assessment, or cross-cultural test adaptation. Candidates:
  1. Tim Huijgen (Groningen) — HPT measurement expert
  2. Ulrich Hartmann (DIPF) — HPT instrument developer
  3. W. Holmes Finch (Ball State) — DIF methodology
  4. Mark D. Smith (NIU/SHEG) — history assessment
     psychometrics
  5. André Kretzschmar — quantitative psych, PCI-active
  6. Others: check PCI Psychology recommender list for
     active quantitative psychologists

- [ ] **Suggest reviewers** (up to 10): Same candidates as
  above plus additional measurement/fairness experts.

- [ ] **Opposed reviewers** (up to 5): Consider whether any
  of the 4 rejecting editors should be excluded (probably
  not necessary — they rejected on scope, not quality).

- [ ] **Cover letter** (optional but recommended): Draft a
  brief letter emphasizing:
  - The generalizable CIV detection protocol (not just HPT)
  - Preregistration + open data/code
  - Why this fits Quantitative Psychology scope
  - Do NOT mention previous rejections unless strategically
    useful
  - Do NOT mention specific journal names that rejected it

- [ ] **Check "anonymous submission"?** — Probably no. Single
  author, preprint is public, OSF is linked to the author.
  Anonymous submission would be inconsistent.

---

## Phase 5: Post-Submission

- [ ] **Monitor PCI Psychology dashboard** for managing board
  triage decision (expect 1–4 weeks).

- [ ] **Respond to any "desk revision" requests** — PCI may
  ask for specific improvements before sending to a
  recommender (unlike traditional desk rejection, this is an
  invitation to revise and resubmit quickly).

- [ ] **If a recommender accepts**: Expect first decision
  within ~60 days. Prepare for open peer review — all reviews
  and responses will be published if recommended.

- [ ] **Update status.md and project_status.md** after each
  milestone.

---

## Changes Summary: IJT → PCI Psychology

| Element | IJT version | PCI version |
|---------|-------------|-------------|
| Running head | Present | Remove |
| Submission date | April 3, 2026 | Remove or update |
| COI statement | Generic | PCI-specific wording |
| CRediT roles | Absent | Add |
| TOP disclosure table | Absent | Add (appended) |
| Line numbers | Absent | Add |
| Biographical note | Present | Remove |
| Data availability | OSF link | OSF link + verify DOI |
| "measurement audience" | Present (line 66) | Broaden |
| "measurement community" | Present (line 450) | Broaden |
| Preprint server | N/A | PsyArXiv (new) |
| Submission platform | ScholarOne (T&F) | PCI Psychology portal |

---

## Risk Assessment

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| No recommender volunteers | Medium | Suggest 5–10 well-matched names; CIV protocol framing broadens appeal beyond HPT specialists |
| Slow review (>60 days) | Medium | PCI commits to 60-day target; patience required |
| Reviewer requests major revisions on clustered SE handling | Medium | Design effects already computed and discussed; can add TYPE=COMPLEX sensitivity check if needed |
| "CFI=.999 is suspicious" concern | Low-Medium | Already addressed in manuscript (Section 4.1, lines 237–248); low-df structural explanation |
| Managing board triage rejection | Low | PCI doesn't desk-reject on scope the way traditional journals do; recommenders self-select |

---

## Timeline Estimate

| Step | Duration |
|------|----------|
| Phase 1: Manuscript adaptation | 1–2 sessions |
| Phase 2: OSF verification | 1 session |
| Phase 3: PsyArXiv upload + moderation | 1–3 days |
| Phase 4: PCI submission | Same day as preprint goes live |
| Phase 5: Triage → recommender assignment | 1–4 weeks |
| First decision | ~60 days after recommender accepts |
