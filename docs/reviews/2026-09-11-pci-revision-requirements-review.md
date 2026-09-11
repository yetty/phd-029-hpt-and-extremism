# PCI Psychology revision requirements review

**Reviewed:** 2026-09-11  
**Scope:** Revised manuscript, supplement, response letter, revision ledger,
source reviews, analysis scripts, and rendered preprint  
**Reviewers simulated:** Methodologist, psychometrician, history-education
domain expert, sceptic, editor, lay reader, stylist, reference verifier, and
requirement-traceability auditor

## Overall verdict

**Not ready for resubmission.**

The revision responds well to most of the recommender's explicit requests and
many of Reviewer 1's and Reviewer 2's suggestions. The single-scenario limit,
readable construct names, confidence intervals, scale reliability, corrected
voting scenario, descriptive relationship figure, and bounded DIF language are
all substantial improvements.

Two technical discrepancies nevertheless prevent resubmission:

1. The reported inadmissible multi-group CFA solutions do not reproduce from
   the current data and model specification. The same models produce admissible
   solutions with no negative residual variances.
2. The focal regression results do not reproduce under the missing-data rules
   stated in the manuscript. The current verification script, the manuscript,
   and the manuscript's stated scoring rules produce three different estimates.

Several response-letter claims also exceed what the revised manuscript
actually implements. These are bounded corrections, not grounds for redesigning
the study, but the technical issues require a fresh analysis freeze and rebuild.

Empty literature-note stubs and reading-status propagation were ignored as
requested. All active manuscript citation keys resolve through the current
bibliography setup. References kept only in `extras.bib` are treated here as
repository hygiene, not as a manuscript-readiness blocker.

## Requirement traceability

The ledger contains 23 identifiers: 5 recommender items, 14 Reviewer 1 items,
3 Reviewer 2 items, and META-01. The ledger's statement of 22 substantive
items appears to omit META-01 from the count.

| ID | Status | Assessment |
|---|---|---|
| REC-01 | Partly met | The dedicated single-scenario limitation is strong, but later references to "observed measurement equivalence" and content "without detectable measurement bias" exceed the available evidence. |
| REC-02 | Met | The manuscript no longer treats factor correlations below .85 as discriminant-validity proof and calls for differential prediction of external criteria. |
| REC-03 | Met | Results prose and Tables 2, 4, and 7 use readable construct names. "Context" in Table 7 could still be expanded. |
| REC-04 | Met | Table 7 supplies Fisher-transformed 95% intervals, identifies pairwise handling, and points to exact sample sizes. |
| REC-05 | Partly met | The breadth-precision trade-off and attenuation are discussed, but one-factor omega values are used to recommend multidimensional composites. The response also inaccurately calls all alpha intervals bootstrap intervals. |
| R1-01 | Partly met | The abstract is much clearer, but its MG-CFA qualification is based on a non-reproducing diagnosis, and the blanket null summary omits the exploratory authoritarianism-presentism association. |
| R1-02 | Partly met | The fairness workflow is more prominent, but Section 5.5 calls it an "evidence-based model" and then devotes substantial space to untested pathways. |
| R1-03 | Partly met | The two ideology pathways are described, but moral evaluation, affective engagement, contextual reconstruction, and CIV are not integrated into one explicit construct-boundary account. |
| R1-04 | Met | The German dissertation and Hartmann-Hasselhorn article are cited according to their distinct source roles. |
| R1-05 | Met | Hartmann-Hasselhorn and Lee-Ashby render as author-date citations rather than titles. |
| R1-06 | Partly met | The unsupported validation superlative is gone, but the Dutch evidence map omits the adverse topic-transfer, reliability, and item-interpretation findings. |
| R1-07 | Met | The manuscript consistently describes voting in the 1930 election rather than joining an extremist movement. |
| R1-08 | Partly met | Lower-secondary curriculum context is documented, but 70.3% of the sample is upper-secondary and that level is not covered by the cited framework or prose. |
| R1-09 | Met | Reliability and source-population suitability are reported and the lack of direct Czech-adolescent validation is acknowledged. |
| R1-10 | Partly met | Detail was moved to the supplement and focal relationships were added, but eight technical subsections still precede the accessible focal findings without an opening Results roadmap. |
| R1-11 | Partly met | Think-aloud precedent is credited, but the topic/format comparison claimed in the response letter is not described in the manuscript. |
| R1-12 | Met | The unsupported gender-moderation generalisation was removed and replaced with a design-specific limitation. |
| R1-13 | Partly met | Think-aloud and cognitive-interview work is proposed, but observable indicators for each competing pathway are not specified. |
| R1-14 | Met | Verified Huijgen studies are named and the vague comparative-use claim is removed. |
| R2-01 | Met | Two numbered questions are prominent and Table S5 maps registered hypotheses and deviations. RQ1/RQ2 labels would improve navigation but are not essential to satisfy the original checklist concern. |
| R2-02 | Partly met | Ideology and knowledge distributions are reported in Results, not "up front" in Participants, and unavailable SES information is not explicitly framed as a sampling limitation. |
| R2-03 | Met | Table 7 and Figure 1 provide the requested accessible, continuous, unadjusted relationships with uncertainty and sample sizes. |
| META-01 | Unmet | The current audit found unresolved model, scoring, and reporting discrepancies. The planned human specialist check also remains pending. |

**Summary:** 11 met, 11 partly met, 1 unmet.

## Critical issues

### 1. The MG-CFA inadmissibility claim does not reproduce

**Affected requirements:** META-01, REC-01, R1-01, R1-02  
**Locations:** Abstract; Sections 4.7, 5.1, 5.4, and 5.8; response META-01

The manuscript states that negative ROA residual variances occurred in the Low
ideology group in the configural and metric models and that these Heywood cases
make the solutions inadmissible. A fresh run of the exact three-factor WLSMV
models with the current data and composite-ideology tertiles reproduced the
published fit indices but not the diagnosis:

| Model | Complete Low | Complete High | CFI | RMSEA | SRMR | Post-check | Negative residuals |
|---|---:|---:|---:|---:|---:|---|---:|
| Configural | 92 | 91 | .978 | .043 | .074 | Passed | 0 |
| Metric | 92 | 91 | .978 | .041 | .081 | Passed | 0 |
| Scalar | 92 | 91 | .980 | .035 | .076 | Passed | 0 |

All residual-variance diagonals were positive. This was independently flagged
by the psychometric reviewer and reproduced again during consolidation using
lavaan 0.7-2.

The manuscript's statement therefore appears to preserve a warning from an
earlier environment or analysis without archiving the exact problematic
estimates. The revision cannot simultaneously claim that current scripts
reproduce the results and use a diagnosis that those scripts do not produce.

**Required action:** Freeze the data, lavaan version, model syntax, grouping,
missing-data treatment, and diagnostics. If the Heywood cases cannot be
reproduced, remove that explanation and reinterpret the invariance sequence
from the reproducible output, retaining the important small-group, clustering,
and post-registration limitations.

### 2. The reported focal regression does not match the stated scoring rules

**Affected requirement:** META-01  
**Locations:** Sections 3.4 and 4.10; analysis script 03; verification script

Section 3.4 says scores require at least two answers per three-item HPT
subscale, four of six FR-LF items, seven of nine KSA-3 items, and four of five
SDR-5 items. The primary multilevel script and `verify_statistics.R` instead
use `rowMeans(..., na.rm = TRUE)` without these thresholds.

A fresh comparison produced:

| Source/rule | N | FR-LF beta | SE | p | Singular fit |
|---|---:|---:|---:|---:|---|
| Current script behaviour | 285 | .0094 | .0668 | .888 | Yes |
| Manuscript minimum-answer rules | 282 | .0157 | .0675 | .817 | Yes |
| Reported manuscript value | not stated locally | .012 | .067 | .863 | not disclosed |

The substantive conclusion remains null, but exact reporting and
reproducibility do not. The main script also produces a singular fit, which the
manuscript does not report or explain.

**Required action:** Select one documented scoring rule, apply it consistently
to all analyses, rerun every affected model and sensitivity analysis, report
analysis-specific sample sizes, and rebuild all manuscript and supplement
values from the frozen outputs.

## Major issues

### 3. A non-corrected authoritarianism-presentism association is omitted

**Affected requirements:** R1-01, R1-03  
**Locations:** Abstract; Sections 4.9, 4.10, 5.1, and 5.8

Table 7 reports an authoritarianism association with reversed present-oriented
perspective of `r = -.13`, with a 95% interval excluding zero. The archived
multilevel output reports `beta = -.159`, 95% CI `[-.288, -.031]`, `p = .016`.
The effect does not survive the documented multiple-comparison correction and
should not be promoted as a confirmed finding. It nevertheless contradicts
blanket statements that authoritarianism was unrelated to all HPT outcomes and
that knowledge was the only consistent predictor.

**Required action:** Report this as an exploratory, multiplicity-sensitive
association or narrow every null statement to the primary composite and the
right-authoritarian attitude measure. Recompute it after resolving the scoring
rules.

### 4. The metric-model modification index is misinterpreted

**Affected requirement:** META-01  
**Location:** Section 4.7

The largest modification index of 11.28 proposes a POP2 cross-loading on ROA in
the Low group. It is not a test of a between-group loading difference and does
not support the manuscript's phrases "partial loading non-invariance" or
"minor loading difference." Lavaan also warns that ordinary modification
indices ignore equality constraints.

**Required action:** Delete the non-invariance interpretation or test the
loading-equality constraints directly with an appropriate score-test procedure.
Keep cross-loading misspecification separate from group non-invariance.

### 5. Some broad fairness claims exceed the bounded evidence

**Affected requirements:** REC-01, R1-02  
**Locations:** Sections 4.9, 5.1, 5.5, and 5.8

Examples include "measures disciplinary reasoning rather than attitudes or
impression management," "evidence-based model," "observed measurement
equivalence," and the conclusion that sensitive content was used "without
detectable measurement bias." These formulations are difficult to reconcile
with a single scenario, a scenario-proximal knowledge measure, SDR-5 alpha of
.30, unknown DIF sensitivity, no response-process evidence, and the unresolved
MG-CFA analysis.

**Required action:** Use compatibility language. State that no association or
DIF flag was detected at the study's sensitivity and that the pattern is
consistent with, but does not establish, the intended interpretation. Call the
protocol an illustrative or possible workflow rather than an evidence-based
model.

### 6. The follow-up evidence map is not balanced

**Affected requirements:** R1-06, R1-11  
**Location:** Section 2.1 and corresponding response entries

The revision correctly removes the unsupported claim that this is one of the
most extensively validated European HPT measures. It reports Dutch replication
but omits the adverse transfer evidence highlighted by Reviewer 1: the slavery
scenario's weak reliability and validity support and later evidence of item
misunderstanding. The response letter also says that topic/format extensions
are distinguished when the manuscript does not make that distinction.

**Required action:** Add one balanced sentence separating successful reuse of
the Nazi scenario from difficulties transferring the item format to another
topic and from later think-aloud evidence. Narrow the response if no
open-versus-closed comparison can be verified.

### 7. The Czech context omits the sample's majority educational level

**Affected requirement:** R1-08  
**Locations:** Section 2.2 and response R1-08

The curriculum account is accurate for lower-secondary Grades 6-9. However,
206 of 293 participants were upper-secondary students, and the cited RVP ZV
does not govern that level. The response therefore answers only part of the
reviewer's question about whether history is compulsory and how related
competences appear in the curriculum.

**Required action:** Add an official, bounded account of upper-secondary
history provision and relevant curriculum expectations. If that evidence is
not added, state explicitly that the documentary curriculum account applies
only to the lower-secondary subsample.

### 8. The construct-boundary synthesis remains incomplete

**Affected requirements:** R1-03, R1-13  
**Locations:** Sections 2.1, 2.3, and 5.6; response R1-03

The manuscript separately discusses rational reconstruction, affective HPT,
attitude congruence, and integrative complexity. It does not explicitly connect
them to explain when affect or moral engagement is target variance under a
broader definition and when attitude-congruent endorsement is CIV under this
instrument's narrower definition. The proposed qualitative follow-up also
does not specify indicators for each pathway.

**Required action:** Add one concise synthesis and name observable indicators
for contextual evidence use, integration of conflicting considerations, moral
evaluation, affective engagement, option matching, and attitude-congruent
endorsement.

### 9. The response-letter location map is inaccurate

**Affected requirement:** Final package checks  
**Location:** Response letter, location map

The current 54-page preprint starts major sections on these pages:

| Location | Actual page |
|---|---:|
| Abstract | 1-2 |
| Section 2.1 | 5 |
| Section 2.2 | 7 |
| Section 2.3 | 9 |
| Section 2.4 | 10 |
| Section 2.5 | 12 |
| Section 3.1 | 12 |
| Section 3.2 | 13 |
| Section 3.4 | 16 |
| Section 4.1 | 19 |
| Section 4.5 | 24 |
| Section 4.6 | 25 |
| Section 4.9 | 29 |
| Section 4.10 | 30 |
| Section 5.1 | 32 |
| Section 5.8 | 38 |
| References | 41 |

The response map is generally one page late after the abstract and omits
Sections 2.4, 4.5, and 5.1. Individual replies also lack precise page locators.

**Required action:** Regenerate the map after the final PDF build and add exact
section/page references to each response where practical.

## Moderate and minor issues

1. **R2-02:** Move a concise ideology and knowledge distribution summary, or a
   direct Table 4 cross-reference, into Participants. State that absent SES
   information limits sample characterisation.
2. **R1-10:** Add a two- or three-sentence Results roadmap before Section 4.1
   so readers encounter the two substantive answers before eight technical
   subsections.
3. **R2-01:** Numbered questions are sufficient, but explicit RQ1/RQ2 labels
   reused in Results and Discussion would improve navigation.
4. **REC-03:** Expand "Context" in Table 7 to "Contextualization" and consider
   making the caption explain that coefficients are below the diagonal and
   intervals above it.
5. **R2-03:** Add a plain-language first sentence to the Figure 1 caption and
   define what higher historical-reasoning scores mean.
6. **REC-05:** Correct the response letter: alpha intervals use a standard-error
   approximation; only the Mosier combined-ideology interval is bootstrapped.
7. **REC-05:** Treat one-factor omega for multidimensional composites as a
   descriptive consistency index unless a reliability coefficient aligned with
   the intended composite model is supplied.
8. **REC-05:** Tighten the Loevinger paraphrase. The source supports the
   breadth-precision principle, but not necessarily the phrase "repeated
   versions of a narrow question."
9. **R1-02:** Reduce the three post-protocol mechanism paragraphs or clearly
   mark them as secondary explanations to preserve the protocol-led focus.
10. The manuscript should report the focal regression formula, model-specific
    sample size, degrees-of-freedom method, and handling of singular fits.

## Strengths to preserve

- The single-scenario content-underrepresentation limitation is explicit and
  much stronger than in the submitted manuscript.
- Discriminant-validity language is appropriately reduced to
  internal-structure evidence.
- Table 7 includes the requested uncertainty and exact pairwise sample sizes
  are available in the supplement.
- Figure 1 directly addresses Reviewer 2's request for a simple baseline
  display of ideology-HPT and knowledge-HPT relationships.
- The voting scenario is corrected consistently and source lineage is clear.
- Reliability is no longer excused merely because the scales are short.
- DIF language distinguishes failure to detect DIF from proof of no DIF and
  states the unknown sensitivity of the design.
- Registered omissions and post-registration additions are disclosed unusually
  candidly in Section 2.5 and Table S5.
- The response letter is professional, appreciative, and non-defensive.

## Minimum resubmission sequence

1. Freeze the current data, scoring rules, package versions, and public scripts.
2. Reproduce and resolve the MG-CFA discrepancy.
3. Apply one scoring rule across all analyses and rerun affected estimates.
4. Correct the modification-index interpretation and report the exploratory
   authoritarianism-presentism result accurately.
5. Recalibrate the broad fairness and protocol claims.
6. Complete the bounded reviewer-facing additions for R1-03, R1-06, R1-08,
   R1-11, R1-13, and R2-02.
7. Correct the response letter, especially the location map and REC-05,
   R1-03, and R1-11 claims.
8. Rebuild the manuscript, supplement, redline, and response package from the
   frozen outputs.
9. Run an independent psychometric reproduction check and obtain the planned
   targeted human methods review.

After steps 1-8 pass, the manuscript should be close to resubmission. Step 9 is
strongly recommended because both external reviewers explicitly disclaimed
technical expertise and the present audit found consequential model drift.
