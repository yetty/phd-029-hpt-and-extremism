# Peer Review Report -- manuscript.tex (journal)

**Reviewed:** 2026-04-13  
**Scope:** Full document (758 lines, LaTeX)  
**Profile:** journal (7 reviewers + reference-verifier)  
**Target:** PCI Psychology submission

---

## Critical Issues (must address)

| # | Reviewer | Issue | Location | Suggestion |
|---|----------|-------|----------|------------|
| 1 | psychometrician | **NS-only scalar invariance exceeds ΔCFI threshold.** The scalar ΔCFI for NS-only grouping is +.014, exceeding the study's own stated criterion of ≤ .01. The text says "confirmed scalar invariance" — this is internally inconsistent. Group sizes are larger here (n = 126/125), making the breach more meaningful, and NS is described as the most theoretically proximal facet. | §4.8, L421 | Acknowledge the ΔCFI criterion breach. Either test partial scalar invariance (free non-invariant thresholds) or explicitly state this is a qualified rather than confirmed finding. Cite Byrne et al. (1989). |
| 2 | sceptic | **Conclusion overclaims relative to evidence.** "The instrument measured what it claims to measure — and ideology had nothing to do with the score." The evidence base is internal-structural, conducted in a range-restricted sample (17.7% above midpoint). None of the analyses establishes that the instrument measures disciplinary reasoning rather than general academic disposition, scenario familiarity, or social desirability compliance. | §5.8, L544 | Rewrite to reflect what was demonstrated: no detectable measurement bias at these sensitivity levels, in this ideological range. Remove the causal-sounding "ideology had nothing to do with the score." |
| 3 | sceptic | **Convergent validity is scenario-contaminated.** KN items were written to cover content relevant to the HPT scenario. The r = .22–.34 may reflect shared specific-content activation rather than the broader "knowledge-dependent competence" claim. This is acknowledged as a caveat in §4.9 but continues to be used as validity evidence in §5.4. | §4.9, §5.4 | Reframe as "scenario-relevant knowledge predicts HPT scores" without generalising to the broader competence claim, or acknowledge this as a substantive validity threat, not just a caveat. |
| 4 | psychometrician | **DIF testing procedure ambiguously described.** L240 says "all-others-as-anchor strategy" but the Table 5 note says "each test compares a fully constrained baseline (all items equal across groups) to a model freeing discrimination and threshold parameters for the tested item." These are different procedures with different error properties. | §3.4, L240 | Clarify the exact DIF procedure: does the reference model constrain all 9 items or 8 (freeing focal only)? Re-characterise Woods (2009) implications accordingly. |
| 5 | sceptic, methodologist | **DIF anchor contamination pattern.** Five of nine items have raw p < .10 (POP1, POP3, ROA1, ROA3, CONT3). This clustering is consistent with multiple items sharing weak DIF in the same direction — exactly the scenario where the no-purification anchor strategy fails. The paper cites Woods (2009) but does not discuss whether the observed cluster constitutes evidence this failure mode occurred. | §4.6, Table 5 | Discuss the directional clustering of near-significant raw p-values as potential anchor contamination. Run iterative purification as sensitivity check, or report summed chi-square across items before Bonferroni. |
| 6 | psychometrician | **Bifactor model df not derived transparently.** The bifactor (1 general + 3 specific) reports df = 18 but no parameter count is given. Standard bifactor for 9 items should yield different df depending on parameterisation. Without derivation, the ΔCFI = .001 comparison to the correlated 3-factor model is uninterpretable. | §4.1, L278 | Report full parameter count and df derivation. Verify identification per Reise (2012). |
| 7 | psychometrician | **Heywood cases dismissed too casually.** Negative estimated residual variances for ROA items in the Low ideology group indicate a non-positive-definite covariance matrix, which can invalidate fit statistics. Calling this "a known estimation artifact" understates the problem. | §4.7, L414 | Report specific affected parameters and estimated values. Run sensitivity analysis without affected items. Cite Kolenikov & Bollen (2012). Qualify configural/metric conclusions accordingly. |
| 8 | layperson | **CIV not spelled out in abstract.** "Construct-irrelevant variance" is used in the abstract (L109) without being spelled out; the definition appears only at L116. | Abstract, L109 | Spell out "construct-irrelevant variance (CIV)" on first use in the abstract. |
| 9 | layperson | **POP abbreviation etymology opaque.** "Presentism (POP)" — the O and P in POP derive from "Othering of People" but this phrase never appears. Readers will not connect the abbreviation to its source. | L118 | Spell out: "presentism/othering of people (POP)" or note the German instrument etymology. |

---

## Major Suggestions (should address)

| # | Reviewer | Issue | Location | Suggestion |
|---|----------|-------|----------|------------|
| 10 | methodologist | **No power analysis for DIF or MG-CFA.** Power analysis targets β ≥ .15 in multilevel regression but the primary fairness tests use different analytic frameworks (GRM DIF, MG-CFA). No power analysis is reported for either. | §3.2 | Report sensitivity analysis for GRM LR-DIF at n ≈ 96/group, Bonferroni α = .01, quantifying minimum detectable Δβ. |
| 11 | methodologist | **Gender imbalance (65.5% male) unaddressed analytically.** Gender moderates authoritarianism in adolescents. No DIF or sensitivity analysis by gender is reported, leaving open whether the null finding is driven by gender composition. | §3.2, §5.6 | Add gender-stratified descriptives for ideology and HPT. Conduct DIF by gender as sensitivity check, or report gender × ideology interaction. |
| 12 | sceptic | **Cancelling pathways not discussed.** The near-zero ideology correlation (r = −.05) is consistent with two opposing moderate-sized effects (congruence pathway vs. authoritarian reduced integrative complexity) that cancel out. The dual-process alternative is mentioned in §2.3 but abandoned. | §5.4 | Discuss whether the null could reflect two cancelling pathways rather than a clean absence of ideological influence. |
| 13 | sceptic | **TOST SESOI wider than preregistered power target.** SESOI ±0.20 vs. preregistered β ≥ .15. Effects between .15 and .20 cannot be excluded. The Discussion uses TOST as substantive support despite this gap. | §4.10, §5.4 | State whether SESOI was preregistered or post hoc. Report TOST at ±0.15 for transparency. Frame the ±0.20 result as exploratory. |
| 14 | sceptic | **Social desirability / task demand as alternative explanation for equivalence.** In a school context, students may uniformly refuse to contextualise a Nazi-adjacent figure regardless of ideology — not because of deliberative reasoning but because contextualisation is socially unsafe. This alternative is not considered. | §5.5 | Add a paragraph considering task-demand compliance as an alternative mechanism for measurement equivalence. |
| 15 | methodologist | **TOST SESOI specification is a researcher degree of freedom.** Post hoc SESOI larger than preregistered effect size moves the equivalence boundary in the direction favouring the null. | §4.10 | Present TOST at ±0.20 as exploratory sensitivity analysis, not confirmatory. Report ±0.15 result alongside. |
| 16 | psychometrician | **SRMR threshold not validated for WLSMV.** The ≤ .08 threshold is from continuous-indicator ML literature. SRMR behaviour differs for WLSMV-estimated models with polychoric residuals. Metric SRMR = .081 exceeds this unvalidated threshold without comment. | §3.4, L232 | Acknowledge threshold limitation. Cite Xia & Yang (2019). Note the metric SRMR = .081 exceedance. |
| 17 | psychometrician | **Omega hierarchical not computed.** ωh from the already-fitted bifactor model would clarify how much composite variance is attributable to general HPT vs. subscale-specific factors — key for interpreting HPT_CTX6 as unidimensional. | §4.3 | Compute ωh via omegaFromSem() and report in Table 3 or supplementary. Per Rodriguez et al. (2016), ωh < .50 means subscale score is uninterpretable as specific construct. |
| 18 | psychometrician | **Ideology composite reliability not reported.** The disattenuated r = −.07 requires reliability of both variables. Reliability of the composite ideology score (FR-LF + KSA-3) is never stated. | §4.3, L328 | Report α/ω for the composite ideology score to make the disattenuated estimate verifiable. |
| 19 | domain-expert | **Citation form inconsistency: "Drie & Boxtel" vs. "van Drie & van Boxtel."** The de-particle form differs between in-text citations and reference list. This risks database disambiguation failures. | L116, throughout | Standardise to "van Drie & van Boxtel (2007)" in all in-text citations. |
| 20 | domain-expert | **Missing key literature: Endacott & Brooks (2013), Maggioni (2010), Seixas & Peck (2004), Lee & Shemilt (2003).** The empathy/perspective-taking debate stops at 2004 (Lee/Ashby vs. Barton/Levstik). Maggioni's epistemic stances are directly relevant to distinguishing reasoning from attitude-driven responding. | §2.1 | Add Endacott & Brooks (2013) for current empathy debate; Maggioni (2010) for epistemic stances; Seixas & Peck (2004) for second-order concept framework; Lee & Shemilt (2003) for developmental progression. |
| 21 | domain-expert | **"Second-order concepts" undefined.** The manuscript uses "substantive concept deployment" and "disciplinary achievement" without defining the meta-level distinction between second-order (procedural/disciplinary) and first-order (substantive) concepts. | §2.1, L134 | Add one sentence defining second-order vs. substantive concepts. |
| 22 | editor | **Intro paragraph 1 is 106 words with three parenthetical citation clusters.** Reader must re-read to follow the logic. | L116 | Break at the em-dash: end first sentence after "(AERA et al., 2014; Hambleton et al., 2005; ITC, 2017)." Begin new sentence: "When assessment content carries attitudinal valence…" |
| 23 | editor | **Possessive on date: "Messick (1995)'s terms."** | L152 | Change to "Messick's (1995) terms." |
| 24 | editor | **Limitations section is a single ~300-word paragraph.** Seven distinct limitations in running prose; impossible to scan. | §5.6, L534 | Convert to numbered list or one paragraph per limitation (matching the lettered list in §5.7). |
| 25 | editor | **Colloquial register in conclusion.** "The HPT instrument survived every fairness test" (L486) and "ideology had nothing to do with the score" (L544) clash with the formal register. | §5.1, §5.8 | Revise: "passed every fairness test" / "political ideology was unrelated to scores across all model specifications." |
| 26 | editor | **TOP table refers to "Sections 3.1–3.6" but Method has only 3.1–3.4.** | L739, TOP table | Correct to "Sections 3.1–3.4." |
| 27 | layperson | **"Disciplinary reasoning" never defined.** Used repeatedly (L138, 205, 428, 486, 544) without explaining that "disciplinary" means reasoning according to the norms of professional historians. | L138 | Add gloss on first use: "disciplinary reasoning (reasoning according to the methodological norms of the historical discipline, such as source criticism and contextualization)." |
| 28 | layperson | **Multiple psychometric terms undefined on first use.** "Tau-equivalence," "congeneric," "design effects (DEFF)," "all-others-as-anchor," "bifactor model," "disattenuated correlation," "threshold-based invariance testing." | §3.4, §4.1, §4.3 | Add brief inline glosses for each on first use. |
| 29 | stylist | **Abstract lacks rhythmic variation.** Three consecutive sentences >45 words with no short punch. | Abstract, L109 | Split into 4 units: purpose+sample (≤35 words), findings stated flatly (~20 words), substantive finding (~20 words), implication (≤30 words). |
| 30 | stylist | **§2.1 paragraph 1: five consecutive 40–80 word sentences.** Multiple nested subordinate clauses; three levels of nesting in one sentence. | L134 | Break the 50-word parenthetical at the dash. Deliver terminological choice as a short sentence. |
| 31 | stylist | **§5.6 Limitations: 10+ sentence single paragraph.** Most glaring structural violation — each limitation buried in a block where no single point lands with force. | L534 | One paragraph per limitation (2–3 sentences each: limitation + scope implication). |

---

## Minor / Style (nice to have)

| # | Reviewer | Issue | Location | Suggestion |
|---|----------|-------|----------|------------|
| 32 | methodologist | Three-factor model: largest MI (7.35) not identified by item pair. | §4.1 | Report which item pair generates MI = 7.35. |
| 33 | methodologist | Pearson r between HPT composites and KN (discrete 0–6 sum) without distributional check. | §3.4 | Report KN distribution, floor/ceiling proportions. Consider polychoric. |
| 34 | methodologist | Middle-tertile exclusion (n ≈ 101): descriptives never reported. Fairness conclusions apply only to extreme groups. | §3.4 | Report middle-tertile descriptives; note fairness conclusions apply to low vs. high only. |
| 35 | methodologist | KN item properties not reported (facility, point-biserial). | §3.3.4 | Report basic item statistics in supplementary table. |
| 36 | psychometrician | No DIFFTEST Δχ² reported for comparing 3-factor vs. 2-factor vs. 1-factor models. | §4.1 | Either report DIFFTEST or state it was not performed and why. |
| 37 | psychometrician | df = 24 derived from "45 observed statistics" but ordinal WLSMV uses polychoric correlations + thresholds, not just covariance elements. | §4.1, L256 | Provide explicit parameter count under WLSMV. |
| 38 | psychometrician | .85 discriminant validity threshold uncited. | §4.1, L276 | Cite source (e.g., Kline, 2023) or replace with AVE-based criterion. |
| 39 | psychometrician | CFI improvement from metric to scalar (ΔCFI = +.002) is atypical; may be WLSMV scaling artefact. | §4.7, L394 | Flag as potential WLSMV small-sample behaviour. Cite Sass et al. (2014). |
| 40 | domain-expert | ROA description slightly understates the construct — should be "recognition of the agent's social role and situational context." | §1, L118 | Expand per Hartmann & Hasselhorn (2008, p. 265). |
| 41 | domain-expert | PCA ≠ CFA: present study is the first confirmatory test of the three-level structure. This strengthens the contribution — worth stating explicitly. | §2.1, L136 | Add clause noting the CFA provides stronger structural evidence than the original PCA. |
| 42 | domain-expert | Weimar Republic / Munich Agreement telescoping: 1933 → 1938 is a 5-year gap. The framing implies direct Czech memory transfer without hedging. | §2.2, L143 | Add qualifier: "while subsequent events may shape contemporary Czech memory of the interwar period, the scenario centres on Weimar-era deliberation (early 1930s)." |
| 43 | domain-expert | Alvén (2019) uses Swedish narrative assessment context, not scenario-based Likert. | §1, L124 | Add "in open-ended narrative assessment" after the citation. |
| 44 | editor | "IRT-based differential item functioning (DIF) analysis" — "Item Response Theory (IRT)-based" with hyphenated compound is awkward. | Abstract, L109 | Introduce IRT in a prior clause; use "IRT-based DIF analysis" subsequently. |
| 45 | editor | "since" ambiguous (temporal vs. causal) at L276. | §4.1 | Change "since" to "because." |
| 46 | editor | "near-unity" vs. "near-perfect" used inconsistently. | §4.1 | Use one term consistently. |
| 47 | editor | "polarization" is American spelling; rest of manuscript uses British. | §5.4, L503 | Change to "polarisation." |
| 48 | editor | Redundant scalar invariance definition (given three times: §2.4, §4.7 inline, §4.7 final sentence). | §4.7, L416 | Delete the third instance (L416). |
| 49 | editor | ICCs interpretation "a desirable property" is vague evaluative gloss. | §4.5, L362 | Reframe: "consistent with between-school variation in history instruction." |
| 50 | editor | "A caveat is warranted" (L426) is filler. | §4.9 | Start with the caveat itself: "A limitation of this evidence is that…" |
| 51 | layperson | ISCED 2 / ISCED 3 unexplained. | L195 | Spell out and gloss age ranges. |
| 52 | layperson | "Kurzskala Autoritarismus" not translated (unlike FR-LF which is). | L215 | Add "(Short Scale of Authoritarianism)." |
| 53 | layperson | CFA fit index acronyms (TLI, RMSEA, SRMR) not spelled out. | L232 | Spell out on first use. |
| 54 | layperson | WLSMV first used in body text before full name appears (only in Table 1 note). | L159 | Spell out on first body-text use. |
| 55 | layperson | "just-identified" (SEM term) unexplained. | L274 | Gloss: "each subscale has exactly as many free parameters as observed statistics, leaving no room for misfit." |
| 56 | layperson | lavaan syntax `=~` appears in body text (L412) without explanation. | §4.7 | Replace with plain language: "a suggested cross-loading of POP2 on the ROA factor." |
| 57 | stylist | L116: buried lede — the fairness question appears at word 44 after three framing clauses. | Intro, para 1 | Open directly with the fairness question. |
| 58 | stylist | L134: "substantive concept deployment" — heavy nominalisation where a verb would clarify. | §2.1 | Rewrite: "students deploy substantive concepts — domain-specific knowledge of events, causes, and actors." |
| 59 | stylist | L238: passive chain in "Clustered data handling" block. | §3.4 | Rewrite in active voice. |
| 60 | stylist | L362: missed short punch in ICCs interpretation. | §4.5 | Separate the two conclusions into distinct sentences. |
| 61 | stylist | Results section headings are labels, not claims (4.2 "Standardized Factor Loadings" etc.). | §4.2–4.5 | Consider claim-carrying headings (e.g., "4.2 Loadings Are Strongest for CONT Items"). |
| 62 | stylist | §5.1: strong short punches ("survived every fairness test"; "ideology predicted nothing") immediately diluted by a long listing sentence. | §5.1, L486 | Move the list sentence before the punches so the punches close the paragraph. |

---

## Strengths Noted

- **methodologist:** Transparent acknowledgment of
  limitations within Results sections (not buried in Discussion).
  Complementary fairness evidence strategy (DIF + MG-CFA).
  Preregistration with transparent deviation documentation.
- **domain-expert:** Rigorous theoretical positioning of HPT
  as knowledge-dependent competence. Transparent construct
  boundary acknowledgment. Accurate representation of the
  Lee & Ashby / Barton & Levstik debate.
- **psychometrician:** Transparent model limitation
  acknowledgment. Complementary fairness evidence strategy.
  Appropriate estimator choice (WLSMV) and design-effect
  evaluation.
- **sceptic:** Genuine pre-emptive self-critique of statistical
  artefacts. Dual-method fairness evaluation. Appropriately
  bounded TOST procedure.
- **editor:** Logical signposting and forward cross-references.
  Honest qualification of positive findings at point of
  relevance. Self-contained, informative table notes.
- **layperson:** Section 2.4 invariance definitions are clear
  and layered. Table 5 DIF note is unusually reader-friendly.
  The "congruence pathway" illustration (L152) makes CIV
  vivid and accessible.
- **stylist:** §5.1 opening two sentences are near-perfect
  short punches. §4.6 DIF table note demonstrates the
  "Not X but Y" move cleanly. §2.3 congruence pathway
  illustration is excellent concrete grounding.

---

## Reference Check

### Summary

| Category | Count |
|----------|-------|
| Fully verified (KB match + content aligned) | 13 |
| In KB — stub notes only | 9 |
| In KB — bibliography errors | 5 issues across 4 refs |
| Not in KB | 15 |
| Content misattribution | 0 confirmed |

### Priority Actions

1. **Confirm KSA-3 working paper number** — manuscript says
   "2014/35"; R/ note says "2014/32". Check GESIS website.
2. **Verify Drie & Boxtel year** — manuscript says 2007;
   CrossRef returns 2008. If 2008, all inline citations
   need updating.
3. **Smith et al. year** — manuscript says 2018; CrossRef
   returns 2019. Clarify online-first vs. issue-date
   convention.
4. **Check Alvén (2019)** — confirm article addresses
   ideological bias specifically (not just general scoring
   bias) before retaining the specific characterisation.
5. **Confirm Decker et al. (2013)** is the right source for
   the FR-LF instrument description (it is a book, not a
   measurement manual).

### Missing from Knowledge Base (15 references)

Chen (2007), Cheung & Rensvold (2002), Duckitt (2001),
Evans & Stanovich (2013), Finch (2005), Flora & Curran
(2004), Hambleton et al. (2005), ITC (2017), Jost et al.
(2003), Kahan et al. (2017), Maas & Hox (2005), Putnick &
Bornstein (2016), Shemilt (1984), Cohen (1988), Barton &
Levstik (2004).

Most are standard methodology/psychometrics references.
Highest priority for R/ notes: Chen (2007), Flora & Curran
(2004), Finch (2005), Hambleton et al. (2005) — given
their centrality to the analytic claims.
