## 1) Judgment

The manuscript is substantially improved (relative to a generic “null result” paper) because it (a) explicitly locates the threat in **construct-irrelevant variance (CIV)** and (b) triangulates across **multilevel modeling, DIF, and MG-CFA**.

However, the core inferential vulnerability remains: **your design can support “no detectable ideological score inflation under observed range/conditions,” but it does not fully warrant “strong evidence of non-contamination” without a power/sensitivity argument tailored to (i) restricted range, (ii) ordinal measurement, and (iii) group-splitting decisions.**

---

## 2) Rationale: Major weak spots a reviewer will press

### A. “Strong evidence” language is not yet earned (sensitivity to small effects)

You repeatedly use “strong evidence” / “supports construct validity” language. A reviewer will ask: **what effect sizes are excluded?** In the Results, you report _p_’s and near-zero _r_’s, but you do not yet provide a **minimum detectable effect** (MDE) or equivalence/ROPE-style inference.

**Concrete vulnerability:** With N≈285 and nested clustering (10 schools, 20 classes), your CI widths likely still allow nontrivial small effects (e.g., β≈.08–.12), particularly for CONT and DIF. Without pre-specified SESOI (smallest effect size of interest) + equivalence tests, “no effect” can read as “underpowered.”

**Fix:** Add (1) SESOI justification; (2) equivalence test; (3) simulation-based MDE for multilevel and DIF.

---

### B. Range restriction and floor effects: you acknowledge them but do not quantify the inferential damage

You note positively skewed ideology with floor effects. That concession is good, but reviewers will demand **diagnostics**:

- Proportion at lowest category per item (RD/NS; KSA facets)
- Effective variance after controlling SDR
- Whether “High ideology tertile” is substantively high or merely “less low”

**Why it matters:** If “High” is still near mainstream, you are not stress-testing contamination; you are stress-testing robustness under _moderate_ ideological heterogeneity. Your Discussion needs to explicitly treat this as a **boundary condition** rather than as broad construct validity.

---

### C. Ideology operationalization mixes constructs and introduces interpretive ambiguity

You have two ideology measures: FR-LF mini (RD+NS) and KSA-3 (authoritarianism facets). But your inferential target is “right-authoritarian attitudes inflating HPT,” while your predictors partially represent:

- regime preference / strong-leader endorsement (RD)
- Nazi crimes relativization (NS)
- authoritarian aggression/submission/conventionalism (KSA)

A reviewer may argue you are testing **multiple causal stories** simultaneously:

- _Content congruence_ (NS; scenario about extremist movement)
- _Cognitive style_ (KSA—need for closure; rigidity)
- _Political authoritarian preference_ (RD)

Yet the manuscript’s theoretical claim emphasizes scenario congruence. If you want to test congruence, NS should be privileged and the congruence mechanism must be operationalized more directly.

**Specific vulnerability:** Your “IDEO_Z composite” (z-scored FR-LF + KSA) used for group-splitting in DIF/MG-CFA arguably conflates **content-congruent attitudes** with **general authoritarian disposition**, diluting the mechanism you claim to be testing.

**Fix:** Run invariance/DIF using (a) NS-only grouping, (b) RD-only grouping, (c) KSA-only grouping; show whether results are invariant to the grouping definition.

---

### D. Tertile split + excluding middle tertile reduces power and can distort DIF/MG-CFA conclusions

You exclude the middle tertile “to maximize contrast.” Reviewers will flag:

1. **Loss of N** harms DIF power.
2. Tertile thresholds are **sample-dependent**, undermining replicability.
3. Dichotomizing a continuous predictor is psychometrically disfavored unless justified.

**Fix:** Prefer continuous multiple-indicators multiple-causes (MIMIC) models for DIF, or alignment optimization, or at minimum replicate with median split and continuous moderation.

---

### E. Measurement model for HPT: excellent fit indices may be artifactual with small item set

Your CFA reports near-perfect fit (CFI≈.999, RMSEA≈.011). With 9 items and WLSMV, extremely good fit can occur even when the substantive measurement story is fragile.

A reviewer might ask:

- Are thresholds sparse (4 categories) causing quasi-saturated models?
- Did you check residual correlations / local dependence?
- Did you test alternative cross-loadings (e.g., POP reverse-coded method factor)?
- Did you test invariance at the **item threshold level** appropriately for ordinal indicators (you claim scalar with thresholds; good, but details are thin)?

**Fix:** Report full model specification and local fit: modification indices, residual correlations, item characteristic curves (if using IRT), and method-factor robustness (reverse-coded POP).

---

### F. Reliability interpretation: the “knowledge predicts therefore reliability is fine” argument is vulnerable

You argue modest α is acceptable because KN predicts HPT. A skeptical reviewer will respond: **predictive validity does not rescue low reliability**; it can coexist with attenuation and still leave you unable to detect small ideology effects.

**Fix:** Address attenuation explicitly:

- Correct for attenuation sensitivity check (even if only approximate).
- Use latent-variable regression (SEM) to reduce measurement error in HPT and ideology.

---

### G. Multilevel modeling: random effects structure and within/between decomposition are underdeveloped

You include random intercepts for school and class. But a reviewer could argue:

- Ideology may operate at **class climate** level (between-class) rather than individual (within-class).
- Your model should decompose ideology into within-class centered and class mean (Mundlak / contextual effects).
- Random slopes: you mention them in sensitivity, but don’t show variance estimates or diagnostics.

**Fix:** Add:

- ICCs for ideology measures (not just HPT).
- Contextual effect models: within-class ideology vs class mean ideology predicting HPT.
- Cross-level interaction: class mean ideology × individual knowledge.

---

### H. Alternative mechanism: “authoritarianism reduces HPT” appears in POP_rev model but is treated cautiously; reviewers may demand a principled treatment

You find a small negative KSA effect for POP_rev (more authoritarian → more presentist). This is potentially theoretically meaningful, but you frame it as post hoc. A reviewer may say: you can’t both (a) claim null ideology effects and (b) highlight this as supportive discriminant validity without a preregistered plan.

**Fix:** Move it to an explicitly labeled exploratory section; adjust claims accordingly; or pre-register follow-up analyses for Stage-2 revision (if possible within RR constraints).

---

### I. Literature gaps likely to be flagged

Your literature coverage is competent for history education and validity frameworks. But for the **ideological cognition** premise you lean on (motivated cognition; attitude-consistent responding), the grounding is thin and partly relies on single exemplars (e.g., “Badham et al., 2025” appears potentially peripheral to ideological congruence mechanisms).

Gaps likely to be raised:

- Identity-protective cognition / cultural cognition (as mechanism for motivated responding)
- Integrative complexity and authoritarian cognition (if you want to explain the negative KSA effect)
- Moral conviction / sacred values and their effect on judgment
- Measurement literature on socially sensitive attitudes in adolescents (especially floor effects and misreporting)

Even if you do not expand the political psychology section substantially, you should add **one paragraph** clarifying why the hypothesized direction (higher ideology → higher HPT) is theoretically plausible **given competing predictions** (rigidity → lower HPT).

---

## 3) Alternative interpretations / design choices with trade-offs

1. **True null (no contamination within observed range)**
   _Trade-off:_ strongest claim is limited to mainstream ideological distributions; generalizability to more extreme contexts remains open.

2. **Small effect masked by measurement error and range restriction**
   _Trade-off:_ requires you to weaken claims or add equivalence/latent models to bound the effect.

3. **Ideology operates at classroom climate level, not individual level**
   _Trade-off:_ requires contextual-effect modeling; may alter interpretation from individual CIV to social-norm constraints.

4. **Opposing pathways cancel: congruence inflates, rigidity deflates**
   _Trade-off:_ implies net zero but meaningful mechanisms; suggests modeling interactions (ideology facets × cognitive style proxies).

---

## 4) Confidence level and what would increase certainty

**Confidence (methodological): 0.62.**

What would most increase certainty:

1. **Equivalence testing / SESOI** showing that plausible contamination effects are statistically ruled out (not merely “non-significant”).
2. **Contextual-effects multilevel models** (within vs between class) to rule out climate-driven contamination.
3. **Robust DIF strategy** using continuous moderators (MIMIC / moderated IRT) rather than tertile splits.

---

## Opportunity Radar (highest leverage fixes)

1. **Add an “Effect-size bounding” subsection**: SESOI + equivalence tests + MDE simulations for multilevel and DIF.
2. **Re-run invariance/DIF with NS-only grouping and continuous moderators**: directly targets the “scenario congruence” mechanism.
3. **Add within/between ideology decomposition** in multilevel models: tests classroom climate pathway with minimal manuscript expansion.

---

## Verbalised sampling (plausible readings; weights sum to 1.0)

1. **True non-contamination within mainstream Czech classrooms** (0.40)
2. **Non-detection due to range restriction + attenuation** (0.25)
3. **Ideology matters indirectly (class norms suppress endorsement) → net null** (0.15)
4. **Cancellation of opposing mechanisms (congruence vs rigidity)** (0.12)
5. **Specification artifacts (group split + measurement model) inflate “no DIF/invariance” confidence** (0.08)

Uncertainty note: weights depend heavily on unreported diagnostics (ideology item-level floors; random-slope variances; DIF power).
