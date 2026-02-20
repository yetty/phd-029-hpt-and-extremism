Below is **Version 3** of the critical review. This version assumes the manuscript is close to submission and evaluates it as a skeptical TRSE reviewer would: focusing on inferential overreach, latent threats to validity, and where your argument is strongest vs. overstated.

All comments refer to the current Stage-2 manuscript .

---

# 1. Overall Judgment

The manuscript is now methodologically serious and far stronger than earlier versions. The triangulation (multilevel + DIF + MG-CFA + sensitivity models + CI bounding) is unusually rigorous for history education research .

However, three structural vulnerabilities remain:

1. **Equivalence inference is not fully formalized** (you imply practical equivalence without conducting formal equivalence tests).
2. **Range restriction is more severe than the discussion admits**, limiting the strength of the contamination test.
3. **The ideology mechanism is theoretically underspecified relative to the empirical operationalization.**

None of these are fatal, but they are exactly the kinds of issues a high-tier reviewer will probe.

---

# 2. Core Inferential Risk: You Are Testing Absence of Moderate Effects, Not Absence of Contamination

### What you claim

You state that the 95% CIs exclude effects of practical educational significance (|β| > .14) and interpret this as strong evidence against contamination.

### Methodological problem

You did **not preregister a SESOI**, and you did not run formal equivalence tests (TOST). You are performing post hoc CI bounding.

This creates two issues:

1. SESOI selection (β = .20) is justified via Cohen (1988), but that is generic psychology convention—not field-specific contamination logic.
2. CI-based exclusion is valid only relative to a clearly specified smallest effect of concern _before_ looking at data.

A reviewer could argue:

> You show that moderate effects are unlikely. You do not show that meaningful contamination is absent.

### What would strengthen this:

- Explicitly state SESOI in the Methods (even if post hoc, transparently).
- Conduct formal TOST equivalence tests.
- Report achieved power for β = .15 and β = .10.

Without this, “strong evidence” is slightly overstated.

---

# 3. Range Restriction: The Study Is a Test of Mainstream Attitudes, Not Ideological Extremity

You are commendably transparent about floor effects .

However, the implications are larger than you acknowledge.

Key numbers:

- FR-LF mean = 2.48 (below midpoint)
- Only 17.7% above midpoint
- “High ideology” tertile mean = 3.20
- Only 50% of that tertile above midpoint

This means:

- Your “High” group is not extreme.
- The contamination hypothesis is tested under **moderate attitudinal heterogeneity**.

You appropriately frame this as boundary conditions in the Discussion , but you still use language like:

> “The instrument does not suffer from ideological contamination.”

Stronger and more defensible phrasing would be:

> “The instrument does not exhibit detectable ideological contamination within the ideological distribution typical of Czech lower-secondary classrooms.”

That wording aligns with the data and avoids generalization risk.

---

# 4. Mechanism Problem: You Operationalize Multiple Ideology Constructs but Argue One Mechanism

The contamination hypothesis rests on **content congruence** (sympathy with extremist themes).

However:

- FR-LF includes dictatorship endorsement (RD) and NS relativization.
- KSA-3 includes submission, aggression, conventionalism (cognitive style).

You then create IDEO_Z (FR-LF + KSA composite) for group splitting .

This mixes:

- Content-congruent attitudes
- General authoritarian cognition
- Possibly unrelated conservative dispositions

Theoretically, the mechanism of contamination should hinge on **NS facet** (direct scenario congruence).

You do run NS-only MG-CFA sensitivity, which is excellent , but the manuscript still frames ideology as a unified predictor.

A reviewer might argue:

> You tested broad authoritarianism, not content congruence per se.

Your argument would be sharper if you explicitly stated:

- NS is the theoretically primary contamination mechanism.
- KSA tests the rigidity pathway.
- FR-LF composite tests generalized right-authoritarian orientation.

Right now that structure is implicit, not explicit.

---

# 5. Multilevel Modeling: Within vs. Between Ideology Is Not Fully Decomposed

You report ideology ICCs (≈ .06–.10) , which is excellent.

However, you do not estimate contextual effects:

- Individual ideology (within-class centered)
- Classroom mean ideology (between-class)

This matters because contamination could operate via **classroom climate**:

- Normative endorsement of extremist reasoning
- Peer framing effects

Random intercepts absorb cluster variance but do not test contextual effects.

Given your ICCs are non-zero, a reviewer could legitimately say:

> You tested individual-level contamination but not classroom-level ideological climate effects.

Adding a Mundlak specification would significantly strengthen the causal interpretation.

---

# 6. DIF Sensitivity Is Limited

You correctly acknowledge small group sizes (~80 per group) .

However:

- 9 items
- 4-category ordinal
- Multi-group graded response model
- Bonferroni α = .01

Power to detect small DIF is modest at best.

Your phrasing sometimes reads like:

> No DIF was found.

More precise:

> No DIF of moderate magnitude was detected under the present sample size.

That distinction matters.

---

# 7. Reliability Defense Is Strong but Slightly Overextended

Your attenuation correction is sophisticated and well-argued .

However:

- You use α_FR-LF = .67.
- You use ω_HPT ≈ .65.
- Disattenuated r = −.065.

This assumes reliability estimates are unbiased and stable.

Given small subscales and ordinal data, reliability estimation error is nontrivial.

Your conclusion that “measurement error alone cannot account for null finding” is reasonable, but the tone could be softened:

> Attenuation does not plausibly mask a moderate association.

That would be more defensible than ruling out attenuation entirely.

---

# 8. Exploratory KSA Finding Is a Double-Edged Sword

The negative KSA–POP_rev association (β = −.159) is interesting.

However:

- It was not preregistered.
- It appears in only one subscale.
- It is marginal (p = .016).
- It may not survive multiplicity correction.

You handle it responsibly, but a skeptical reviewer could argue:

> You are interpreting a post hoc significant coefficient as theoretically meaningful while dismissing nonsignificant ideology effects.

I recommend:

- Label it explicitly exploratory.
- Avoid implying theoretical confirmation.
- Possibly add multiplicity note.

---

# 9. Theoretical Literature on Motivated Cognition Is Thin

You cite Jost, Duckitt, Tetlock appropriately .

However, if your argument is about **ideology contaminating disciplinary reasoning**, you might consider brief engagement with:

- Identity-protective cognition
- Moral conviction theory
- Cultural cognition literature

Currently, the literature section gives more space to HPT theory than to ideological contamination theory. Given your paper’s contribution is primarily a validity stress-test, the ideological mechanism could be theoretically sharpened.

---

# 10. Strongest Parts of the Paper

To balance criticism:

### 1. Convergent triangulation is exemplary.

Multilevel + DIF + MG-CFA + sensitivity + fixed effects + attenuation correction is unusually rigorous in this field .

### 2. Knowledge effect strengthens construct validity.

The nomological network is exactly what theory predicts.

### 3. Measurement invariance reporting is unusually thorough for history education.

### 4. Boundary framing is present (but should be slightly strengthened).

---

# 11. Verbalised Sampling: What Is Most Plausible?

Given the current data:

1. **True absence of moderate contamination in mainstream classrooms** — 0.45
2. **Small contamination effect masked by range restriction** — 0.25
3. **Opposing congruence vs rigidity mechanisms cancel** — 0.15
4. **DIF too underpowered to detect subtle bias** — 0.10
5. **Specification artifact** — 0.05

Uncertainty hinges primarily on ideological variance and DIF power.

---

# 12. Confidence Level

**Methodological confidence: 0.75**

The null result is credible for moderate effects.
The inference is slightly overstated for small effects or extreme ideological contexts.

---

# 13. Opportunity Radar (High-Leverage Improvements Before Submission)

1. **Add formal equivalence testing (TOST) with preregistered-style SESOI justification.**
   Highest inferential gain.

2. **Add within/between ideology decomposition (Mundlak model).**
   Directly addresses classroom climate contamination pathway.

3. **Sharpen mechanism framing:**
   Explicitly separate congruence (NS) vs rigidity (KSA) pathways in theoretical model.

These would elevate the paper from “methodologically solid null” to “methodologically decisive boundary test.”

---

If you would like, I can now:

- Produce a simulated “Reviewer 2 decision letter” (accept / minor / major / reject scenarios).
- Or draft revised paragraphs that tighten your claim language to be maximally defensible for TRSE.
