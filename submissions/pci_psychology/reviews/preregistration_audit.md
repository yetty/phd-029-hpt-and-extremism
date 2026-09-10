# OSF preregistration audit

**Audit date:** 2026-09-10
**Project:** <https://osf.io/yng37/>
**Immutable registration:** `zsngy`, registered 2025-11-27

## Overall finding

The current manuscript does not faithfully map its inferential claims to the
registered hypotheses. The registration contains six hypotheses, whereas the
manuscript replaces them with two research questions and presents several
post-registration measurement analyses as if they were the original
confirmatory focus. Most seriously, registered H4 predicts positive
differential item functioning (DIF) on CONT items, while the manuscript states
that measurement invariance was hypothesised.

The revision must not describe the current two-question framework as the
original preregistered hypothesis structure. It should report the registered
H1-H6 family transparently, distinguish implemented modifications from
omissions, and label post-registration analyses explicitly.

## Registered hypotheses

The registration's question `q2` specifies:

1. **H1:** FR-LF-mini and KSA-3 positively predict CONT after adjustment for
   historical knowledge and social desirability.
2. **H2:** FR-LF-mini and KSA-3 negatively predict raw POP, where higher POP
   indicates greater presentism.
3. **H3:** FR-LF-mini and KSA-3 positively predict total HPT after adjustment
   for historical knowledge and social desirability.
4. **H4:** CONT items show positive DIF by ideological attitudes at equal
   latent HPT ability.
5. **H5:** Ideology-HPT associations are stronger at lower historical
   knowledge. Question `q17` specifies FR-LF-by-knowledge and
   KSA-by-knowledge interactions with simple slopes.
6. **H6:** Exploratorily, FR-LF and KSA correlate positively with each other
   and with CONT, with the FR-LF-CONT association exceeding the KSA-CONT
   association.

## Registered analysis commitments

Question `q17` specifies:

- two-level models with students nested in classes and a random class
  intercept;
- demographic covariates comprising grade level, gender, and last history
  grade, in addition to knowledge and SDR-5;
- one-tailed directional tests for H1, H2, H3, and H5;
- Benjamini-Hochberg correction across confirmatory tests;
- a primary WLSMV MIMIC DIF model for CONT and continuous ideology;
- tertile-based multi-group CFA as a sensitivity analysis;
- both ideology-by-knowledge interactions and simple slopes at -1, 0, and +1
  SD;
- class-clustered correlations and a Williams/Steiger comparison for H6;
- listwise deletion at no more than 5% missingness and 20-imputation MICE
  otherwise;
- estimates, standard errors, confidence intervals, p-values, ICCs, partial
  R-squared values, covariate-only comparisons, and model diagnostics.

## Material deviations and omissions

### Changed inferential hierarchy

- The manuscript's RQ1 psychometric-validation frame was not a registered
  primary question.
- The manuscript's invariance prediction reverses registered H4's positive-DIF
  prediction.
- Registered H1-H6 are not individually reported under their registered
  labels.
- `HPT_CTX6` is promoted to the primary outcome although the registration did
  not define that hierarchy. The registered total was the sum or mean of all
  valid HPT items.

### Missing or incomplete registered analyses

- The primary MIMIC model is absent; all-item IRT DIF replaces it.
- KSA-by-knowledge moderation and the registered simple slopes are absent.
- The Williams/Steiger comparison and class-clustered correlation standard
  errors are absent.
- Demographic covariates are absent from the primary models.
- Tests are two-tailed rather than the registered one-tailed tests.
- Benjamini-Hochberg correction is absent.
- The registered missing-data decision rule is not reported as implemented.
- Partial R-squared values and covariate-only model comparisons are not fully
  reported.

### Post-registration analyses requiring explicit labels

- the six-item `HPT_CTX6` primary composite;
- all-item graded-response DIF;
- psychometric validation as the principal research question;
- bifactor analysis;
- NS-only invariance analysis;
- TOST equivalence tests and their SESOI;
- Mundlak within/between decomposition;
- alternative exclusion rules and design-effect calculations;
- school-level random effects.

### Instrument and sample discrepancies

- The registration says HPT responses use a 0-3 scale; the instrument and
  analyses use 1-4. This is a location shift but still requires disclosure.
- The registration describes ROA as two items; the administered instrument has
  three.
- The registration describes an approximately ten-item knowledge test scored
  0-10; the administered analysis uses six items.
- The registration gives inconsistent class targets: 12-16 classes in `q11`
  and 8-12 classes in `q12`. The realised sample exceeds the registered minimum
  of 250 usable students and six classes.
- The registered structure is students within classes. Three-level
  school-and-class models are post-registration additions.

## Scenario and adaptation record

The registration describes the instrument only as a Czech translation of the
Hartmann and Hasselhorn scenario measure. It does not identify the German
dissertation as the translation source and does not document a change from
voting to joining. Direct source comparison confirms that German, English,
administered Czech, and OSF instrument versions all concern voting. The
manuscript's joining language is therefore a paraphrase error rather than a
registered adaptation.

## Required revision decision

Before substantive manuscript integration, choose between:

1. running and reporting the omitted registered analyses, restoring H1-H6 as
   the confirmatory spine, and retaining the current measurement work as
   explicitly post-registration or exploratory evidence; or
2. retaining the current analysis set but providing a complete deviation table
   and sharply limiting all confirmatory language.

In either case, the TOP statements that only H1-H2 were preregistered, that all
registered analyses are reported, and that TOST is the only deviation must be
corrected.
