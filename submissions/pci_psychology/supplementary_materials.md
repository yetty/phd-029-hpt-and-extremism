# Supplementary Materials

**Czech Adaptation and Initial Validation of a Historical Perspective
Taking Instrument: Ideological Fairness Evidence**

Juda Kaleta

Institute of History, Faculty of Arts, Charles University,
Czech Republic

---

## Table S1: GRM Item Parameters

Graded Response Model (GRM) item discrimination ($a_1$) and
category-intercept ($d_1$, $d_2$, $d_3$) parameters for the nine HPT
items. Parameters were estimated using the `mirt` R package
with a constrained multi-group model (High- vs.
Low-ideology groups defined by tertile split on the composite
FR-LF + KSA score). An all-others-as-anchor DIF testing
strategy with Bonferroni-corrected significance level
($\alpha$ = .01) detected no significant DIF for any item.
Because no items were flagged, the constrained model---in
which item parameters are held equal across groups---is the
final model. The parameters below therefore apply to both
ideology groups.

| Item  | Subscale          | $a_1$  | $d_1$  | $d_2$  | $d_3$  |
|-------|-------------------|--------|--------|--------|--------|
| CONT1 | Contextualization |  1.864 |  2.812 |  0.718 | -1.964 |
| CONT2 | Contextualization |  1.382 |  2.082 |  0.728 | -1.273 |
| CONT3 | Contextualization |  1.364 |  2.763 |  0.768 | -1.324 |
| POP1  | Present-oriented perspective | -1.016 |  0.207 | -1.345 | -2.674 |
| POP2  | Present-oriented perspective | -0.355 |  0.903 | -0.843 | -2.217 |
| POP3  | Present-oriented perspective | -0.630 |  0.683 | -1.068 | -2.727 |
| ROA1  | Role of historical agent |  1.193 |  2.633 |  1.175 | -0.804 |
| ROA2  | Role of historical agent |  0.719 |  2.159 |  0.481 | -1.777 |
| ROA3  | Role of historical agent |  1.430 |  2.706 |  1.313 | -0.953 |

*Note.* $a_1$ = item discrimination; $d_1$, $d_2$, $d_3$ =
category intercepts in `mirt`'s default parameterization. Raw POP
responses entered the GRM and were not reversed for this analysis;
their negative discrimination estimates therefore reflect the
opposite direction of the present-oriented items relative to CONT and
ROA. POP was reversed only when computing composite scores. The
constrained model was retained because Bonferroni-corrected
likelihood-ratio DIF tests showed no significant difference
between free and constrained parameters for any item (all
$p$ > .01).

## Table S2: HPT Standardized Factor Loadings

| Item | Subscale | Loading | SE | 95% CI |
|---|---|---:|---:|---:|
| POP1 | Present-oriented perspective | .74 | .099 | [.54, .93] |
| POP2 | Present-oriented perspective | .30 | .076 | [.15, .44] |
| POP3 | Present-oriented perspective | .50 | .068 | [.37, .63] |
| ROA1 | Role of historical agent | .61 | .071 | [.47, .75] |
| ROA2 | Role of historical agent | .30 | .074 | [.15, .44] |
| ROA3 | Role of historical agent | .67 | .074 | [.53, .82] |
| CONT1 | Contextualization | .72 | .055 | [.61, .82] |
| CONT2 | Contextualization | .61 | .059 | [.49, .72] |
| CONT3 | Contextualization | .64 | .054 | [.53, .74] |

*Note.* POP items are reverse-scored. Estimates are from the
three-factor WLSMV model. All loadings were significant at
$p$ < .001.

## Table S3: Reliability Estimates

| Scale | Items | Complete $n$ | Raw $\alpha$ | 95% CI | Polychoric $\alpha$ | $\omega_t$ |
|---|---:|---:|---:|---:|---:|---:|
| HPT: reversed present-oriented perspective | 3 | 282 | .46 | [.35, .57] | .52 | .54 |
| HPT: role of the historical agent | 3 | 281 | .50 | [.40, .60] | .53 | .55 |
| HPT: contextualization | 3 | 287 | .64 | [.57, .71] | .69 | .70 |
| HPT composite | 6 | 282 | .60 | [.53, .68] | .65 | .66 |
| HPT total | 9 | 276 | .66 | [.60, .72] | .69 | .70 |
| Historical knowledge | 6 | 286 | .55 | [.46, .63] | - | - |
| FR-LF: dictatorship acceptance | 3 | 279 | .51 | [.42, .61] | .56 | .61 |
| FR-LF: Nazi-crime relativization | 3 | 280 | .59 | [.51, .68] | .63 | .64 |
| FR-LF total | 6 | 275 | .67 | [.61, .73] | .70 | .71 |
| KSA-3: aggression | 3 | 279 | .61 | [.53, .69] | .64 | .66 |
| KSA-3: submission | 3 | 277 | .39 | [.26, .51] | .42 | .44 |
| KSA-3: conventionalism | 3 | 276 | .56 | [.47, .65] | .60 | .61 |
| KSA-3 total | 9 | 267 | .72 | [.67, .77] | .75 | .75 |
| Social desirability | 5 | 279 | .30 | [.17, .43] | .34 | .55 |
| Combined ideology composite | 2 scales | 283 | .80 | [.74, .84] | - | - |

*Note.* The historical-knowledge coefficient is KR-20, equivalent
to raw alpha for dichotomous items. Alpha confidence intervals use
the standard-error approximation reported by `psych::alpha`. The
combined ideology coefficient is Mosier reliability for the mean of
standardized FR-LF and KSA-3 scores; its interval is a percentile
bootstrap interval with 2,000 resamples. The other omega estimates
are total omega from a one-factor model of the polychoric matrix.
For the multidimensional HPT, FR-LF, and KSA composites, these omega
values are descriptive one-factor estimates rather than reliability
derived from the correlated-factor measurement model.

### Table S3b: Pairwise Sample Sizes for Table 7

| | HPT | Context | Pres. (rev.) | Knowledge | FR-LF | KSA-3 | SDR-5 |
|---|---:|---:|---:|---:|---:|---:|---:|
| HPT | - | 287 | 287 | 287 | 283 | 282 | 282 |
| Context | 287 | - | 287 | 287 | 283 | 282 | 282 |
| Pres. (rev.) | 287 | 287 | - | 287 | 283 | 282 | 282 |
| Knowledge | 287 | 287 | 287 | - | 284 | 283 | 283 |
| FR-LF | 283 | 283 | 283 | 284 | - | 283 | 283 |
| KSA-3 | 282 | 282 | 282 | 283 | 283 | - | 283 |
| SDR-5 | 282 | 282 | 282 | 283 | 283 | 283 | - |

## Table S4: Instrument Source Populations and Language

| Instrument | Source population | Source language | Relevance to this sample |
|---|---|---|---|
| HPT | 170 German Grade 10 students; Dutch extensions included ages 10-17 | German; published validation in English; later Dutch translation | Direct secondary-student evidence exists, but only in German and Dutch contexts |
| FR-LF mini | German population surveys sampled respondents aged 14 years and older; samples were predominantly adult | German | Includes adolescents but provides no distinct adolescent or Czech validation |
| KSA-3 | German-speaking general population aged 18 years and older; development samples $n$ = 228 and 223, mean ages 51.4 and 49.0 | German | No adolescent or Czech validation in the source report |
| SDR-5 | US medical outpatients ($n$ = 614, mean age 37; $n$ = 3,053, mean age 47) and 75 older adults | English | No adolescent or Czech validation in the source report |
| Historical knowledge | Six study-specific items for the Weimar scenario | Czech | Scenario-proximal criterion rather than a general knowledge scale |

The non-HPT scales were administered in Czech. Beyond expert review
of the battery, separate cognitive-interview or psychometric
validation studies of these Czech adolescent versions were not
available. Their use should therefore be interpreted through the
sample-specific reliability estimates in Table S3 rather than assumed
population portability.

### Table S4b: Analysis-Specific Sample Sizes

| Analysis or score | Included $n$ |
|---|---:|
| Recruited sample | 293 |
| Three-factor CFA and HPT descriptives | 287 |
| Historical-knowledge descriptives | 293 |
| FR-LF descriptives | 284 |
| KSA-3 descriptives | 283 |
| SDR-5 descriptives | 283 |
| Reliability | 267-287, by complete item set (Table S3) |
| Pairwise correlations | 282-287 (Table S3b) |
| Composite-ideology DIF/MG-CFA groups | 96 low, 96 high; 101 middle excluded |
| NS-only MG-CFA groups | 126 low, 125 high; 42 middle excluded |

The differences arise from item nonresponse, the scale-specific
minimum-answer rules described in the manuscript, listwise-complete
item sets for reliability, pairwise-complete correlations, and the
post-registration extreme-group exclusions used for DIF and MG-CFA.

Knowledge-item facilities were .57 (KN1), .44 (KN2), .73 (KN3), .44
(KN4), .41 (KN5), and .47 (KN6). The total score had $M$ = 3.04,
$SD$ = 1.62, median = 3, IQR [2, 4], with 4.1% at zero and 8.9% at six.

## Table S5: Preregistration Mapping and Deviations

The immutable OSF registration `zsngy` was registered on 27 November
2025. The table records how the present report differs from that plan.

| Registered element | Status in this report |
|---|---|
| H1: ideology positively predicts CONT with knowledge and SDR-5 covariates | Relevant associations are reported, but the exact registered one-tailed, covariate-adjusted test is not presented as a confirmatory test |
| H2: ideology negatively predicts raw POP | Relevant associations are reported, but the exact registered one-tailed model is not presented as a confirmatory test |
| H3: ideology positively predicts total HPT with covariates | Related models are reported, but HPT_CTX6 was adopted post-registration as the primary outcome |
| H4: positive DIF on CONT items | The registered primary continuous-ideology MIMIC analysis was not implemented; post-registration all-item GRM DIF and MG-CFA analyses are reported instead |
| H5: stronger ideology-HPT associations at lower knowledge | KSA-by-knowledge moderation and registered simple slopes were not implemented |
| H6: exploratory positive correlations and FR-LF-CONT > KSA-CONT | Correlations are reported; class-clustered errors and the registered Williams/Steiger comparison were not implemented |
| Two-level class random-intercept models | Three-level and school-level specifications were added post-registration |
| Grade level, gender, history grade, knowledge, and SDR-5 covariates | The complete registered covariate set was not used in all reported models |
| One-tailed tests and Benjamini-Hochberg correction | Reported tests are two-tailed and do not implement the registered correction family |
| Registered missing-data rule | The registered listwise-deletion/MICE decision rule was not implemented; the score-specific rules used for revision reporting are documented in Section 3.4 |
| HPT response scale 0-3; two ROA items; approximately ten knowledge items | Administered battery used 1-4 HPT responses, three ROA items, and six knowledge items |
| CFA-focused validation, bifactor model, NS-only invariance, TOST, and alternative exclusions | Added after registration and treated as exploratory or descriptive |

## Supplementary Method S1: Response-Process Follow-up Design

A follow-up study should purposively sample students from low, middle,
and high HPT-score ranges and include variation in ideology and school
level. Students would complete the scenario while thinking aloud and
then participate in retrospective item-by-item probing. Coding would
distinguish comprehension of the scenario, use of supplied historical
evidence, mobilization of prior knowledge, present-day moral judgment,
attitude-congruent endorsement, and reconstruction of the actor's
historical situation. Independent coders would classify whether each
verbalized process matches the intended POP, ROA, or CONT level and
identify response options reached through unintended pathways. This
would test the response-process interpretation directly rather than
inferring it from internal structure alone.
