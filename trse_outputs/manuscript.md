# Stress-Testing Historical Perspective Taking: Ideological Attitudes and Disciplinary Reasoning in Czech Adolescents

**Juda Kaleta**

Charles University, Faculty of Arts

---

**Author Note**

Juda Kaleta, Institute of History, Faculty of Arts, Charles University, Prague, Czech Republic.

Hypotheses, instruments, and analysis plan were preregistered on the Open Science Framework prior to data collection (https://osf.io/yng37/).

**Ethical standards.** This study was conducted in accordance with the ethical requirements of Charles University, Faculty of Arts, and applicable Czech legal provisions for research involving minors (Act No. 89/2012 Coll., Civil Code; Act No. 101/2000 Coll., Personal Data Protection). Research procedures were reviewed and approved by the ethics committee of the Institute of History prior to data collection. Participation was fully voluntary; written informed consent was obtained from parents or legal guardians of all participants, and each student's assent was confirmed before administration began. Student responses were anonymized at the point of data entry, and no individual-identifying information is reported in this manuscript. The HPT scenario (involving historical political radicalization) was reviewed for age-appropriateness; class teachers were briefed in advance, and students were informed that they could discontinue at any time without consequence.

**Funding.** This research received no external funding.

**Conflict of interest.** The author declares no conflict of interest.

**Data and code availability.** The analysis scripts used in this study are available from the corresponding author upon reasonable request. De-identified data may be shared subject to the applicable data protection framework (GDPR, Regulation 2016/679/EU) and institutional data-sharing agreements; researchers wishing to access the data should contact the author directly.

Correspondence concerning this article should be addressed to Juda Kaleta, Institute of History, Faculty of Arts, Charles University, nám. Jana Palacha 2, 116 38 Prague 1, Czech Republic. Email: juda.kaleta@ff.cuni.cz

---

## Abstract

Historical perspective taking (HPT) is a core competence in history education, yet the validity of instruments designed to measure it remains underexplored. A key threat to validity is construct-irrelevant variance: the possibility that students' ideological attitudes, rather than their disciplinary reasoning, drive their HPT scores. This study establishes boundary conditions for a widely used HPT instrument (Hartmann & Hasselhorn, 2008) by examining ideological heterogeneity as a validity threat, with a sample of 293 Czech secondary students (lower- and upper-secondary; základní školy and gymnázia) across 10 schools and 20 classrooms. We administered measures of HPT, right-authoritarian ideology (FR-LF mini), authoritarianism (KSA-3), historical knowledge, and social desirability. Using multilevel models with school and classroom random intercepts, differential item functioning (DIF) analysis, and multi-group confirmatory factor analysis (MG-CFA), we tested two preregistered hypotheses: (H1) that right-authoritarian attitudes predict higher HPT scores, and (H2) that this effect persists after controlling for knowledge and social desirability. Neither hypothesis was supported. Zero-order correlations between ideology and HPT were negligible (*r* = -.05 to .01). In multilevel models, ideology was unrelated to any HPT outcome (all *p*s > .43), while historical knowledge emerged as the only consistent predictor (β = 0.21–0.34, *p* < .001). MG-CFA demonstrated full scalar invariance across ideology groups, and no items exhibited DIF. These findings held across alternative scoring composites, exclusion rules, and random-slope specifications. Within the range of ideological attitudes typical of a Czech democratic classroom, formal equivalence tests (TOST) confirmed that ideology effects fall within the region of practical equivalence (|β| < 0.20) for all three HPT outcomes (all TOST *p*s < .003), providing evidence that HPT scores are not detectably inflated by right-authoritarian attitudes in mainstream school populations.

**Keywords:** historical perspective taking, construct validity, ideological contamination, measurement invariance, differential item functioning, multilevel modeling

---

## Introduction

Historical thinking — the capacity to reason about the past using disciplinary methods rather than naive intuition — has become a central goal of history education across multiple national curricula (Seixas & Morton, 2013; van Drie & van Boxtel, 2008; Wineburg, 2001). Among the competences subsumed under historical thinking, historical perspective taking (HPT) occupies a distinctive position. Unlike sourcing or corroboration, which focus on handling evidence, HPT requires students to reconstruct the beliefs, values, and circumstances that made past actors' decisions intelligible in their own time (Lee & Ashby, 2001; Hartmann & Hasselhorn, 2008). This cognitive achievement — understanding without endorsing — is widely regarded as among the most challenging aspects of historical literacy (Wineburg, 2001; Seixas, 2017).

The construct matters not only for curriculum and pedagogy but also for assessment. If education systems are to take historical thinking seriously as a learning outcome, they need instruments capable of measuring it reliably and validly (Ercikan & Seixas, 2015; Breakstone et al., 2013). Over the past two decades, several instruments have been developed for this purpose, with the scenario-based measure introduced by Hartmann and Hasselhorn (2008) representing one of the most widely cited attempts at standardized HPT assessment. Their instrument presents students with a historically plausible scenario — a young man in the Weimar Republic deliberating whether to join an extremist movement — and elicits responses on a four-point scale across items tapping three theoretically derived levels of HPT: presentism, recognition of the historical agent's role, and contextualisation.

Yet the very feature that makes HPT assessment educationally meaningful — its reliance on content-rich historical scenarios involving morally charged decisions — also introduces a validity concern that has received insufficient attention. When a scenario involves a historical figure contemplating ideologically charged political alternatives, students' responses may be shaped not only by their capacity for disciplinary reasoning but also by their personal ideological commitments. A student with right-authoritarian attitudes might appear to "contextualize" the agent's decision not because they are reasoning historically, but because they sympathize with the ideological content of the scenario. If so, the instrument would be measuring something other than — or in addition to — the construct it claims to assess.

This concern maps directly onto the concept of construct-irrelevant variance (CIV), a central threat to validity within the unified framework articulated by Messick (1995) and codified in the *Standards for Educational and Psychological Testing* (AERA et al., 2014). CIV occurs when systematic variance in test scores is attributable to factors extraneous to the target construct — in this case, when ideological attitudes inflate HPT scores independently of disciplinary reasoning. If CIV from ideology is present, group comparisons and individual inferences drawn from HPT scores would be compromised, potentially rewarding ideological sympathy rather than historical understanding.

It is important to be precise about what kind of threat this represents. Several distinct mechanisms could link ideology to HPT scores: (a) *construct contamination* in the strict sense, where ideological sympathy inflates scores without any corresponding reasoning competence; (b) *construct overlap*, where the scenario content genuinely measures something that is both part of HPT and partially ideologically determined; (c) a *true causal effect* of ideology on the underlying reasoning process, for instance if authoritarian students approach historical inference differently; and (d) *differential task engagement*, where ideologically aligned students invest more cognitive effort in the scenario. The present study focuses specifically on mechanism (a) — whether ideological attitudes introduce score inflation independent of disciplinary reasoning — and tests this through the pattern of correlations, multilevel models, and measurement invariance analyses. Distinguishing (a) from (c) in principle requires process data (e.g., think-aloud protocols) that the current design does not provide; accordingly, we interpret null ideology effects as evidence against score inflation, while remaining agnostic about whether ideology shapes the reasoning process in ways that do not manifest in score differences.

Despite its theoretical salience, this threat has not been empirically tested. The present study addresses this gap through a preregistered investigation of whether right-authoritarian ideological attitudes predict HPT scores in a sample of Czech secondary students, and whether any such relationship persists after accounting for historical knowledge and socially desirable responding. We complement these hypothesis tests with item-level analyses — differential item functioning (DIF) and multi-group confirmatory factor analysis (MG-CFA) — to determine whether the measurement properties of the HPT instrument are invariant across ideological groups. In terms of contribution type, the study constitutes a **boundary-condition validation of HPT under ideological heterogeneity** (AERA et al., 2014; Ercikan & Seixas, 2015): it subjects an existing, widely-used HPT measure to a targeted validity challenge within democratic classroom contexts, asking whether historical perspective taking can be measured with epistemic fairness when students hold diverse ideological commitments.

### Competing Theoretical Predictions

It is worth noting that the direction of any ideology–HPT relationship is not theoretically settled. The contamination hypothesis assumes that right-authoritarian students would score *higher* on the Weimar scenario because they sympathize with the content: a young man weighing the appeal of an extremist movement in a context of perceived national humiliation overlaps affectively with themes central to right-authoritarian ideology (nationalist grievance, strong-leader appeal, anti-democratic sentiment). On this *congruence* pathway, ideological sympathy reduces the psychological effort of contextualizing the scenario, inflating scores as a side-effect of attitude alignment rather than reasoning quality.

However, an entirely different prediction follows from research on authoritarian cognition. High authoritarianism is associated with reduced integrative complexity — the capacity to hold multiple conflicting perspectives simultaneously (Suedfeld & Tetlock, 1977; Duckitt, 2001). Historical perspective taking, which requires situating historical actors within unfamiliar value systems while simultaneously maintaining a present-day analytical stance, is precisely the kind of cognitively integrative task where reduced complexity should manifest as poorer performance. On this *rigidity* pathway, authoritarian students would score *lower* on HPT, independently of ideological congruence with the scenario.

These two pathways generate opposite predictions and could partially cancel in aggregate data, producing a null correlation even when both mechanisms are operating. The preregistered hypotheses (H1 and H2) focus on the congruence pathway — the theoretically dominant concern for construct validity. An exploratory secondary analysis examines whether the rigidity pathway is detectable in the data.

### Hypotheses

Following the preregistered protocol, we tested two directional hypotheses:

- **H1 (Ideological contamination).** Students with stronger right-authoritarian attitudes score higher on HPT, consistent with construct-irrelevant variance from ideological sympathy with the scenario content.
- **H2 (Persistence after controls).** The H1 relationship persists after controlling for prior historical knowledge and social desirability, indicating that the ideological signal is not reducible to confounds.

Complementary measurement analyses tested whether the HPT items function equivalently across ideology groups (DIF) and whether the factor structure is invariant (MG-CFA).

---

## Literature Review

### Historical Perspective Taking: Conceptualization

HPT refers to the ability to understand the actions, decisions, and worldviews of people in the past by reconstructing the historical context in which they operated (Lee & Ashby, 2001; Hartmann & Hasselhorn, 2008). The concept has roots in British traditions of history education that distinguished "empathy" — understood not as emotional identification but as rational reconstruction — from naive presentism (Lee & Ashby, 2001; Ashby & Lee, 1987). Lee and Ashby (2001) argued that understanding historical actors requires setting aside present-day moral frameworks and recognizing that past decisions were made within fundamentally different belief systems, institutional structures, and material conditions.

Hartmann and Hasselhorn (2008) operationalized this theoretical framework into a developmental model with three hierarchical levels. At the lowest level, **presentism** (Perspektivenübernahme ohne Perspektivenwechsel, POP), students judge historical actors by contemporary standards without recognizing the alien character of the past. At an intermediate level, **recognition of the historical agent's role** (ROA), students acknowledge that the agent occupied a socially recognizable role (e.g., father, soldier) but do not fully reconstruct the contextual forces shaping the agent's reasoning. At the highest level, **contextualisation** (CONT), students situate the agent's decisions within the specific historical conditions — political, economic, cultural — that rendered those decisions intelligible at the time. Achieving contextualisation requires an epistemically demanding discipline: students must hold historical alterity — the fundamental difference of past thought, institutions, and values — in analytical view while simultaneously maintaining a critical scholarly stance toward the present. This is the epistemic norm Lee and Ashby (2001) called "rational understanding": understanding without endorsing, inhabiting a past perspective without surrendering present-day moral judgment. Measured against this standard, contextualisation is not merely a higher developmental level but a specific epistemic achievement that distinguishes disciplinary historical reasoning from culturally or ideologically assimilated interpretation. This three-level model draws on earlier developmental work by Lee and Ashby (2001) as well as Selman's (1980) social perspective-taking framework, while insisting that HPT is domain-specific: it requires understanding not merely another person's viewpoint, but a viewpoint embedded in a fundamentally different historical context (Hartmann & Hasselhorn, 2008).

Subsequent work by Huijgen and colleagues (Huijgen et al., 2014, 2017) extended this framework to larger samples and different historical topics, confirming the core theoretical structure while highlighting the difficulty of contextualisation for most students. Van Boxtel and van Drie (2018) situated HPT within a broader framework of historical reasoning that also encompasses using evidence, constructing arguments, and employing substantive concepts. Across these frameworks, HPT is understood as a cognitively demanding, knowledge-dependent form of reasoning — not a disposition or attitude, but a competence that requires both domain-specific knowledge and the disciplinary capacity to deploy that knowledge in reconstructing past perspectives (Chapman, 2021).

### Measuring HPT: The Scenario-Based Approach

The difficulty of measuring HPT stems from its inherently interpretive character. Unlike factual recall, HPT cannot be assessed through multiple-choice items that have a single correct answer (Smith, 2017; Wineburg, 2018). Hartmann and Hasselhorn (2008) addressed this challenge by designing a scenario-based instrument in which students read a historically plausible narrative and then respond to nine Likert-scaled statements (1–4), each representing one of the three HPT levels (POP, ROA, CONT; three items per level). This approach has several advantages: it is efficient to administer, enables standardized scoring, and produces interval-level data suitable for statistical modeling.

In the original validation study (*n* = 170 German students, Grade 10), Hartmann and Hasselhorn (2008) found that the three HPT levels could be recovered through principal component analysis, and that students classified at the highest level (contextualisation) had significantly better history grades. The scenario — a young man in the Weimar Republic weighing whether to join an extremist political movement — was designed in consultation with historians to be age-appropriate and thematically connected to the German curriculum.

However, the psychometric properties of the instrument also reveal limitations. Internal consistency for three-item subscales is inherently constrained; in the original study and subsequent replications, Cronbach's alpha values for individual subscales have been modest. Moreover, the scenario's content — involving a historical figure's potential radicalization — creates a specific validity concern: responses to items about contextualizing an extremist's reasoning may covary with respondents' own ideological attitudes toward extremism, introducing construct-irrelevant variance.

### Validity Framework: Construct-Irrelevant Variance

The unified validity framework (Messick, 1995; AERA et al., 2014) conceptualizes validity not as a property of a test but as the degree to which evidence and theory support the interpretations of test scores for their intended uses. Two principal threats undermine score interpretations: **construct underrepresentation** (the test fails to capture important aspects of the construct) and **construct-irrelevant variance** (the test captures systematic variance from sources other than the target construct).

CIV is particularly concerning when test content intersects with respondents' attitudes, beliefs, or social identities. Messick (1995) emphasized that validity evidence must address not only whether scores correlate with theoretically expected criteria but also whether they are free from systematic contamination by irrelevant factors. The *Standards* (AERA et al., 2014) elaborate that evidence of internal structure, including measurement invariance across relevant subgroups, constitutes essential validity evidence when scores are used to compare groups or make inferences about individuals.

In the context of HPT measurement, the CIV concern is straightforward. The Hartmann and Hasselhorn (2008) scenario asks students to reason about a historical figure contemplating political extremism. If students with right-authoritarian attitudes systematically score higher — not because they reason more historically but because they feel affinity with the scenario's ideological content — then HPT scores contain construct-irrelevant variance. This would compromise the instrument's use for measuring disciplinary reasoning and would be particularly problematic in educational contexts where HPT scores might be used to evaluate teaching effectiveness or compare student populations.

The concern is not merely hypothetical. Älven (2019) documented ideological bias in teachers' assessments of students' historical narratives, suggesting that ideological content can infiltrate judgments of historical competence. More broadly, research on attitude-consistent responding in educational measurement has shown that test items touching on politically or morally charged content can elicit responses driven by attitudes rather than the intended cognitive processes (Badham et al., 2025). The underlying mechanism is well-documented in political psychology: individuals process information in identity-protective ways, filtering evidence through pre-existing commitments to preserve group identity and ideological coherence (Kahan et al., 2012; Jost et al., 2003). If such processes operate during HPT responses, students whose ideological identity aligns with the scenario's historical actors may interpret contextualizing cues as confirming their worldview rather than as evidence of historical distance.

### The Czech Educational Context

The Czech Republic provides a particularly apt setting for this investigation. As a post-communist society, the Czech Republic has a complex relationship with totalitarian ideologies, and the Weimar Republic scenario resonates with broader European and Czech historical experiences of democratic fragility and radicalization (Cinatl et al., 2021). Czech history education has traditionally followed a transmissive model emphasizing factual knowledge, though recent reforms and initiatives — including the HistoryLab project and the Dejepis+ curriculum development effort — have sought to promote historical thinking competences including perspective taking (Pinkas, 2024; Cinatl et al., 2021).

Czech adolescents are exposed to narratives about totalitarianism through the national curriculum, family memory, and public discourse, making the relationship between ideological attitudes and historical reasoning both educationally relevant and socially significant. At the same time, the Czech adaptation of the Hartmann and Hasselhorn instrument has not previously been subjected to the kind of validity stress-test that the present study undertakes.

---

## Method

### Participants and Procedure

The sample comprised 293 students from 20 classrooms across 10 schools in the Czech Republic. The sample spanned both lower-secondary (ISCED 2; *n* = 87, 29.7%) and upper-secondary (ISCED 3; *n* = 206, 70.3%) school levels, drawn from comprehensive lower-secondary schools (základní školy) and gymnázia (four-year and multi-year). The inclusion criterion was completion of the relevant curricular unit on the interwar period — covering the Weimar Republic, the rise of extremist movements, and political polarization in 1920s–1930s Europe — irrespective of school type or exact grade level. Schools were drawn from five Czech regions (Praha: *n* = 125, 42.7%; Královéhradecký: *n* = 102, 34.8%; Zlínský: *n* = 33, 11.3%; Jihomoravský: *n* = 24, 8.2%; Ústecký: *n* = 9, 3.1%) and three school-funding categories (public: *n* = 266, 90.8%; private: *n* = 19, 6.5%; church-affiliated: *n* = 8, 2.7%). Gender composition was 192 male (65.5%), 88 female (30.0%), and 13 students identifying as another gender (4.4%). Students' most recent history course grade averaged 1.71 (*SD* = 0.92, range 1–5; *n* = 289) on the Czech grading scale where 1 = excellent and 5 = failing, indicating above-average subject-area preparation, consistent with the selective academic profile of the gymnázium-majority sample. Socioeconomic background and parental education were not collected. After excluding cases with missing cluster identifiers (*n* = 6–17 depending on variable combination), the analytic sample ranged from 276 to 287.

Schools were recruited through convenience sampling stratified by Czech region and school type. A formal comparison against national school population distributions was not conducted; the sample over-represents gymnázium students relative to the national composition of the relevant age cohort and should not be treated as a probability sample of Czech adolescents. Sample size was determined by an a priori power analysis specified in the preregistered protocol (OSF: https://osf.io/yng37/), targeting 80% power to detect standardized ideology effects of β ≥ .15 in a two-level multilevel model; the achieved *N* = 285–293 met this target. Within each school, all students present on the day of administration completed the instrument battery during a regular class period (approximately 30–40 minutes). Teachers were present during administration but did not intervene in students' responses.

### Instruments

#### Historical Perspective Taking (HPT)

We used a Czech adaptation of the Hartmann and Hasselhorn (2008) scenario-based HPT instrument. The Czech version was developed through forward translation from the original German by a bilingual Czech-German historian, followed by expert review of the scenario narrative and all nine items by Czech history educators to ensure conceptual equivalence and age-appropriateness for Czech secondary students who have completed the interwar period curriculum unit. The scenario narrative was cross-checked against the Czech history curriculum to confirm that all contextual references were familiar to the target population regardless of school type. The original four-point response format, the three-factor item structure, and the reverse-scoring of POP items were retained unchanged from the validated German version (cf. Cinatl et al., 2021, for the use of this instrument in the Czech HistoryLab context). Students read a narrative about a young man in the Weimar Republic who is considering joining an extremist political movement, then responded to nine items on a 4-point Likert scale (1 = *strongly disagree* to 4 = *strongly agree*). Three items each tapped presentism (POP1–POP3), recognition of the agent's role (ROA1–ROA3), and contextualisation (CONT1–CONT3).

Following Hartmann and Hasselhorn (2008) and Huijgen et al. (2014), POP items were reverse-scored (5 − POP) so that higher scores on all items indicate more contextualised, disciplinarily desirable reasoning. The primary dependent variable was **HPT_CTX6**, the mean of reversed POP and CONT items (six items), which captures the core contrast between presentism and contextualisation. The nine-item total (HPT_TOT9, adding ROA) served as a secondary outcome in sensitivity analyses. CONT was also analyzed separately as the theoretically highest-level subscale.

#### Right-Authoritarian Ideology (FR-LF mini)

Ideological attitudes were measured using a six-item short form of the Fragebogen Rechtsextremer Einstellungen — Leipziger Form (FR-LF; Decker et al., 2013). The scale comprises two facets: acceptance of right-wing dictatorship (RD1–RD3) and National Socialist sympathy/relativization (NS1–NS3), each rated on a 5-point Likert scale (1 = *strongly disagree* to 5 = *strongly agree*). Higher scores indicate stronger right-authoritarian attitudes. The composite FR-LF score (mean of all six items) served as the primary ideology predictor representing generalized right-authoritarian orientation. Facet scores were examined in supplementary models. Theoretically, the NS facet (National Socialist sympathy/relativization) is the most direct operationalization of the *content congruence* contamination pathway — students endorsing NS items sympathize with the ideological content most proximate to the Weimar scenario — while the RD facet indexes preference for strong-leader authoritarianism more broadly. This distinction was tested in the NS-only invariance analysis.

#### Authoritarianism (KSA-3)

We administered the Kurze Skala Autoritarismus (KSA-3; Beierlein et al., 2014), a nine-item measure of authoritarian attitudes comprising three facets: authoritarian aggression (A1–A3), authoritarian submission (U1–U3), and conventionalism (K1–K3), each rated on a 5-point scale. KSA-3 served as a convergent indicator of the authoritarian attitude domain alongside FR-LF, as preregistered. Theoretically, KSA-3 operationalizes the *cognitive rigidity* pathway: authoritarian aggression, submission, and conventionalism are associated in the literature with reduced integrative complexity (Duckitt, 2001; Suedfeld & Tetlock, 1977), which could inhibit rather than inflate HPT performance. The inclusion of both FR-LF and KSA-3 thus allows the study to test both the congruence pathway (which predicts higher scores for ideology-concordant students) and the rigidity pathway (which predicts lower scores for cognitively authoritarian students) simultaneously.

#### Historical Knowledge (KN)

Prior historical knowledge was assessed using a six-item multiple-choice test (KN1–KN6) covering content directly relevant to the HPT scenario: the political, economic, and social conditions of the Weimar Republic, the rise of extremist movements in interwar Germany, and the circumstances facing young people in 1930s Germany. Each item was scored dichotomously (0 = incorrect, 1 = correct), yielding a sum score ranging from 0 to 6. The items were developed in consultation with the curricular content that Czech ninth-grade students are expected to have covered prior to the study, ensuring that the knowledge test functions as a targeted diagnostic of scenario-relevant background knowledge rather than a proxy for general intelligence or academic achievement.

#### Social Desirability (SDR-5)

Social desirability was measured using the five-item short form developed by Hays et al. (1989), with items rated on a 5-point scale. Items SDR2–SDR4 were reverse-coded prior to data collection per the published scoring protocol. The SDR-5 mean served as a control variable to address the possibility that apparent HPT performance reflects socially desirable responding rather than genuine reasoning.

### Analytic Approach

All analyses were conducted in R (version 4.4.2) using lme4 and lmerTest for multilevel models, lavaan for CFA, and mirt for IRT-based DIF analysis.

#### Measurement Quality

We first evaluated the internal structure and reliability of the HPT instrument. Confirmatory factor analyses (CFA) compared one-factor, two-factor (POP+CONT vs. ROA), and three-factor (POP, ROA, CONT) models using the WLSMV estimator for ordered-categorical data. Reliability was assessed via Cronbach's alpha (raw and polychoric) and McDonald's omega. Intraclass correlations (ICCs) were computed to quantify school- and classroom-level variance in HPT scores, informing the multilevel modeling strategy.

#### Differential Item Functioning

To test whether individual HPT items function differently across ideology groups, we formed Low and High ideology groups using tertile splits on a composite ideology score (the mean of z-scored FR-LF and z-scored KSA-3 composites), as preregistered (OSF: https://osf.io/yng37/). The middle tertile was excluded to maximize contrast between groups. This approach follows established practice in DIF and MG-CFA research where extreme group designs improve detection sensitivity (Finch, 2005). We acknowledge that tertile splits are sample-dependent and reduce available N (to approximately *n* = 80–96 per group), which limits sensitivity to small DIF effects; these limitations are discussed in the Limitations section. As a supplementary sensitivity check, we also ran MG-CFA using NS-only grouping (grouping by National Socialist sympathy score only, the facet most directly content-congruent with the scenario) to verify that the invariance finding is not an artifact of the composite ideology definition.

We fitted a constrained multi-group graded response model (slopes and intercepts constrained equal, latent means and variances free) and tested each item for DIF using likelihood-ratio tests with Bonferroni correction (α = .01). Both uniform DIF (threshold differences) and non-uniform DIF (slope differences) were tested.

#### Multi-Group CFA

Measurement invariance was assessed through a stepwise multi-group CFA across Low and High ideology groups: configural (same factor structure), metric (equal loadings), and scalar (equal loadings and thresholds). Models were estimated with WLSMV on ordered items. Invariance was evaluated using changes in CFI (ΔCFI ≤ .01) and RMSEA (ΔRMSEA ≤ .015), following established guidelines (Chen, 2007; Cheung & Rensvold, 2002).

#### Hypothesis Tests

H1 and H2 were tested using random-intercept multilevel models with students nested in classrooms within schools (two random intercepts: school and class). All predictors were z-standardized for comparability. The base model specification was:

$$\text{HPT}_{ijk} = \gamma_{00} + \gamma_{10}\text{FR-LF}_{ijk} + \gamma_{20}\text{KSA}_{ijk} + \gamma_{30}\text{KN}_{ijk} + \gamma_{40}\text{SDR}_{ijk} + u_{0j} + u_{0k} + e_{ijk}$$

where *i* indexes students, *j* classrooms, and *k* schools. This model tests H2 directly (ideology predicting HPT after controls). H1 was assessed through zero-order correlations and by inspecting the ideology coefficient in models without control variables. Supplementary models examined FR-LF facets (RD, NS) separately and the FR-LF × KN interaction. The moderate correlation between FR-LF and KSA-3 (*r* = .52) was the largest inter-predictor correlation; given the remaining small predictor intercorrelations (|*r*| ≤ .19; see Table 4), variance inflation factors (VIFs) for all model predictors were below 2.0 in every model specification, indicating that multicollinearity did not constitute a threat to the coefficient estimates.

Three dependent variables were analyzed: HPT total (CTX6), CONT, and POP_rev.

#### Sensitivity Analyses

Robustness was assessed across (a) alternative HPT scoring composites (6-item, 8-item, 9-item), (b) different ideology operationalizations (FR-LF total, NS facet, KSA-3 total), (c) exclusion of high social desirability responders (top 10%), (d) random-slope models allowing ideology effects to vary across classrooms, and (e) school fixed-effects models replacing school random intercepts with school dummy variables.

*Post hoc supplementary analyses (not preregistered):* To move beyond null-hypothesis significance testing and provide formally bounded inference about ideology effects, we conducted two additional analyses. First, formal two one-sided tests (TOST; Lakens, 2017) were used to test equivalence between the observed ideology effects and a region of practical equivalence defined by the SESOI of β = ±0.20 — the threshold below which ideology effects would be too small to influence educational decisions about instrument use (corresponding to approximately 4% explained variance; Cohen, 1988). Second, a Mundlak-style contextual effects model (Mundlak, 1978) decomposed ideology into within-class centered scores and class-mean scores, testing whether classroom-level ideological climate predicts HPT outcomes independently of individual ideology.

---

## Results

### Descriptive Statistics

Table 1 presents descriptive statistics for all study variables. HPT scores showed the expected ordering across subscales: students scored highest on reversed presentism (POP_rev: *M* = 2.97, *SD* = 0.64), followed by role of agent (ROA: *M* = 2.80, *SD* = 0.68) and contextualisation (CONT: *M* = 2.71, *SD* = 0.74). The primary composite HPT_CTX6 had a mean of 2.84 (*SD* = 0.55) on the 1–4 scale. Ideology scores were below the scale midpoint (FR-LF: *M* = 2.48, *SD* = 0.75; KSA: *M* = 2.86, *SD* = 0.62), indicating that most students did not endorse strong right-authoritarian attitudes. Both ideology distributions were positively skewed, with the mass of responses concentrated in the lower range of the scales, consistent with floor effects typical of right-extreme attitude measures in mainstream adolescent samples. Only 17.7% of the full sample scored above the FR-LF scale midpoint (3.0). Even within the "High ideology" tertile used in DIF and MG-CFA analyses (*n* = 96, FR-LF *M* = 3.20, *SD* = 0.62), only 50% exceeded the scale midpoint — underscoring that this group represents moderately elevated, not extreme, ideological endorsement. Historical knowledge was moderate (*M* = 3.04, *SD* = 1.62 on a 0–6 scale).

**Table 1.** *Descriptive Statistics for Study Variables*

| Variable | *n* | *M* | *SD* | Min | Max |
|:---------|----:|----:|-----:|----:|----:|
| HPT POP (reversed) | 287 | 2.97 | 0.64 | 1.33 | 4.00 |
| HPT ROA | 287 | 2.80 | 0.68 | 1.00 | 4.00 |
| HPT CONT | 287 | 2.71 | 0.74 | 1.00 | 4.00 |
| HPT CTX6 (primary) | 287 | 2.84 | 0.55 | 1.33 | 4.00 |
| HPT TOT9 | 287 | 2.83 | 0.49 | 1.67 | 3.89 |
| KN Total (0–6) | 293 | 3.04 | 1.62 | 0.00 | 6.00 |
| FR-LF RD | 285 | 2.54 | 0.88 | 1.00 | 5.00 |
| FR-LF NS | 286 | 2.43 | 0.89 | 1.00 | 5.00 |
| FR-LF Total | 284 | 2.48 | 0.75 | 1.00 | 5.00 |
| KSA-3 Total | 283 | 2.86 | 0.62 | 1.00 | 5.00 |
| SDR-5 Total | 283 | 3.02 | 0.63 | 1.00 | 4.60 |

*Note.* HPT items scored on a 1–4 scale; POP items reverse-coded. FR-LF and KSA-3 scored on a 1–5 scale. KN scored as sum of correct responses (0–6).

[**Figure 3** about here: Score distributions for HPT outcomes, ideology predictors, and historical knowledge.]

### Measurement Properties of the HPT Instrument

#### Factor Structure

Confirmatory factor analysis supported the three-factor model (POP, ROA, CONT) as the best-fitting structure for the HPT items in the Czech sample (see Table 2). The three-factor model showed near-perfect fit indices (CFI = .999, TLI = .998, RMSEA = .011, 90% CI [.000, .051], SRMR = .048; the near-ceiling CFI reflects the three-indicator factor structure rather than genuine model adequacy — see the local fit discussion below), substantially outperforming both the two-factor model combining POP and CONT (CFI = .956, RMSEA = .057) and the one-factor model (CFI = .926, RMSEA = .073). Standardized factor loadings in the three-factor model ranged from .30 to .74, with CONT items loading most strongly (.61–.72) and POP2 showing the weakest loading (.30). The relatively low loading of POP2 warrants brief comment: a loading of .30 is above the conventional minimum threshold (.30) for item retention, and POP2 was retained to preserve fidelity to the original instrument structure. The weak loading suggests POP2 carries greater unique variance than the other POP items and may be tapping a slightly distinct facet of presentism; this should be considered in future revisions of the Czech adaptation.

Local fit was evaluated through modification indices and standardized residual correlations. The largest modification index was 7.35 (ROA =~ POP1), below the conventional threshold of 10 that would indicate substantive local misfit. Standardized residual correlations were small, with the largest being POP2–POP3 (*r* = .13) and ROA3–POP2 (*r* = −.11). We also tested whether reverse-coding of POP items introduced a method factor by comparing the three-factor model against a version including a method factor for the three reverse-coded items; this did not improve fit meaningfully. The near-perfect global fit indices (CFI = .999) reflect the small item set (9 items in three just-identified three-indicator factors) and should be interpreted alongside the local fit diagnostics, which confirm no substantial misspecification.

These results provide confirmatory evidence for the three-factor structure that the original Hartmann and Hasselhorn (2008) study could only suggest through principal component analysis (N = 170, explained variance = 51%). The original study used PCA rather than CFA and noted instability in the separation of POP and CONT as distinct components — a limitation acknowledged in subsequent replications. The present CFA, with a larger Czech sample and a confirmatory framework, establishes that CONT is a separable factor from POP and ROA: the two-factor model forcing POP and CONT onto a single factor showed meaningfully worse fit (ΔCFI = −.043), confirming that contextualisation represents a distinct facet of HPT rather than simply the absence of presentism.

**Table 2.** *CFA Fit Indices for HPT Factor Models (WLSMV Estimator)*

| Model | χ² | CFI | TLI | RMSEA | 90% CI | SRMR |
|:------|---:|----:|----:|------:|:------:|-----:|
| 1-Factor | — | .926 | .902 | .073 | [.051, .095] | .075 |
| 2-Factor (POP+CONT, ROA) | — | .956 | .940 | .057 | [.032, .081] | .066 |
| 3-Factor (POP, ROA, CONT) | — | .999 | .998 | .011 | [.000, .051] | .048 |

#### Reliability

Internal consistency estimates reflect the inherent constraints of three-item subscales designed to capture heterogeneous facets of a complex cognitive process (Table 3). Raw Cronbach's alpha values were .47 (POP), .50 (ROA), and .64 (CONT). Polychoric alpha, which accounts for the ordinal nature of the items, was somewhat higher: .53 (POP), .53 (ROA), and .69 (CONT). These alpha values are comparable to those reported in the original Hartmann and Hasselhorn (2008) study and subsequent replications.

McDonald's omega total — the appropriate reliability estimate when subscales are embedded within a multidimensional instrument — ranged from .55 to .69. CONT demonstrated the strongest reliability (.69), consistent with its highest average factor loadings.

The modest subscale alphas require contextualisation. Cronbach's alpha penalizes item heterogeneity, and HPT subscales are intentionally broad to represent distinct facets of a multidimensional construct rather than narrow unidimensional traits. A critical test of whether low reliability is producing measurement noise rather than simply reflecting construct breadth comes from the nomological network: if subscales were primarily random noise, no substantive predictor should show consistent associations. Yet historical knowledge predicted HPT outcomes with standardized coefficients of .21–.34 across all three subscales. The robustness of this knowledge effect across subscales with varying reliability argues that the instrument carries genuine signal, and that the modest alpha for POP and ROA reflects construct breadth rather than random error.

Floor and ceiling effects were also evaluated for the HPT composites. The primary composite HPT_CTX6 ranged from 1.33 to 4.00 (Table 1), indicating that the scale ceiling was reachable but that the distribution centered well below it (*M* = 2.84, *SD* = 0.55). No student scored at the absolute floor of 1.00 on the composite, and the minimum of 1.33 suggests that the lowest scores reflect genuine responses across items rather than a concentration at floor. The ideology scales showed no ceiling effects (FR-LF max = 5.00, in range; KSA-3 max = 5.00), and their positive skew reflects the expected floor tendency in mainstream non-extreme samples rather than a measurement artifact.

A further test addresses the concern that low reliability attenuates the observed ideology–HPT correlations, masking a true effect. Correcting the observed zero-order correlation between FR-LF and HPT_CTX6 (*r* = −.043) for attenuation using the reliability estimates for both measures (α_FR-LF = .67; ω_HPT ≈ .65) yields a disattenuation-corrected *r* = −.065. This calculation indicates that attenuation does not plausibly mask a moderate association: even under the optimistic assumption of perfect reliability, the estimated true-score correlation remains trivially small (−.065), well below any threshold of practical significance.

**Table 3.** *Reliability Coefficients for HPT Subscales*

| Subscale | Items | α (raw) | α (polychoric) | ω total |
|:---------|------:|--------:|---------------:|--------:|
| POP | 3 | .47 | .53 | .55 |
| ROA | 3 | .50 | .53 | .56 |
| CONT | 3 | .64 | .69 | .69 |

*Note.* POP items are reverse-scored. α = Cronbach's alpha; ω = McDonald's omega total.

#### Intraclass Correlations

ICCs indicated meaningful school-level clustering for most HPT outcomes (HPT_CTX6: ICC_school = .041; HPT_TOT9: ICC_school = .085; ROA: ICC_school = .102), justifying multilevel modeling. Classroom-level variance was generally near zero for the primary composite (HPT_CTX6) but non-negligible for CONT (ICC_class = .033) and ROA (ICC_class = .104). These patterns support the two-level random-intercept specification used in hypothesis testing.

ICCs for the ideology predictors at classroom level were small but non-negligible: FR-LF ICC_class = .061, KSA-3 ICC_class = .097. This indicates modest classroom clustering of ideological attitudes — consistent with shared social environments, peer-group influences, or school selection. This clustering raises the theoretical possibility that ideology might operate as a classroom-level characteristic (climate effect) rather than purely at the individual level. However, the null individual-level effects across all sensitivity specifications, combined with the negligible magnitude of the ideology ICCs, make a substantial classroom-level contamination pathway unlikely. The random-intercept specification absorbs classroom-level variance in the outcome, and the sensitivity analysis using school fixed effects (see Sensitivity Analyses) further confirms that the null result is not an artifact of any particular variance structure.

### Zero-Order Correlations

Table 4 presents Pearson correlations among key study variables. The most striking finding at the bivariate level is the near-complete absence of association between ideology and HPT. The correlation between FR-LF total and the primary HPT composite (HPT_CTX6) was *r* = -.05; between FR-LF and CONT it was *r* = .01; and between FR-LF and POP_rev it was *r* = -.09. KSA-3 showed similarly negligible correlations with HPT outcomes (*r* = -.03 to .07).

By contrast, historical knowledge (KN) was the strongest correlate of HPT performance: *r* = .34 with HPT_CTX6, *r* = .22 with CONT, and *r* = .32 with POP_rev. The ideology measures correlated moderately with each other (FR-LF–KSA: *r* = .52), confirming convergent validity within the authoritarian attitude domain. Social desirability showed weak negative associations with ideology (FR-LF–SDR: *r* = -.19) and was unrelated to HPT (*r* = -.02).

**Table 4.** *Zero-Order Pearson Correlations Among Key Variables*

|  | HPT_CTX6 | HPT_CONT | HPT_POP | KN | FR-LF | KSA | SDR |
|:-|:--------:|:--------:|:-------:|:--:|:-----:|:---:|:---:|
| HPT_CTX6 | — | | | | | | |
| HPT_CONT | .82 | — | | | | | |
| HPT_POP (rev) | .76 | .26 | — | | | | |
| KN Total | .34 | .22 | .32 | — | | | |
| FR-LF Total | -.05 | .01 | -.09 | -.10 | — | | |
| KSA-3 Total | -.03 | .07 | -.13 | .05 | .52 | — | |
| SDR-5 Total | -.02 | .01 | -.05 | .03 | -.19 | -.15 | — |

*Note.* Pairwise complete observations (*n* = 276–287). HPT_POP is reverse-scored.

### Differential Item Functioning

DIF analysis tested whether individual HPT items functioned differently across Low and High ideology groups (defined by tertile split on the IDEO_Z composite). Using a constrained multi-group graded response model with likelihood-ratio tests for each item (Bonferroni-adjusted α = .01), **no items were flagged for either uniform or non-uniform DIF** (Table 5). All adjusted *p*-values exceeded .01, indicating that the item parameters — both discrimination (slopes) and threshold (location) parameters — were statistically equivalent across ideology groups.

**Table 5.** *DIF Results per HPT Item (Multi-Group Graded Response Model)*

| Item | *p* (non-uniform) | *p* (uniform) | Flagged |
|:-----|-------------------:|--------------:|:-------:|
| POP1 | > .01 | > .01 | No |
| POP2 | > .01 | > .01 | No |
| POP3 | > .01 | > .01 | No |
| ROA1 | > .01 | > .01 | No |
| ROA2 | > .01 | > .01 | No |
| ROA3 | > .01 | > .01 | No |
| CONT1 | > .01 | > .01 | No |
| CONT2 | > .01 | > .01 | No |
| CONT3 | > .01 | > .01 | No |

*Note.* Non-uniform DIF tests slope (discrimination) differences; uniform DIF tests threshold differences. All *p*-values Bonferroni-adjusted (α = .01). 0 of 9 items flagged.

### Multi-Group CFA: Measurement Invariance

The stepwise MG-CFA across Low and High ideology groups demonstrated full measurement invariance (Table 6). The configural model, which allowed all parameters to differ across groups, showed adequate fit (CFI = .978, RMSEA = .055, SRMR = .079). Constraining factor loadings to equality (metric model) produced virtually no change in fit (ΔCFI = .000, ΔRMSEA = −.006), supporting equivalent loadings across groups. Constraining both loadings and thresholds (scalar model) similarly maintained or improved fit (ΔCFI = +.002, ΔRMSEA = −.007), providing evidence for full scalar invariance.

**Table 6.** *Multi-Group CFA Fit Indices Across Invariance Levels (WLSMV)*

| Model | CFI | RMSEA | SRMR | ΔCFI | ΔRMSEA |
|:------|----:|------:|-----:|-----:|-------:|
| Configural | .978 | .055 | .079 | — | — |
| Metric | .978 | .049 | — | .000 | −.006 |
| Scalar | .980 | .042 | — | +.002 | −.007 |

*Note.* ΔCFI and ΔRMSEA computed relative to the preceding model. Guidelines for invariance: |ΔCFI| ≤ .01, |ΔRMSEA| ≤ .015 (Chen, 2007).

Full scalar invariance indicates that the factor structure, loadings, and item thresholds of the HPT instrument are equivalent across ideology groups, meaning that observed group differences (or, in this case, the absence thereof) in HPT scores can be attributed to true differences in the latent construct rather than to measurement artifacts. In epistemic terms, this equivalence confirms that all students — regardless of ideological background — face epistemically equivalent demands when responding to the HPT instrument: the items make the same cognitive appeal and require the same interpretive work across ideology groups. This is a necessary condition for measurement fairness in the disciplinary sense: if different students found the items to evoke different cognitive tasks or if the response categories carried different meanings across groups, the instrument could not support equitable assessment of disciplinary reasoning.

As a supplementary sensitivity check addressing concerns about the composite ideology grouping, we re-ran the MG-CFA using groups defined by NS score alone (National Socialist sympathy — the ideology facet most directly content-congruent with the Weimar scenario). Results were consistent with the primary analysis: configural fit indices reached ceiling values (CFI = 1.000, RMSEA = .000), which indicates near-model-saturation rather than a good fit — a known artifact when group sizes are small (~*n* = 80 per group) and the configural model has very few degrees of freedom with ordinal WLSMV estimation. In this configuration, absolute fit statistics are uninformative; invariance is evaluated from ΔCFI relative to the configural baseline. Scalar invariance was supported (metric ΔCFI = −.002, scalar ΔCFI = +.002), both within the |ΔCFI| ≤ .01 criterion. This confirms that measurement invariance holds even when groups are defined by the ideology facet theoretically most likely to interact with the scenario content.

Figure 2 visualizes the primary invariance results through factor loadings (Panel A) and IRT item parameter comparisons (Panel B).

[**Figure 2** about here: Measurement invariance and DIF across ideological groups. Panel A shows standardized factor loadings from configural MG-CFA. Panel B shows IRT item parameter scatter plots from the multi-group graded response model.]

### Hypothesis Tests: Multilevel Models

#### H1: Ideology Predicting HPT (Uncontrolled)

At the bivariate level, the correlations reported in Table 4 already indicate that H1 is not supported: ideology is essentially unrelated to HPT scores. To provide a more rigorous test accounting for the clustered data structure, we also note that zero-order correlations between FR-LF and all HPT outcomes were negligible and non-significant (*r* = -.09 to .01).

#### H2: Controlled Multilevel Models

Table 7 presents results from the preregistered multilevel models including all predictors simultaneously. Across all three HPT outcomes and all model specifications (base, facet, interaction), ideology was consistently non-significant.

For the primary outcome **HPT total (CTX6)**, FR-LF was unrelated to HPT scores: β = 0.012, *SE* = 0.067, 95% CI [−0.120, 0.143], *p* = .863. KSA-3 was similarly non-significant (β = −0.052, *p* = .435). **Historical knowledge was the only significant predictor** (β = 0.336, *SE* = 0.057, 95% CI [0.223, 0.448], *p* < .001), accounting for virtually all of the model's explanatory power (*R*²_marginal = .113). Social desirability was non-significant (β = −0.043, *p* = .455).

For **contextualisation (CONT)**, the pattern was identical: FR-LF β = 0.004, *SE* = 0.069, 95% CI [−0.131, 0.140], *p* = .949. Knowledge again emerged as the sole significant predictor (β = 0.211, *p* < .001). The marginal *R*² was lower (.049), reflecting the more modest proportion of CONT variance explained by the predictors. The conditional *R*² (.072) indicated a small additional contribution from classroom-level clustering.

For **reversed presentism (POP_rev)**, FR-LF was again non-significant (β = 0.005, *p* = .945). Knowledge was a strong predictor (β = 0.334, *p* < .001). Notably, KSA-3 showed a small but statistically significant *negative* association with POP_rev (β = −0.159, *SE* = 0.065, 95% CI [−0.288, −0.031], *p* = .016), suggesting that students with higher authoritarianism scores were slightly *more* presentist — the opposite direction from the contamination hypothesis. This effect was not preregistered and warrants cautious interpretation. Importantly, however, it is directionally opposite to what construct contamination would produce: if ideology were inflating scores, high-KSA students should score *higher* on POP_rev, not lower. The negative coefficient is instead consistent with a cognitive rigidity account — the well-documented association between high authoritarianism and reduced integrative complexity (Jost et al., 2003; Duckitt, 2001) — whereby authoritarian students find the "unnatural act" of setting aside present-day frameworks more cognitively demanding. On this reading, the KSA-3 result actually supports the instrument's discriminant validity: ideology does not inflate HPT performance, but certain authoritarian cognitive styles may be associated with genuine difficulty at the demanding end of the presentism–contextualisation spectrum.

**Table 7.** *Fixed Effects from Multilevel Models Predicting HPT Outcomes (Standardized Coefficients)*

| Predictor | HPT CTX6 (Base) | CONT (Base) | POP_rev (Base) |
|:----------|:---------------:|:-----------:|:--------------:|
| FR-LF (z) | 0.012 (0.067) | 0.004 (0.069) | 0.005 (0.066) |
| KSA-3 (z) | −0.052 (0.066) | 0.062 (0.068) | −0.159* (0.065) |
| KN (z) | 0.336*** (0.057) | 0.211*** (0.059) | 0.334*** (0.056) |
| SDR-5 (z) | −0.043 (0.057) | 0.007 (0.059) | −0.082 (0.057) |
| *R*²_marginal | .113 | .049 | .132 |
| *N* | 285 | 285 | 285 |

*Note.* Standard errors in parentheses. All predictors z-standardized. Random intercepts for school and classroom.
\* *p* < .05. \*\*\* *p* < .001.

[**Figure 4** about here: Forest plot of standardized coefficients (β) with 95% CIs from multilevel models predicting HPT CTX6, Contextualisation, and Presentism (reversed).]

To move beyond conventional null-hypothesis testing, we conducted formal two one-sided equivalence tests (TOST; Lakens, 2017) using a pre-specified SESOI of β = ±0.20 — the threshold below which ideology effects would be too small to influence practical decisions about instrument use (approximately 4% explained variance; Cohen, 1988). *Note: TOST was not preregistered; it is reported as a post hoc supplementary analysis to strengthen the inferential basis of the null result.*

TOST confirmed equivalence for all three HPT outcomes. For **HPT_CTX6**: TOST *p* = .0026, 95% CI [−0.120, 0.143]; for **CONT**: TOST *p* = .0024, 95% CI [−0.131, 0.140]; for **POP_rev**: TOST *p* = .0017, 95% CI [−0.125, 0.134]. In all cases, the 90% CIs (appropriate for TOST interpretation) fall entirely within the [−0.20, +0.20] equivalence bounds, indicating that ideology effects are statistically ruled out as practically significant at conventional α = .05. The observed point estimates (β = 0.001–0.015 across outcomes) are 13–200 times smaller than the SESOI.

The facet models, which decomposed FR-LF into its RD and NS components, yielded the same pattern: neither facet significantly predicted any HPT outcome (all *p*s > .21). The interaction models, testing whether the FR-LF effect was moderated by historical knowledge (FR-LF × KN), found no significant interaction for HPT_CTX6 (β = −0.020, *p* = .732) or POP_rev (β = 0.074, *p* = .210). For CONT, the interaction approached but did not reach conventional significance (β = −0.100, *p* = .099).

Figure 5 visualizes the substantive triviality of the ideology–HPT relationship through predicted marginal effects from the uncontrolled (H1) and controlled (H2) models. Both prediction lines are essentially flat across the full range of observed FR-LF scores, with nearly complete overlap between models.

[**Figure 5** about here: Predicted marginal effects of ideology on HPT contextualisation from the uncontrolled (H1) and controlled (H2) multilevel models.]

### Sensitivity Analyses

The null finding for ideology was robust across all preregistered sensitivity checks (Table 8). Across six alternative model specifications — varying HPT scoring (6-item, 8-item, 9-item), ideology operationalization (FR-LF total, NS facet, KSA-3 total), sample composition (full sample vs. excluding top-10% SDR), and variance structure (random intercepts vs. random slopes for ideology) — the ideology coefficient remained non-significant in every case (all *p*s > .43). Point estimates clustered tightly around zero, with 95% confidence intervals consistently spanning the null.

**Table 8.** *Sensitivity Analysis: Ideology Coefficients Across Model Specifications*

| HPT Score | Predictor | Sample | Model | *B* | 95% CI | *p* |
|:----------|:----------|:-------|:------|----:|:------:|----:|
| 6-item | FR-LF (z) | Full | RI | 0.009 | [−0.057, 0.074] | .798 |
| 6-item | FR-LF (z) | Full | RS | 0.007 | [−0.081, 0.096] | .864 |
| 6-item | NS (z) | Full | RI | 0.026 | [−0.038, 0.089] | .429 |
| 6-item | KSA-3 (z) | Full | RI | −0.020 | [−0.088, 0.048] | .561 |
| 8-item | FR-LF (z) | Full | RI | 0.009 | [−0.049, 0.067] | .760 |
| 9-item | FR-LF (z) | Full | RI | 0.021 | [−0.037, 0.079] | .471 |
| 6-item | FR-LF (z) | Excl. | RI | −0.006 | [−0.075, 0.062] | .853 |
| 8-item | FR-LF (z) | Excl. | RI | −0.006 | [−0.068, 0.055] | .838 |
| 9-item | FR-LF (z) | Excl. | RI | 0.005 | [−0.056, 0.065] | .876 |
| 6-item | FR-LF (z) | Full | FE | 0.028 | [−0.106, 0.163] | .677 |

*Note.* RI = random intercepts; RS = random slopes for ideology; FE = school fixed effects (dummy variables) instead of random intercepts. Excl. = excluding top-10% SDR respondents. All models control for KN and SDR.

Particularly noteworthy is that the null result survived the random-slopes specification, which allows ideology effects to vary across classrooms. This indicates that the absence of an ideology effect is not an artifact of averaging over heterogeneous classroom-level relationships.

*Post hoc supplementary analysis:* To test the classroom climate contamination pathway, we estimated a Mundlak contextual effects model decomposing FR-LF into within-class centered scores (individual deviation from class mean) and class-mean scores (between-class ideology climate). Neither component predicted HPT in any outcome: within-class FR-LF (HPT_CTX6: β = +0.015, *p* = .828; CONT: β = +0.001, *p* = .988; POP_rev: β = +0.023, *p* = .734) and between-class class mean FR-LF (HPT_CTX6: β = −0.023, *p* = .909; CONT: β = +0.065, *p* = .771; POP_rev: β = −0.170, *p* = .329) were uniformly non-significant. This rules out the possibility that ideology contamination operates at the classroom climate level even while individual-level effects are zero.

Addressing the concern that multilevel models with only 10 schools may produce biased standard errors (Maas & Hox, 2005), we also estimated the model with school fixed effects (dummy-coded) rather than random intercepts. This specification does not rely on Level-2 sample size assumptions for unbiased inference. The fixed-effects school model yielded FR-LF β = 0.028, *SE* = 0.068, 95% CI [−0.106, 0.163], *p* = .677 — virtually identical to the random-intercepts result — confirming that the null ideology effect is not an artifact of the multilevel specification with few schools.

---

## Discussion

### Summary of Findings

This study tested whether the Hartmann and Hasselhorn (2008) HPT instrument exhibits ideological contamination within the range of right-authoritarian attitudes typical of Czech secondary classrooms (základní školy and gymnázia). Across every analysis reported here, no detectable contamination was found. Neither of the preregistered hypotheses was supported. Right-authoritarian ideology, measured by both the FR-LF mini and the KSA-3, was unrelated to HPT scores at the bivariate level (*r* = -.05 to .01) and in multilevel models controlling for historical knowledge and social desirability (all β ≈ 0, all *p*s > .43). The finding was invariant across three HPT scoring composites (6, 8, and 9 items), multiple ideology operationalizations, inclusion and exclusion of high social desirability responders, and models allowing random slopes.

At the item level, DIF analysis found no items that functioned differently across ideology groups at detectable magnitudes under the present sample size, and multi-group CFA demonstrated full scalar invariance — meaning that the factor structure, loadings, and thresholds of the HPT instrument are equivalent for students with low and high ideological attitudes. These converging lines of evidence — from bivariate correlations through latent-variable invariance tests — provide consistent support for the conclusion that HPT scores, under the conditions studied, reflect disciplinary reasoning rather than ideological sympathy with the scenario content. This conclusion is bounded by the ideological range observed in a Czech mainstream school sample, the use of explicit self-report ideology measures, and the specific Weimar Republic scenario; generalization beyond these conditions requires further empirical work.

### What Does Predict HPT? The Role of Knowledge

Although the study was designed primarily to test for ideological contamination, a secondary finding is equally consequential: **historical knowledge was the only consistent predictor of HPT performance** across all outcomes and specifications. The standardized effect of KN was moderate and robust (β = 0.21 for CONT; β = 0.33–0.34 for HPT_CTX6 and POP_rev; all *p* < .001), persisting unchanged whether or not ideology was included in the model.

This finding supports the theoretical conceptualization of HPT as a knowledge-dependent competence rather than a general cognitive disposition. Hartmann and Hasselhorn (2008) emphasized that the ability to take a historical perspective requires domain-specific knowledge about the historical context in question. Our results corroborate this claim empirically: students who knew more about the relevant historical period scored higher on HPT, whereas students' ideological attitudes had no bearing on their performance. Controlling for historical knowledge is theoretically essential here — not merely a statistical precaution — precisely because HPT is constitutively knowledge-dependent: the ability to contextualize past decisions requires knowing the actual political, economic, and social facts that constrained action. By isolating ideology's role after accounting for genuine disciplinary competence, the null ideology effect demonstrates that right-authoritarian attitudes do not provide performance benefits over and above what historical knowledge itself predicts. Ideology does not appear to function as a shortcut that delivers context-rich responses without the underlying knowledge — a finding with direct implications for the instrument's validity in heterogeneous classrooms. This is consistent with broader arguments in history education research that disciplinary reasoning depends on substantive historical knowledge (Chapman, 2021; Wineburg, 2001; van Boxtel & van Drie, 2018).

The knowledge–HPT relationship also supports the construct validity of the instrument from the perspective of nomological network evidence (Messick, 1995). The pattern of correlations — strong knowledge effects, null ideology effects, null social desirability effects — is precisely what would be expected if the instrument were measuring disciplinary reasoning and not some combination of attitudes and impression management.

### Alternative Explanations for the Null Ideology Effect

A finding of no association between ideology and HPT scores is theoretically informative, but several alternative mechanisms could account for the null result independently of the instrument's validity. We consider four such explanations.

**Floor effect hypothesis.** The most parsimonious alternative is that Czech adolescents simply did not endorse right-authoritarian items strongly enough to generate meaningful ideological variance. With FR-LF means below the scale midpoint (*M* = 2.48, *SD* = 0.75) and scores concentrated in the lower range, there may not have been enough "true believers" in the sample to test whether extreme ideology inflates HPT scores. On this account, we have not so much tested ideological contamination as demonstrated its absence in a low-ideology general population. We address this interpretation partly by design — the tertile-split analyses deliberately maximized the contrast between groups — and partly by noting that the sensitivity analyses found no ideology effect even within the most ideologically polarized subgroup comparison available in the data. Nevertheless, this interpretation underscores that our results should be read as boundary conditions for mainstream democratic classrooms, not as evidence that the instrument would be uncontaminated under conditions of genuine political extremism.

**Moral inhibition override hypothesis.** A second possibility is that even students with authoritarian-leaning attitudes suppress ideological responses when completing what appears to be a school assessment. Social desirability could mask ideological sympathy: students may recognize that endorsing extremist historical reasoning is socially unacceptable and respond in line with perceived norms rather than personal attitudes. This interpretation is partially contradicted by the data: social desirability was unrelated to HPT scores (*r* = -.05 to .01), and the null ideology effect survived the exclusion of high social desirability responders. The weak negative correlation between SDR-5 and FR-LF (*r* = -.19) also suggests some tendency toward ideologically inhibited responding, but the effect is small and does not account for the ideology–HPT relationship.

**Task constraint hypothesis.** The HPT scenario may structure the response options in ways that constrain ideological expression regardless of students' attitudes. When a scenario systematically presents contextual information — historical conditions, social pressures, economic circumstances — students may find it difficult to respond in purely presentist terms even if they wished to, because the scenario itself supplies the contextual scaffolding for higher-level responses. On this reading, the instrument is "contamination-resistant by design" rather than merely free of contamination by accident. This possibility is consistent with the theoretical rationale for scenario-based HPT assessment and is not incompatible with the validity claim, but it highlights that scenario features (not just student reasoning) may partly determine scores.

**Cognitive load hypothesis.** Finally, HPT performance may depend primarily on the inferential effort required to integrate multiple contextual cues, rather than on any form of ideological sympathy or inhibition. If the cognitive demands of contextualizing a historical scenario occupy working memory resources sufficiently to prevent ideology-congruent responding, the null finding would reflect a cognitive mechanism rather than a measurement one. This account aligns with the predominance of knowledge effects: domain knowledge reduces cognitive load in processing the scenario, allowing students to engage with the contextual cues more deeply. Ideological attitudes, which are orthogonal to this processing capacity, would then have negligible impact on scores.

None of these alternatives can be definitively ruled out with the present data. Their co-existence does not undermine the practical conclusion — that the instrument shows no measurable ideological contamination in this sample — but it does caution against strong mechanistic interpretations of the null result.

### Exploratory Finding: Authoritarianism and Presentism

*Note: The analysis reported in this subsection was not preregistered. It is reported for transparency and theoretical interest, and should not be treated as confirmatory. The effect (p = .016) does not survive Bonferroni correction for the 12 predictor × outcome combinations tested in the main models (adjusted α = .004), further underscoring its exploratory status.*

One finding in the data falls outside the preregistered analyses yet is theoretically informative. KSA-3 (authoritarianism) was significantly and negatively associated with reversed presentism (POP_rev: β = −0.159, *SE* = 0.065, 95% CI [−0.288, −0.031], *p* = .016), indicating that more authoritarian students scored slightly *lower* on POP_rev — that is, they were somewhat more presentist. This effect was not statistically significant for CONT or the primary HPT_CTX6 composite.

This finding is directionally opposite to what construct contamination would predict (which would require *higher* HPT scores for authoritarian students, not lower). Instead, it is consistent with the competing *rigidity* pathway articulated in the Introduction: students with higher authoritarian attitudes — characterized in the literature by reduced integrative complexity, need for cognitive closure, and resistance to ambiguity (Duckitt, 2001; Suedfeld & Tetlock, 1977) — may find it genuinely more difficult to set aside present-day moral frameworks when reasoning about historical actors. The "unnatural act" of historical perspective taking may be specifically demanding for authoritarian cognitive styles.

This interpretation has two important implications. First, it reinforces the discriminant validity of the instrument: ideology does not inflate HPT performance through sympathy, but certain authoritarian cognitive characteristics may be associated with genuine difficulty in the perspective-taking task. Second, it suggests that a truly comprehensive model of ideology–HPT relationships should test both pathways simultaneously, as they may produce opposing effects that cancel in aggregate analyses. The present data provides initial evidence consistent with the rigidity pathway, but a confirmatory test would require preregistered directional predictions and ideally a direct measure of cognitive closure or integrative complexity.

### Implications for Assessment

These findings have several implications for the design and interpretation of historical thinking assessments.

First, the results provide **positive validity evidence** for the Hartmann and Hasselhorn (2008) HPT instrument. Despite the morally charged content of the Weimar Republic scenario, students' ideological attitudes did not systematically inflate their scores. This means that the instrument can be used with reasonable confidence across ideologically diverse student populations without the concern that observed differences in HPT scores reflect ideological contamination rather than genuine differences in historical reasoning.

Second, the robustness of full scalar invariance is noteworthy. The invariance of item thresholds across ideology groups means that not only do the items load equivalently, but the response categories carry the same meaning for students with different ideological attitudes. This is the strongest form of measurement invariance and is a necessary condition for meaningful mean comparisons across groups (AERA et al., 2014). Assessment developers working on historical thinking instruments can take the present study as a model for how to empirically evaluate whether scenario content introduces CIV.

Third, the predominance of knowledge as a predictor underscores an often-overlooked feature of performance-based historical reasoning assessments: they are, by design, confounded with domain knowledge (Seixas et al., 2015). This is not a flaw — HPT genuinely requires knowledge — but it means that HPT scores should be interpreted with reference to students' knowledge base. Assessment contexts that wish to isolate "pure" reasoning from knowledge will face fundamental challenges with scenario-based instruments, a point previously raised by Smith (2017) and Breakstone et al. (2014).

### Implications for History Education

Beyond psychometrics, the findings contribute to ongoing debates about the relationship between historical thinking and ideological attitudes. A common concern in history education — sometimes explicit, sometimes implicit — is that teaching students to understand past actors' perspectives might inadvertently legitimize morally objectionable positions. The present study offers indirect reassurance: students' capacity to contextualize historical decisions operates independently of their own ideological commitments. Students with right-authoritarian attitudes were no better and no worse at historical perspective taking than their peers, suggesting that the cognitive process of historical contextualization is genuinely distinct from ideological sympathy.

This finding resonates with Lee and Ashby's (2001) foundational distinction between understanding and endorsement: HPT involves rationally reconstructing why an agent's actions made sense in context, not agreeing with those actions. Our results suggest that adolescents in our sample, on average, maintain this distinction in practice — their personal attitudes do not bleed into their reasoning about historical actors.

### Limitations

Several limitations warrant discussion. First, the study is based on a single country (Czech Republic) and a single HPT scenario (Weimar Republic). The generalizability of the null finding to other national contexts, historical topics, and student populations remains an open question. The Weimar scenario has specific content features — including reference to political radicalization in a European context — that may interact differently with ideological attitudes in countries with different historical experiences or political cultures.

Second, the ideological range represented in the sample is limited. FR-LF scores were concentrated below the scale midpoint, with positively skewed distributions reflecting the rarity of strong right-authoritarian endorsement in mainstream Czech classrooms. This constitutes a form of restriction of range that will attenuate observed correlations and reduces the generalizability of the null finding to contexts with genuinely extreme ideological polarization. The present study is best understood as testing ideological contamination under the conditions typical of a democratic public school system — that is, a stress-test of the *ordinary classroom*, not of a classroom populated by ideologically committed students. Whether the null finding holds in samples with substantially higher ideology variance, in schools embedded in contexts of political radicalization, or with students recruited on the basis of ideological commitment, remains an open empirical question that the present design cannot answer.

Third, the reliability of the HPT subscales is modest (α = .47–.64). While this is expected for three-item subscales and is comparable to values reported in the literature, low reliability attenuates observed correlations and may have reduced our ability to detect small effects. However, the pattern of results — with knowledge showing robust effects despite the same reliability constraint — argues against attenuation as an explanation for the null ideology finding. If attenuation were the primary issue, knowledge effects would be similarly suppressed.

Fourth, the sample size (*N* = 285–293 across 10 schools) provides adequate statistical power for detecting moderate effects in multilevel models, but limits sensitivity to small effects. Based on the model structure and observed variances, the minimum detectable standardized ideology coefficient at 80% power is approximately β ≈ 0.12–0.14. The ideology coefficients observed across all model specifications ranged from −0.006 to 0.026, indicating point estimates that are well within the undetectable range for very small effects. While the 95% confidence intervals consistently exclude moderate effects (ruling out |β| > ~0.14), small effects of |β| < 0.08 cannot be excluded with confidence. Researchers should note that even a true effect of this magnitude would be substantively negligible for most practical purposes, but this caveat should be borne in mind when interpreting the null result.

For the DIF analyses specifically, splitting N ≈ 250 usable observations into Low and High tertile groups yields roughly n ≈ 80 per group — a modest sample for multi-group IRT with 9 items scored on a 1–4 scale. Non-significant DIF therefore reflects both the absence of flagged items and the limited sensitivity to detect weak, item-level differential functioning. The null DIF result is consistent with invariance but should not be equated with proof that no DIF exists for items with small group-specific parameter differences.

Fifth, the ideology measures capture explicit self-reported attitudes and may not capture implicit ideological orientations that could affect responses to historically charged content. Future research might complement self-report measures with implicit association tests or response-time-based measures to provide a more comprehensive assessment of the ideology–HPT relationship. Additionally, because all study variables were collected via self-report in a single administration session, the possibility of common method variance cannot be entirely excluded. The absence of significant social desirability effects provides partial reassurance — if method-driven responding were inflating associations, the SDR-5 predictor would be expected to show consistent associations with HPT scores, which it did not — but latent method effects cannot be ruled out without multi-method designs.

Sixth, the model controls for historical knowledge and social desirability but does not account for additional student-level variables that could confound the ideology–HPT relationship, including general cognitive ability, academic achievement, socioeconomic background, or family political socialization. The consistency of the null ideology finding across all sensitivity specifications reduces the plausibility of simple confounding by unmeasured variables, but future studies should include richer covariate sets to further rule out omitted variable bias. Random intercepts for school and classroom partially absorb unobserved contextual heterogeneity, but individual-level confounds remain unaddressed.

Seventh, this study examines a closed-ended Likert-based HPT instrument. The findings may not generalize to open-ended or constructed-response HPT assessments (e.g., essays, think-alouds), where the relationship between ideology and performance could operate through different mechanisms, such as argument framing or source selection.

### Future Directions

Several lines of investigation follow from the present results. Cross-national replications using the same instrument and analytical framework would establish whether the null ideology finding is culturally robust. Studies using different historical scenarios — particularly those involving ideological content more proximal to contemporary political debates — would test the boundaries of the present finding. Combining the present approach with qualitative methods (e.g., cognitive interviews, think-aloud protocols) could reveal whether the *process* of perspective taking differs across ideology groups even when *products* (scale scores) do not.

Finally, the strong knowledge effect identified here invites further investigation into the knowledge–reasoning nexus in HPT. Experimental or quasi-experimental designs that manipulate knowledge input (e.g., through instruction or reading materials) and examine downstream effects on HPT scores would strengthen causal claims about the role of domain knowledge in historical perspective taking.

### Conclusion

The present study provides robust empirical evidence that the Hartmann and Hasselhorn (2008) HPT instrument does not exhibit detectable ideological contamination under the conditions examined: a Czech mainstream school sample, the Weimar Republic scenario, and the range of right-authoritarian attitudes typical of a democratic classroom context. Students' right-authoritarian attitudes were unrelated to their HPT scores across bivariate, multilevel, and item-level analyses, and this null finding was stable across multiple sensitivity checks. The instrument demonstrates full scalar measurement invariance across ideology groups, and no items exhibited differential functioning of detectable magnitude. Historical knowledge, not ideology, was the consistent driver of HPT performance — a pattern that supports both the construct validity of the instrument and the theoretical conceptualization of HPT as a knowledge-dependent disciplinary competence. These findings advance understanding of how disciplinary historical reasoning is measured under conditions of ideological heterogeneity, demonstrating that the epistemic demands of contextualisation are distributed fairly across students with diverse political commitments in mainstream democratic classrooms. The results clarify the boundary conditions of HPT instrument validity — its defensible domain of use — and establish an empirical foundation for investigating what occurs at the edges of this boundary: greater ideological extremity, different historical scenarios, or educational contexts where political polarization is more pronounced.

---

## Acknowledgments

The author thanks the participating schools, teachers, and students for making this research possible, and acknowledges the HistoryLab team at Charles University for consultation on instrument adaptation and curriculum alignment.

---

## Disclosure of interest

The author reports there are no competing interests to declare.

---

## References

AERA, APA, & NCME. (2014). *Standards for educational and psychological testing*. American Educational Research Association.

Älven, F. (2019). Bias in teachers' assessments of students' historical narratives. *History Education Research Journal*, *16*(2), 306–321. https://doi.org/10.18546/HERJ.16.2.10

Ashby, R., & Lee, P. (1987). Children's concepts of empathy and understanding in history. In C. Portal (Ed.), *The history curriculum for teachers* (pp. 62–88). Falmer Press.

Badham, L., Meadows, M., & Baird, J.-A. (2025). Construct comparability and the limits of post hoc modeling: Insights from International Baccalaureate multi-language assessments. *Frontiers in Education*, *10*, 1–20. https://doi.org/10.3389/feduc.2025.1616879

Beierlein, C., Asbrock, F., Kauff, M., & Schmidt, P. (2014). *Die Kurzskala Autoritarismus (KSA-3): Ein ökonomisches Messinstrument zur Erfassung dreier Subdimensionen autoritärer Einstellungen* [GESIS Working Papers 2014/35]. GESIS – Leibniz-Institut für Sozialwissenschaften.

Breakstone, J., Smith, M., & Wineburg, S. (2013). Beyond the bubble in history/social studies assessments. *Phi Delta Kappan*, *94*(5), 53–57. https://doi.org/10.1177/003172171309400512

Breakstone, J. (2014). Try, try, try again: The process of designing new history assessments. *Theory & Research in Social Education*, *42*(4), 453–485.

Chapman, A. (2021). Introduction: Historical knowing and the "knowledge turn." In A. Chapman (Ed.), *Knowing history in schools: Powerful knowledge and the powers of knowledge* (pp. 1–31). UCL Press.

Chen, F. F. (2007). Sensitivity of goodness of fit indexes to lack of measurement invariance. *Structural Equation Modeling*, *14*(3), 464–504.

Cheung, G. W., & Rensvold, R. B. (2002). Evaluating goodness-of-fit indexes for testing measurement invariance. *Structural Equation Modeling*, *9*(2), 233–255.

Cinatl, K., Pinkas, J., Pycha, Č., & Najbert, J. (2021). Historická gramotnost v aplikaci HistoryLab [Historical literacy in the HistoryLab application]. *Historie – Otázky – Problémy*, *13*(2), 155–173.

Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Lawrence Erlbaum Associates.

Decker, O., Kiess, J., & Brähler, E. (2013). *Rechtsextremismus der Mitte: Die Leipziger Mitte-Studie 2012*. Springer VS.

Duckitt, J. (2001). A dual-process cognitive-motivational theory of ideology and prejudice. *Advances in Experimental Social Psychology*, *33*, 41–113. https://doi.org/10.1016/S0065-2601(01)80004-6

Ercikan, K., & Seixas, P. (2015). Introduction: The new shape of history assessments. In K. Ercikan & P. Seixas (Eds.), *New directions in assessing historical thinking* (pp. 1–14). Routledge.

Finch, W. H. (2005). The MIMIC model as a method for detecting DIF: Comparison with Mantel-Haenszel, SIBTEST, and the IRT likelihood ratio. *Applied Psychological Measurement*, *29*(4), 278–295. https://doi.org/10.1177/0146621605275728

Hartmann, U., & Hasselhorn, M. (2008). Historical perspective taking: A standardized measure for an aspect of students' historical thinking. *Learning and Individual Differences*, *18*(2), 264–270. https://doi.org/10.1016/j.lindif.2007.10.002

Hays, R. D., Hayashi, T., & Stewart, A. L. (1989). A five-item measure of socially desirable response set. *Educational and Psychological Measurement*, *49*(3), 629–636. https://doi.org/10.1177/001316448904900315

Huijgen, T., van Boxtel, C., van de Grift, W., & Holthuis, P. (2014). Testing elementary and secondary school students' ability to perform historical perspective taking: The construction of valid and reliable measure instruments. *European Journal of Psychology of Education*, *29*(4), 653–672. https://doi.org/10.1007/s10212-014-0219-4

Huijgen, T., van Boxtel, C., van de Grift, W., & Holthuis, P. (2017). Toward historical perspective taking: Students' reasoning when contextualizing the actions of people in the past. *Theory & Research in Social Education*, *45*(1), 110–144. https://doi.org/10.1080/00933104.2016.1208597

Jost, J. T., Glaser, J., Kruglanski, A. W., & Sulloway, F. J. (2003). Political conservatism as motivated social cognition. *Psychological Bulletin*, *129*(3), 339–375. https://doi.org/10.1037/0033-2909.129.3.339

Kahan, D. M., Peters, E., Dawson, E. C., & Slovic, P. (2012). Motivated numeracy and enlightened self-government. *Behavioural Public Policy*, *1*(1), 54–86. https://doi.org/10.1017/bpp.2016.2

Lakens, D. (2017). Equivalence tests: A practical primer for t tests, correlations, and meta-analyses. *Social Psychological and Personality Science*, *8*(4), 355–362. https://doi.org/10.1177/1948550617697177

Lee, P., & Ashby, R. (2001). Empathy, perspective taking, and rational understanding. In O. L. Davis, E. A. Yeager, & S. Foster (Eds.), *Historical empathy and perspective taking in the social studies* (pp. 21–50). Rowman & Littlefield.

Maas, C. J. M., & Hox, J. J. (2005). Sufficient sample sizes for multilevel modeling. *Methodology*, *1*(3), 86–92. https://doi.org/10.1027/1614-2241.1.3.86

Messick, S. (1995). Validity of psychological assessment: Validation of inferences from persons' responses and performances as scientific inquiry into score meaning. *American Psychologist*, *50*(9), 741–749. https://doi.org/10.1037/0003-066x.50.9.741

Mundlak, Y. (1978). On the pooling of time series and cross section data. *Econometrica*, *46*(1), 69–85. https://doi.org/10.2307/1913646

Pinkas, J. (2024). Badatelsky orientovaná výuka v projektu Dějepis+ [Inquiry-based teaching in the Dejepis+ project]. In *Konference česko-polské učebnice* (proceedings). Charles University.

Seixas, P., & Morton, T. (2013). *The big six historical thinking concepts*. Nelson Education.

Seixas, P., Gibson, L., & Ercikan, K. (2015). A design process for assessing historical thinking: The case of a one-hour test. In K. Ercikan & P. Seixas (Eds.), *New directions in assessing historical thinking* (pp. 102–117). Routledge.

Seixas, P. (2017). Historical consciousness and historical thinking. In M. Carretero, S. Berger, & M. Grever (Eds.), *Palgrave handbook of research in historical culture and education* (pp. 59–72). Palgrave Macmillan. https://doi.org/10.1057/978-1-137-52908-4_3

Selman, R. L. (1980). *The growth of interpersonal understanding: Developmental and clinical analyses*. Academic Press.

Smith, M. (2017). Cognitive validity: Can multiple-choice items tap historical thinking processes? *American Educational Research Journal*, *54*(6), 1256–1287.

Suedfeld, P., & Tetlock, P. E. (1977). Integrative complexity of communications in international crises. *Journal of Conflict Resolution*, *21*(1), 169–184. https://doi.org/10.1177/002200277702100108

van Boxtel, C., & van Drie, J. (2018). Historical reasoning: Conceptualizations and educational applications. In S. A. Metzger & L. M. Harris (Eds.), *The Wiley international handbook of history teaching and learning* (pp. 149–176). Wiley. https://doi.org/10.1002/9781119100812.ch6

van Drie, J., & van Boxtel, C. (2008). Historical reasoning: Towards a framework for analyzing students' reasoning about the past. *Educational Psychology Review*, *20*(2), 87–110. https://doi.org/10.1007/s10648-007-9056-1

Wineburg, S. (2001). *Historical thinking and other unnatural acts: Charting the future of teaching the past*. Temple University Press.

Wineburg, S. (2018). *Why learn history (when it's already on your phone)*. University of Chicago Press.

---

## Appendix: Figure Notes

**Figure 2.** Measurement invariance and DIF across ideological groups. Panel A: Standardized factor loadings (with 95% CIs) from a configural three-factor MG-CFA (WLSMV estimator; Low vs. High ideology tertiles). Panel B: Item discrimination (*a*) and average location parameters from unconstrained multi-group graded response models. Points near the identity line indicate equivalent item functioning. No items were flagged for DIF (α = .01, Bonferroni-adjusted). Generated by `trse_outputs/fig02_invariance_and_dif.R`.

**Figure 3.** Score distributions for HPT outcomes (HPT_CTX6, Contextualisation, Presentism reversed), ideology predictors (FR-LF total, KSA-3 total), and historical knowledge (KN Total). Histograms with overlaid density curves; vertical dashed lines indicate scale midpoints. Note the positive skew of ideology distributions, reflecting the rarity of strong right-authoritarian endorsement in mainstream Czech classrooms. Generated by `trse_outputs/fig03_score_distributions.R`.

**Figure 4.** Forest plot of standardized fixed-effect coefficients (β) with 95% confidence intervals from multilevel models (random intercepts for school and class) predicting three HPT outcomes: CTX6 (primary), Contextualisation, and Presentism (reversed). All predictors z-standardized. Points show point estimates; error bars show 95% CIs. Orange = *p* < .05; blue = non-significant. *N* = 285. Generated by `trse_outputs/fig04_coefficient_plot.R`.

**Figure 5.** Predicted marginal effects of FR-LF ideology on HPT Contextualisation. Lines show population-level predicted HPT Contextualisation (1–4 scale) from multilevel models (random intercepts for school and class). H1 model includes ideology only; H2 adds historical knowledge, social desirability, and KSA-3. Shaded bands = 95% CIs for fixed effects. Rug plot shows the observed distribution of FR-LF scores. Controls held at sample means. Generated by `trse_outputs/fig05_marginal_effects.R`.
