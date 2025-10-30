// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  pagenumbering: "1",
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: pagenumbering,
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black or heading-decoration == "underline"
           or heading-background-color != none) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)
#import "@preview/fontawesome:0.5.0": *

#show: doc => article(
  title: [LIQPLAT Statistical Analysis Plan],
  authors: (
    ( name: [Johannes Schwenke],
      affiliation: [],
      email: [] ),
    ),
  date: [2025-10-27],
  margin: (x: 2cm,y: 2.5cm,),
  paper: "a4",
  sectionnumbering: "1.1.1.1",
  pagenumbering: "1",
  toc: true,
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

#pagebreak()
= Administrative information
<administrative-information>
== Revision history
<revision-history>
#table(
  columns: (19.18%, 19.18%, 34.25%, 27.4%),
  align: (left,left,left,left,),
  table.header([Version], [Date], [Who], [Comments],),
  table.hline(),
  [0.1], [2024-09-20], [Johannes Schwenke], [First draft (Original SAP).],
  [0.2], [\[Date TBD\]], [Johannes Schwenke, Giusi Moffa], [Consolidated draft based on outcome-specific plans.],
)
== Roles and responsibilities
<roles-and-responsibilities>
#table(
  columns: (22.22%, 40.28%, 37.5%),
  align: (left,left,left,),
  table.header([Name], [Affiliation], [Role],),
  table.hline(),
  [Giusi Moffa], [University of Basel], [Trial statistician],
  [Johannes Schwenke], [University of Basel and University Hospital of Basel], [PhD Student],
  [Benjamin Kasenda], [University Hospital of Basel], [Principal investigator and Sponsor-Investigator],
  [Matthias Briel], [University of Basel and University Hospital of Basel], [TBD],
)
== Abbreviations
<abbreviations>
#table(
  columns: (23.61%, 76.39%),
  align: (left,left,),
  table.header([Abbreviation], [Full Term],),
  table.hline(),
  [#strong[ATE];], [Average Treatment Effect],
  [#strong[BSC];], [Best Supportive Care],
  [#strong[CDWH];], [Clinical Data Warehouse],
  [#strong[CHIP];], [Clonal Hematopoiesis of Indeterminate Potential],
  [#strong[ctDNA];], [Circulating Tumor DNA],
  [#strong[DAG];], [Directed Acyclic Graph],
  [#strong[ECOG];], [Eastern Cooperative Oncology Group],
  [#strong[EHR];], [Electronic Health Records],
  [#strong[EORTC];], [European Organisation for Research and Treatment of Cancer],
  [#strong[GRC];], [General Research Consent],
  [#strong[HR];], [Hazard Ratio],
  [#strong[IE];], [Intercurrent Event],
  [#strong[IQR];], [Interquartile Range],
  [#strong[ITT];], [Intention-to-Treat],
  [#strong[KM];], [Kaplan-Meier],
  [#strong[LOO-CV];], [Leave-One-Out Cross-Validation],
  [#strong[MAR];], [Missing At Random],
  [#strong[MCAR];], [Missing Completely at Random],
  [#strong[MCMC];], [Markov Chain Monte Carlo],
  [#strong[MICE];], [Multiple Imputation by Chained Equations],
  [#strong[NA];], [Not Applicable / Nelson-Aalen],
  [#strong[OR];], [Odds Ratio],
  [#strong[OS];], [Overall Survival],
  [#strong[PFS];], [Progression-Free Survival],
  [#strong[PMM];], [Predictive Mean Matching],
  [#strong[PO];], [Proportional Odds],
  [#strong[PROMs];], [Patient Reported Outcome Measures],
  [#strong[psATE];], [Principal Stratum Average Treatment Effect],
  [#strong[QLQ-C15];], [Quality of Life Questionnaire - Core 15],
  [#strong[QLQ-C30];], [Quality of Life Questionnaire - Core 30],
  [#strong[QoL];], [Quality of Life],
  [#strong[rcs];], [Restricted Cubic Splines],
  [#strong[RCT];], [Randomized Controlled Trial],
  [#strong[RMST];], [Restricted Mean Survival Time],
  [#strong[SAP];], [Statistical Analysis Plan],
  [#strong[SAT];], [Single-Arm Trial],
  [#strong[SE];], [Standard Error],
  [#strong[SoC];], [Standard of Care],
  [#strong[SOP];], [State Occupancy Probability],
  [#strong[TOH];], [Time Out of Hospital],
)
#pagebreak()
= Introduction
<introduction>
The purpose of this Statistical Analysis Plan (SAP) is to provide a detailed description of the intended statistical analyses for the LIQPLAT trial. The SAP is based on the protocol version 1.1.

We developed this SAP using a data set based on routinely collected data of patients who sought care prior to LIQPLAT, and who fulfill the eligibility criteria. These data are complemented with simulations where needed.

= Background and rationale
<background-and-rationale>
Liquid biopsies, particularly circulating tumor DNA (ctDNA) analysis, offer potential solutions to challenges in treating patients with solid cancers. ctDNA may provide a more comprehensive view of tumor heterogeneity than traditional biopsies, aiding in identifying targetable alterations, quantifying disease burden, detecting early treatment resistance, and predicting outcomes. While promising, more robust trial data on the implementation of ctDNA in routine care is needed. We therefore conduct a single-arm trial (SAT) to investigate ctDNA implementation in routine care for advanced cancer patients, using random invitation to ensure representative sampling and enhance result applicability.

= Objectives
<objectives>
As per protocol LIQPLAT v. 1.1 has two primary objectives:

+ To assess the implementation and feasibility of ctDNA measurements from peripheral blood during routine clinical care of cancer patients in the University Hospital Basel.
+ To compare clinical and patient-reported outcomes of patients from this trial with patients who did not receive systematic ctDNA measurement.

This SAP focuses primarily on the second objective, detailing the analyses for the three primary outcomes of the randomized comparison: #strong[Overall Survival (OS)];, #strong[Quality of Life (QoL)];, and #strong[Time Out of Hospital (TOH)];.

= Study methods
<study-methods>
== Trial design
<trial-design>
This is a single-arm, single-center trial at the University Hospital of Basel. Patients are randomly invited from an ongoing research registry for participation in the trial (#ref(<fig-trial-design>, supplement: [Figure])). The ongoing research registry is made up of all patients who have signed the general research consent (GRC) of the University Hospital Basel.

Based on past-data of the Division of Medical Oncology, around 240-300 patients would be eligible for the trial over a period of 18 months. However, for logistic reasons and capacity of the Department of Pathology, we limit the sample size to 150 participants, with a maximum of three patients selected for invitation to the trial per week.

#figure([
#box(image("img/liqplat-flow.png"))
], caption: figure.caption(
position: bottom, 
[
Patients are randomly selected for invitation to the single-arm trial (SAT) from a prospective research registry. Some eligible participants might not be considered for selection for operational reason, e.g., inpatients who require emergency treatment before trial staff was made aware of their existence. All outcome data are obtained from routinely collected data. The design enables an unconfounded (randomized) comparison of patients who were selected for invitation to the SAT with patients who were not selected for invitation.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-trial-design>


== Trial population
<sec-eligibility>
=== Inclusion criteria
<inclusion-criteria>
+ All adult patients who are part of the University Hospital Basel prospective research registry, i.e., have signed the GRC, with
+ A proven advanced solid malignant disease, and
+ An indication for medical anti-cancer treatment (including combined chemo-radiotherapy or immuno-radiotherapy).

=== Exclusion criteria
<exclusion-criteria>
+ Patients with primary brain tumors,
+ Patients with primary resectable disease, and
+ Patients with prior treatment for advanced or metastatic disease.

== Random Invitation to the SAT
<random-invitation-to-the-sat>
=== Screening list
<screening-list>
We build a weekly screening list containing all patients who have an appointment for a first consultation at the Division of Medical Oncology in the following week in REDCap (v. 14.3.14), using routinely collected data from the electronic patient records. The list is randomly ordered by sorting a hashed version (SHA-256) of the sequentially generated patient hospital ID. The trial team (at least one oncologist) checks whether patients fulfill the eligibility criteria every Friday afternoon.

=== Invitation to SAT
<invitation-to-sat>
Descending the screening list row by row, we select up to three patients per week who will be invited to the trial. The decision to invite is made randomly, using a random sampling table uploaded to REDCap. The random sampling table was generated by a trialist not associated with the trial using R version 4.3.1 (2023-06-16). The table was stratified by presence or absence of a primary diagnosis of a lung tumor, with a block size of 9, and has odds of 2:1 for an invitation to the trial. All investigators are blinded to the random sampling table.

Once three patients have been randomly selected for an invitation to the trial, the list is closed for the current week and a new one is started the next week. Inpatients can also be invited when fulfilling eligibility criteria and slots are available for the given week.

== Randomized comparison with external comparator
<randomized-comparison-with-external-comparator>
Randomly inviting patients from a prospective research registry to the SAT enables a randomized comparison with patients who were not invited to the trial, as the data on outcomes for the latter can be obtained from routine data. As visualized in #strong[?\@fig-dag];, invitation to the trial and the outcomes of interest have no common causes, because the decision to select a patient for invitation to the SAT was made at random.

As all patients who are considered for LIQPLAT are part of the prospective research registry and have therefore agreed for their data to be used for research purposes, we can compare the outcomes of patients who were invited to the trial to those who were not invited. However, a patient's decision to accept the invitation to the SAT is not random and likely to be influenced by factors ($U$) which also causally affect the outcome. In other words, the patients who were selected for invitation to the SAT and patients who were not selected for invitation to the SAT are exchangeable regarding their potential outcomes @Rubin2005-wc, but participants of the SAT are not exchangeable with non-participants. This means that for the randomized comparison, the random selection for invitation to the SAT is considered as the treatment.

Even though LIQPLAT is not strictly speaking a randomized controlled trial (RCT), when considering selection for invitation to the trial as treatment, it can be analyzed analogously to an RCT. We will therefore specify and analyze comparative analyses in line with the estimand framework @Kahan2024-au, where we consider invitation to the SAT as the treatment.

#quarto_super(
kind: 
"quarto-float-fig"
, 
caption: 
[
Simplified causal diagram for the LIQPLAT trial with random selection for invitation to the SAT as Z, actual invitation I, acceptance of the invitation A, and outcome Y. U represents the unmeasured common causes of I, A and Y. S is an indicator of selection for into the population who accepted the invitation . The Z-Y association in the population who accepted the invitation (a restriction represented by the box around S in the second causal diagram) will be affected by selection bias unless all prognostic factors L are correctly identified and adjusted for.
]
, 
label: 
<fig-dags>
, 
position: 
bottom
, 
supplement: 
"Figure"
, 
subrefnumbering: 
"1a"
, 
subcapnumbering: 
"(a)"
, 
[
#grid(columns: 2, gutter: 2em,
  [
#block[
#figure([
#box(image("img/DAG1.drawio.svg"))
], caption: figure.caption(
position: bottom, 
[
Dag 1
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-dag1>


]
],
  [
#block[
#figure([
#box(image("img/DAG.drawio.svg"))
], caption: figure.caption(
position: bottom, 
[
Dag 2
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-dag2>


]
],
)
]
)
== Outcome data
<outcome-data>
All participants of LIQPLAT are part of the University Hospital's prospective research registry and have thus consented to further use of their routinely collected data for research purposes. We will obtain outcome data almost exclusively from routinely collected data from electronic health records (EHR), which are mirrored in the clinical data warehouse (CDWH), and REDCap databases of the Division of Medical Oncology.

Trial specific data, such as dates of random selection, are stored within REDCap database specific to LIQPLAT.

== Primary Endpoints for Randomized Comparison
<primary-endpoints-for-randomized-comparison>
This SAP focuses on the three primary endpoints for the randomized comparison between the group selected for invitation to the SAT and the external control group (not selected for invitation).

+ #strong[Overall Survival (OS):] Time from date of random selection until death due to any cause.
+ #strong[Quality of Life (QoL):] Patient-reported global quality of life measured longitudinally using EORTC questionnaires during routine care.
+ #strong[Time Out of Hospital (TOH):] Time spent alive and not hospitalized (including emergency room visits) during the follow-up period.

Secondary endpoints as described in the protocol (e.g., PFS, number of imaging exams) will be analyzed but detailed plans are TBD.

= Analysis Populations
<analysis-populations>
== Intention-to-Treat (ITT) Population
<intention-to-treat-itt-population>
The ITT population includes all patients from the prospective research registry who were eligible for LIQPLAT according to the criteria in #ref(<sec-eligibility>, supplement: [Section]). It comprises two groups defined by the random selection process:

+ #strong[Intervention Group (Selected for Invitation):] All eligible patients randomly selected for invitation to participate in the LIQPLAT single-arm trial (SAT).
+ #strong[External Control Group (Not Selected for Invitation):] All eligible patients who were considered for invitation but not randomly selected during the same recruitment period.

This population forms the basis for the primary analyses of Overall Survival and Time Out of Hospital.

== Principal Stratum Population (for QoL Analysis)
<principal-stratum-population-for-qol-analysis>
The primary analysis for the Quality of Life endpoint will be conducted on the #strong[Principal Stratum Population];. This population is defined as the subset of ITT patients who would have survived the entire 26-week (6-month) follow-up period, regardless of their assignment to be invited to the SAT or not. In practice, this population will be approximated by including all patients from the ITT population who were observed to be alive at the end of the 26-week follow-up. The rationale for restricting the QoL analysis to this Principal Stratum is detailed in #ref(<sec-qol-rationale-ps>, supplement: [Section]).

= Estimands and Analysis Plans for Primary Outcomes
<estimands-and-analysis-plans-for-primary-outcomes>
== Overall Survival (OS)
<overall-survival-os>
=== Estimand Definition
<estimand-definition>
#table(
  columns: (20.83%, 79.17%),
  align: (left,left,),
  table.header([Attribute], [Definition],),
  table.hline(),
  [#strong[Target Population];], [Adult (≥ 18 years) patients with advanced solid cancer eligible for LIQPLAT.],
  [#strong[Treatment Arms];], [(A=1): Selection for invitation to ctDNA-guided care (SAT). (A=0): No selection for invitation (External Control Group / SoC).],
  [#strong[Variable];], [Time-to-event: Time (in days) from random selection to death from any cause.],
  [#strong[Time Horizon];], [$t_(m a x) = 182$ days (6 months).],
  [#strong[Summary Measure];], [#strong[Difference in Restricted Mean Survival Time (RMST)] at $t_(m a x) = 182$ days.],
  [#strong[Intercurrent Events];], [See #ref(<sec-os-intercurrent-events>, supplement: [Section]).],
)
==== Formal Definition
<formal-definition>
We use the potential outcomes framework. Let $T_i^a$ be the potential time-to-death for individual $i$ under treatment assignment $a in { 0 \, 1 }$. The survival function under treatment $a$ is $S^a (t) = P (T^a > t)$.

The Restricted Mean Survival Time (RMST) under treatment $a$ up to the time horizon $t_(m a x) = 182$ days is the area under the survival curve:

$ upright("RMST")^a (t_(m a x)) = integral_0^(t_(m a x)) S^a (t) d t $

Our primary estimand for OS is the Average Treatment Effect (ATE) on the RMST at 182 days, denoted $tau_(O S)$:

$ tau_(O S) = upright("RMST")^1 (t_(m a x)) - upright("RMST")^0 (t_(m a x)) $

$tau_(O S)$ represents the average difference in days lived within the first 182 days comparing the group selected for invitation to the SAT versus the external control group.

=== Handling of Intercurrent Events (OS)
<sec-os-intercurrent-events>
#table(
  columns: (20.55%, 20.55%, 58.9%),
  align: (left,left,left,),
  table.header([Intercurrent Event], [Strategy], [Rationale],),
  table.hline(),
  [SAT not offered after selection], [Treatment Policy], [Analyze as selected (part of A=1). Preserves ITT principle; estimates effect of the #emph[strategy] of offering ctDNA-guided care.],
  [Patient declined participation in SAT], [Treatment Policy], [Analyze as selected (part of A=1). As above.],
  [Patient changes hospital / Lost to F/U], [Censoring (if alive)], [Death information is obtained via CDWH / administrative databases, minimizing informative censoring due to death ascertainment. If lost and known alive at last contact, standard survival censoring applies.],
  [Control patient receives ctDNA test], [Treatment Policy], [Analyze as not selected (part of A=0). Reflects real-world practice where ctDNA may be used outside the trial context.],
  [Discontinuation of ctDNA monitoring (SAT)], [Treatment Policy], [Analyze as selected (part of A=1). Adherence does not affect assignment group.],
)
=== Analysis Method (OS)
<analysis-method-os>
==== Statistical Model
<statistical-model>
We will use a Bayesian proportional hazards survival model. The hazard rate $h_i (t)$ for individual $i$ at time $t$ is modeled as:

$ h_i (t) = h_0 (t) dot.op exp (eta_i) $

Where:

- $h_0 (t)$ is the #strong[baseline hazard function];, modeled flexibly using #strong[M-splines] with 5 degrees of freedom: $ h_0 (t) = sum_(k = 1)^5 gamma_k dot.op M_k (t) $ $M_k (t)$ are non-negative M-spline basis functions, and $gamma_k$ are coefficients.
- $eta_i$ is the #strong[linear predictor];: $ eta_i = beta_0 + beta_(upright("tx")) dot.op upright("Treatment")_i + upright(bold(X))_i beta_(upright("covars")) $
  - $upright("Treatment")_i$ is the indicator for being selected for invitation (1 if selected, 0 otherwise). $beta_(upright("tx"))$ is the log-hazard ratio associated with selection.
  - $upright(bold(X))_i$ is a vector of baseline covariates included for precision: baseline ECOG performance status (`ecog_fstcnt`) and cancer diagnosis category (`diagnosis_cat`). $beta_(upright("covars"))$ are the corresponding coefficients.
  - #emph[Note: Inclusion of additional baseline covariates like albumin and C-reactive protein (modeled with splines) will be considered based on model diagnostics and convergence.]

==== Prior Specifications
<prior-specifications>
- #strong[Intercept (];$beta_0$): $beta_0 tilde.op upright("Normal") (0 \, 20)$ (Weakly informative).
- #strong[Treatment Effect (];$beta_(upright("tx"))$): $beta_(upright("tx")) tilde.op upright("Normal") (0 \, 0.55^2)$. This prior places approximately 80% probability mass on Hazard Ratios between 0.5 and 2.0, reflecting a belief that the intervention is unlikely to more than halve or double the hazard rate.
- #strong[Covariate Effects (];$beta_(upright("covars"))$): $beta_k tilde.op upright("Normal") (0 \, 2.5^2)$ (Default weakly informative prior in `rstanarm`).
- #strong[M-spline Coefficients (];$gamma$): $gamma tilde.op upright("Dirichlet") (upright(bold(1))_5)$ (Default prior in `rstanarm`, implying coefficients are positive and sum to 1).

==== Implementation
<implementation>
The model will be fitted using the `stan_surv` function from the `rstanarm` package in R. MCMC settings (iterations, chains, warmup) will follow standard practice (e.g., 4 chains, 2000 iterations post-warmup). Convergence will be assessed using $hat(R)$ and effective sample size diagnostics.

#block[
```r
# Example R code for model fitting (adjust covariates as needed)
# Assumes 'data' has columns: day, death (0/1), tx (0/1), ecog_fstcnt, diagnosis_cat

library(rstanarm)

# Define priors
tx_scale <- 0.55
default_scale <- 2.5
my_prior <- normal(
  location = c(0, rep(0, nlevels(data$ecog_fstcnt)-1), rep(0, nlevels(data$diagnosis_cat)-1)), # Adjust size based on factors
  scale = c(tx_scale, rep(default_scale, nlevels(data$ecog_fstcnt)-1 + nlevels(data$diagnosis_cat)-1))
)

# Fit the model
model_os <- stan_surv(
    formula = Surv(day, death) ~ tx + ecog_fstcnt + diagnosis_cat, # Add other covariates if used
    data = data,
    basehaz = "ms",
    basehaz_ops = list(df = 5),
    prior = my_prior,
    prior_intercept = normal(0, 20),
    prior_aux = dirichlet(rep(1, 5)), # Match df for dirichlet
    chains = 4,
    cores = 4, # Adjust based on system
    seed = 1234,
    iter = 4000 # Includes warmup
) 
```

]
==== Derivation of the Estimand
<derivation-of-the-estimand>
+ #strong[Generate Posterior Survival Curves:] Using the fitted model (`model_os`), generate posterior draws of the standardized survival curves for each treatment group ($a = 0$ and $a = 1$) up to $t_(m a x) = 182$ days. This involves predicting survival probabilities for each individual under both treatment scenarios and averaging these predictions over the sample's covariate distribution for each posterior draw. The `posterior_survfit` function in `rstanarm` with `standardise = TRUE` will be used.
+ #strong[Calculate RMST per Draw:] For each posterior draw, calculate the RMST for both treatment groups ($upright("RMST")^0$ and $upright("RMST")^1$) by integrating the respective survival curves from $t = 0$ to $t_(m a x) = 182$ days (e.g., using the trapezoidal rule on the discrete time points generated by `posterior_survfit`).
+ #strong[Calculate RMST Difference per Draw:] For each posterior draw, compute the difference $tau_(O S \, upright("draw")) = upright("RMST")_(upright("draw"))^1 - upright("RMST")_(upright("draw"))^0$.
+ #strong[Summarize Posterior:] The collection of $tau_(O S \, upright("draw"))$ values forms the posterior distribution of the primary estimand. Summarize this distribution using the posterior median and a 95% credible interval.

==== Simulation Study Summary (OS)
<simulation-study-summary-os>
A simulation study based on historical data was planned to evaluate the operating characteristics (Bias, Power, Type I error) of this analysis approach.

- #strong[Data Generation:] Based on a second-order Markov model fitted to daily health state data (`3-sim-data.qmd`). Datasets were generated under the null hypothesis (OR=1) and alternative hypotheses (e.g., OR=0.9, 0.8, 0.7) corresponding to specific RMST differences.
- #strong[Analysis:] The planned Bayesian proportional hazards model was fitted to simulated trial datasets (N=270, 2:1 allocation) (`4-power-09.qmd`, `6-estimand.qmd`).
- #strong[Results:] \[Specific results for bias, coverage, Type I error, and power under different effect sizes to be filled in based on simulation outputs.\]

== Quality of Life (QoL)
<quality-of-life-qol>
=== Rationale for Ordinal Longitudinal Modeling
<rationale-for-ordinal-longitudinal-modeling>
Quality of Life (QoL), measured using the EORTC QLQ-C30 global health status / QoL item (question 30), is an inherently ordinal outcome with 7 levels. Treating it as numeric is inappropriate @Liddell2018-zj. Furthermore, QoL changes over time, and data are collected at irregular intervals during routine care. A longitudinal model is required to capture the trajectory and handle irregular spacing. Death is a competing risk.

We will use a #strong[first-order Markov longitudinal ordinal transition model];. This model analyzes the probability of transitioning between QoL states from one week to the next, conditional on the previous state and the time gap since the last measurement. This approach accounts for the ordinal nature, longitudinal correlation, irregular measurement times, and provides a framework to derive interpretable summary measures like time spent in specific states.

=== Estimand Definition (QoL)
<estimand-definition-qol>
#table(
  columns: (18.06%, 81.94%),
  align: (left,left,),
  table.header([Attribute], [Definition],),
  table.hline(),
  [#strong[Target Population];], [The #strong[Principal Stratum] of adult (≥ 18 years) patients with advanced solid cancer eligible for LIQPLAT, defined as those patients who would survive the 26-week follow-up period #emph[irrespective] of their selection status for invitation to the SAT.],
  [#strong[Treatment Arms];], [(A=1): Selection for invitation to ctDNA-guided care (SAT). (A=0): No selection for invitation (External Control Group / SoC).],
  [#strong[Variable];], [Ordinal QoL state (7 levels, derived from EORTC QLQ-C30 Q30) measured longitudinally at irregular intervals. We use an inverted scale where 1='Excellent', …, 7='Very Poor'.],
  [#strong[Time Horizon];], [$t_(m a x) = 26$ weeks (6 months).],
  [#strong[Summary Measure];], [#strong[Difference in mean number of weeks spent with "Good" QoL] (defined as states 1, 2, or 3) during the 26-week period, within the Principal Stratum.],
  [#strong[Intercurrent Events];], [See #ref(<sec-qol-intercurrent-events>, supplement: [Section]).],
)
==== Formal Definition (QoL)
<formal-definition-qol>
Let $S_i^a$ be an indicator for survival of patient $i$ at 26 weeks under treatment assignment $a in { 0 \, 1 }$. $S_i^a = 1$ if patient $i$ survives, $S_i^a = 0$ otherwise. The Principal Stratum of "always-survivors" includes patients for whom $S_i^1 = S_i^0 = 1$.

Let $Y_(i t)^a$ be the potential ordinal QoL state (1-7, 1=best) for patient $i$ at week $t$ under treatment $a$. Let $W_i^a$ be the potential total number of weeks patient $i$ spends in a "Good" QoL state ($j in { 1 \, 2 \, 3 }$) under treatment $a$ over 26 weeks:

$ W_i^a = sum_(t = 1)^26 sum_(j = 1)^3 bb(I) (Y_(i t)^a = j) $

The primary estimand for QoL, the Principal Stratum Average Treatment Effect (psATE) on weeks in a "Good" state, denoted $tau_W^(upright("psATE"))$, is:

$ tau_W^(upright("psATE")) = bb(E) [W_i^1 \| S_i^1 = 1 \, S_i^0 = 1] - bb(E) [W_i^0 \| S_i^1 = 1 \, S_i^0 = 1] $

==== Identification Assumption (QoL)
<sec-qol-identification>
Identification of $tau_W^(upright("psATE"))$ by analyzing only those patients #emph[observed] to survive 26 weeks relies on the #strong[key assumption] that selection for invitation to the SAT has #strong[no effect on 26-week survival status];: For all patients $i$, $S_i^1 = S_i^0$. Under this assumption, the group observed to survive is equivalent to the "always-survivor" Principal Stratum.

==== Rationale for Principal Stratum (QoL)
<sec-qol-rationale-ps>
Modeling the QoL trajectory while incorporating the absorbing state of death proved problematic with sparse intermediate QoL measurements common in routine care. Our simulations indicated that interval-censoring approaches for missing QoL states led to biased estimates, particularly overestimating the probability of death (#ref(<sec-appendix-qol-censoring>, supplement: [Section])). Assuming no treatment effect on 6-month survival allows us to focus the QoL analysis on the stratum of patients who survive this period regardless of treatment, providing a potentially less biased estimate of the QoL effect #emph[for this subgroup];, conditional on the validity of the no-survival-effect assumption.

=== Handling of Intercurrent Events (QoL)
<sec-qol-intercurrent-events>
#table(
  columns: (15.28%, 15.28%, 69.44%),
  align: (left,left,left,),
  table.header([Intercurrent Event], [Strategy], [Rationale],),
  table.hline(),
  [#strong[Death];], [#emph[Principal Stratum];], [Death before 26 weeks defines exclusion from the primary analysis population. This relies on the assumption in #ref(<sec-qol-identification>, supplement: [Section]).],
  [Discontinuation of ctDNA monitoring (SAT)], [#emph[Treatment Policy];], [Analyze as selected (part of A=1). Preserves ITT principle; estimates the effect of the #emph[strategy] of offering ctDNA-guided care, irrespective of adherence.],
  [Switch to best supportive care (BSC)], [#emph[Treatment Policy] / #emph[Missing Data];], [Patient remains in their assigned group (A=1 or A=0). If BSC leads to cessation of QoL data collection, this contributes to missing data, handled via Multiple Imputation (see #ref(<sec-missing-data>, supplement: [Section])). The potential for informative missingness is a key limitation (see #ref(<sec-qol-limitations>, supplement: [Section])).],
  [Treatment at another hospital / LFU], [#emph[Missing Data];], [If QoL data collection stops, handled via Multiple Imputation under MAR assumption.],
  [SAT not offered after selection], [#emph[Treatment Policy];], [Analyze as selected (part of A=1).],
  [Patient declined participation in SAT], [#emph[Treatment Policy];], [Analyze as selected (part of A=1).],
  [Control patient receives ctDNA test], [#emph[Treatment Policy];], [Analyze as not selected (part of A=0).],
)
#block[
#callout(
body: 
[
#strong[Challenge: Informative Missingness due to Routine Care Data Collection] A major limitation is that QoL is measured during routine appointments. If the ctDNA strategy influences clinical decisions (e.g., earlier switch to BSC, different follow-up intensity), it affects #emph[both] the patient's true QoL #emph[and] the probability of observing QoL. This creates a potential for informative missingness that Multiple Imputation under MAR may not fully address. See #ref(<sec-qol-limitations>, supplement: [Section]).

]
, 
title: 
[
Warning
]
, 
background_color: 
rgb("#fcefdc")
, 
icon_color: 
rgb("#EB9113")
, 
icon: 
fa-exclamation-triangle()
, 
body_background_color: 
white
)
]
=== Analysis Method (QoL)
<analysis-method-qol>
==== Statistical Model: Bayesian First-Order Markov Ordinal Transition Model
<statistical-model-bayesian-first-order-markov-ordinal-transition-model>
Let $y_(i t)$ be the ordinal QoL state (1-7, 1=best) for patient $i$ at week $t$. Let $y_(i t')$ be the last observed state at a prior week $t'$, and let the time gap be $Delta t = t - t'$. The model is a cumulative logit model for transition probabilities:

$ "logit" (P (y_(i t) gt.eq j \| y_(i t'))) & = alpha_j + eta_(i t) + gamma_(i t \, j) &  & upright("for ") j = 2 \, dots.h \, 7\
eta_(i t) & = beta_(upright("tx")) dot.op upright("Treatment")_i + f (upright("week")_t) + sum_(k = 2)^7 beta_(upright("yprev") = k) dot.op bb(I) (y_(i t') = k)\
 & #h(2em) + beta_(upright("gap")) dot.op Delta t + sum_(k = 2)^7 beta_(upright("yprev") = k times upright("gap")) dot.op bb(I) (y_(i t') = k) dot.op Delta t + upright(bold(X))_i beta_(upright("covars"))\
gamma_(i t \, j) & = (tau_(upright("week")) dot.op upright("week")_t) dot.op j\
 $

Where:

- $alpha_j$: Category-specific intercepts (cutpoints).
- $eta_(i t)$: Linear predictor for effects assuming proportional odds (PO).
  - $beta_(upright("tx"))$: Main effect of treatment selection (A=1 vs A=0).
  - $f (upright("week")_t)$: Flexible function of study week (e.g., restricted cubic spline with 4 knots).
  - $beta_(upright("yprev") = k)$: Effect of the previous QoL state (categorical, state 1 as reference).
  - $beta_(upright("gap"))$: Linear effect of the time gap $Delta t$.
  - $beta_(upright("yprev") = k times upright("gap"))$: Interaction between previous state and time gap.
  - $upright(bold(X))_i beta_(upright("covars"))$: Effects of baseline covariates (ECOG status `ecog_fstcnt`, diagnosis category `diagnosis`).
- $gamma_(i t \, j)$: Models deviation from PO for the effect of time (`week`). We allow the effect of time to be non-proportional, constrained to be linear in the outcome category $j$. $tau_(upright("week"))$ is the non-proportional odds parameter for time.

==== Prior Specifications
<prior-specifications-1>
- #strong[Intercepts (];$alpha_j$): Priors induced by a Dirichlet(0.308) distribution on baseline cell probabilities (default in `rmsb::blrm`). Ensures ordering $alpha_2 < dots.h < alpha_7$.
- #strong[Coefficients (];$beta_k \, tau_(upright("week"))$): $upright("Normal") (0 \, 100^2)$ (Default weakly informative priors in `rmsb::blrm`).

==== Implementation
<implementation-1>
The model will be fitted using the `blrm` function from the `rmsb` package in R, restricted to the Principal Stratum population (survivors at 26 weeks). MCMC settings as per OS analysis.

#block[
```r
# Example R code for model fitting
# Assumes 'data_survivors' is the dataset filtered for 26-week survivors
# Assumes columns: y (QoL 1-7), tx (0/1), week, yprev (factor 1-7), gap, 
#                  ecog_fstcnt, diagnosis

library(rmsb)
library(rms) # Needed for rcs()

# Prepare data if necessary (e.g., create yprev, gap)
# ... data preparation code ...

# Ensure yprev is a factor with correct levels
# data_survivors$yprev <- factor(data_survivors$yprev, levels = 1:7) 

model_qol <- blrm(
  formula = y ~ tx + rcs(week, 4) + yprev * gap + ecog_fstcnt + diagnosis,
  data = data_survivors,
  ppo = ~week,             # Allow week effect to be non-proportional
  cppo = function(y) y,    # Constraint: non-proportionality linear in y
  iter = 4000, chains = 4, seed = 1234,
  # ... other MCMC settings ...
)
```

]
==== Derivation of the Estimand (QoL)
<derivation-of-the-estimand-qol>
+ #strong[Fit Model:] Fit the `model_qol` to each multiply imputed dataset restricted to the Principal Stratum.
+ #strong[Calculate Weekly SOPs:] For each posterior draw from the fitted model, calculate the State Occupancy Probabilities (SOPs), $P (Y_(i t)^a = j)$, for each week $t = 1 \, dots.h \, 26$, for each state $j = 1 \, dots.h \, 7$, and for each treatment arm $a in { 0 \, 1 }$. This involves marginalizing over the distribution of baseline covariates, baseline QoL states, and the Markov process transitions over time.
+ #strong[Calculate Expected Weeks in "Good" State per Draw:] For each posterior draw, calculate the expected number of weeks in a "Good" state (1, 2, or 3) for each treatment arm by summing the relevant SOPs over the 26 weeks: $ bb(E) [W^a]_(upright("draw")) = sum_(t = 1)^26 sum_(j = 1)^3 upright("SOP") (Y_(i t)^a = j)_(upright("draw")) $
+ #strong[Calculate Difference per Draw:] For each posterior draw, compute the difference $tau_(W \, upright("draw"))^(upright("psATE")) = bb(E) [W^1]_(upright("draw")) - bb(E) [W^0]_(upright("draw"))$.
+ #strong[Summarize Posterior:] The collection of $tau_(W \, upright("draw"))^(upright("psATE"))$ values forms the posterior distribution of the primary estimand for QoL. Summarize using posterior median and 95% credible interval.

==== Simulation Study Summary (QoL)
<simulation-study-summary-qol>
A simulation study based on historical data was conducted (`qol-sap.qmd`).

- #strong[Data Generation:] Based on a first-order Markov model fitted to historical oncology QoL data. Dataset included irregular follow-up and death as an absorbing state.
- #strong[Analysis Comparison:] Compared the proposed Markov model (on Principal Stratum) to alternatives: Ordinal Multilevel Model, Time-to-Deterioration, Cumulative Logit at Month 6, Change from Baseline. Models were fitted to simulated sparse datasets (85% missingness post-baseline).
- #strong[Results:] The Markov model and the Ordinal Multilevel model showed substantially higher Bayesian power (\~45%) compared to other methods (\<20%) while maintaining appropriate Type I error control (\~5%) when analyzing the Principal Stratum under the specific data generating mechanism used. \[Bias and coverage results TBD\].

== Time Out of Hospital (TOH)
<time-out-of-hospital-toh>
=== Rationale for Ordinal Longitudinal Modeling (TOH)
<rationale-for-ordinal-longitudinal-modeling-toh>
We define a patient's state weekly based on their location and vital status: 1=Alive and Out of Hospital, 2=Alive and In Hospital (Unplanned Admission/ER), 3=Alive and In Hospital (Planned Admission), 4=Alive and In Hospice/Palliative Care Unit, 5=Dead. This creates a longitudinal ordinal outcome (with state 5 being absorbing). Similar to QoL, data on location might be collected irregularly or be interval-censored between known points. A longitudinal state transition model is appropriate.

Based on model comparisons using historical data (`4-freq-fit-tests.qmd`), a #strong[second-order Markov model] showed a substantially better fit (lower AIC) than a first-order model, suggesting that the state two weeks prior provides additional predictive information beyond the immediately preceding week's state.

=== Estimand Definition (TOH)
<estimand-definition-toh>
#table(
  columns: (18.06%, 81.94%),
  align: (left,left,),
  table.header([Attribute], [Definition],),
  table.hline(),
  [#strong[Target Population];], [Adult (≥ 18 years) patients with advanced solid cancer eligible for LIQPLAT (ITT Population).],
  [#strong[Treatment Arms];], [(A=1): Selection for invitation to ctDNA-guided care (SAT). (A=0): No selection for invitation (External Control Group / SoC).],
  [#strong[Variable];], [Weekly health state: 1=Alive/Out-of-Hospital, 2=Alive/In-Hospital(Unplanned), 3=Alive/In-Hospital(Planned), 4=Alive/Hospice, 5=Dead. Derived from EHR/CDWH data.],
  [#strong[Time Horizon];], [$t_(m a x) = 26$ weeks (6 months).],
  [#strong[Summary Measure];], [#strong[Difference in mean number of weeks spent Alive and Out of Hospital] (State 1) during the 26-week period.],
  [#strong[Intercurrent Events];], [Primarily death, which is modeled as the absorbing state (State 5). Other events (e.g., LFU) handled via missing data/model assumptions. See #ref(<sec-os-intercurrent-events>, supplement: [Section]) for general treatment policy considerations.],
)
==== Formal Definition (TOH)
<formal-definition-toh>
Let $Y_(i t)^a$ be the potential health state (1-5) for individual $i$ at week $t$ under treatment assignment $a in { 0 \, 1 }$. State 1 represents "Alive and Out of Hospital".

Let $W_i^a$ be the potential total number of weeks individual $i$ spends alive and out of the hospital (State 1) under treatment $a$ over the 26-week period:

$ W_i^a = sum_(t = 1)^26 bb(I) (Y_(i t)^a = 1) $

Our primary estimand for TOH is the Average Treatment Effect (ATE) on this outcome, denoted $tau_(T O H)$:

$ tau_(T O H) = bb(E) [W_i^1] - bb(E) [W_i^0] $

$tau_(T O H)$ represents the population-average difference in weeks spent alive and out of the hospital over 26 weeks, comparing the group selected for invitation versus the external control group.

=== Handling of Intercurrent Events (TOH)
<handling-of-intercurrent-events-toh>
Death is explicitly modeled as the absorbing state (State 5) within the transition model. Other intercurrent events follow the Treatment Policy approach as outlined for OS (#ref(<sec-os-intercurrent-events>, supplement: [Section])). Missing state information between observed points will be handled implicitly by the Markov model's transition probabilities over the time gap, assuming the process is MAR conditional on the history and covariates.

=== Analysis Method (TOH)
<analysis-method-toh>
==== Statistical Model: Bayesian Second-Order Markov Ordinal Transition Model with Random Effects
<statistical-model-bayesian-second-order-markov-ordinal-transition-model-with-random-effects>
Let $y_(i t)$ be the ordinal health state (1-5) for patient $i$ at week $t$. Let $y_(i \, t - 1)$ and $y_(i \, t - 2)$ be the states at the previous two weeks. The model is a cumulative logit model:

$ "logit" (P (y_(i t) gt.eq j \| y_(i \, t - 1) \, y_(i \, t - 2))) & = alpha_j + eta_(i t) + gamma_(i t) &  & upright("for ") j = 2 \, dots.h \, 5\
eta_(i t) & = beta_(upright("tx")) dot.op upright("Treatment")_i + f (upright("week")_t)\
 & #h(2em) + sum_(k = 2)^4 beta_(upright("yprev") = k) dot.op bb(I) (y_(i \, t - 1) = k) + sum_(l = 2)^4 beta_(upright("ypprev") = l) dot.op bb(I) (y_(i \, t - 2) = l)\
 & #h(2em) + beta_(upright("week") times upright("yprev")) dot.op upright("week")_t dot.op bb(I) (y_(i \, t - 1) > 1) quad upright("[Simplified interaction indicator]")\
 & #h(2em) + beta_(upright("week") times upright("ypprev")) dot.op upright("week")_t dot.op bb(I) (y_(i \, t - 2) > 1) quad upright("[Simplified interaction indicator]")\
 & #h(2em) + upright(bold(X))_i beta_(upright("covars"))\
gamma_(i t) & = upright("gamma")_(0 i) quad upright("[Patient-specific random intercept]")\
 $

Where:

- $alpha_j$: Category-specific intercepts (cutpoints).
- $eta_(i t)$: Linear predictor.
  - $beta_(upright("tx"))$: Main effect of treatment selection (A=1 vs A=0).
  - $f (upright("week")_t)$: Flexible function of study week (e.g., restricted cubic spline with 4 knots).
  - $beta_(upright("yprev") = k)$, $beta_(upright("ypprev") = l)$: Effects of the state at week $t - 1$ and $t - 2$ (categorical, state 1 reference). #emph[Note: Summation upper limit is 4, as state 5 is absorbing.]
  - $beta_(upright("week") times upright("yprev"))$, $beta_(upright("week") times upright("ypprev"))$: Interaction terms allowing the effect of previous states to change over time (potentially simplified from full categorical interaction based on model fitting in `4-freq-fit-tests.qmd`).
  - $upright(bold(X))_i beta_(upright("covars"))$: Effects of baseline covariates (ECOG `ecog_fstcnt`, diagnosis `diagnosis`, albumin `albumin`, C-reactive protein `c_reactive_protein`). Continuous covariates modeled flexibly (e.g., restricted cubic splines).
- $gamma_(i t)$: Patient-specific random effect (random intercept $upright("gamma")_(0 i)$) assuming $upright("gamma")_(0 i) tilde.op upright("Normal") (0 \, sigma_(upright("gamma"))^2)$. Accounts for within-patient correlation beyond the Markov dependency.

#emph[Note: The exact specification of interaction terms (e.g., `%ia%` vs `*` in `rms` syntax) and flexibility of time/covariates depends on final model selection based on convergence and diagnostics.]

==== Prior Specifications
<prior-specifications-2>
- #strong[Intercepts (];$alpha_j$): Priors induced by Dirichlet distribution on baseline cell probabilities (e.g., Dirichlet(0.38) for 5 levels, calculated as $1 \/ (0.8 + 0.35 dot.op upright("max") (k \, 3))$).
- #strong[Fixed Effects Coefficients (];$beta_k$): $upright("Normal") (0 \, 100^2)$ (Default weakly informative priors in `rmsb::blrm`).
- #strong[Random Effect Standard Deviation (];$sigma_(upright("gamma"))$): TBD (e.g., Half-Cauchy or Half-Normal).
- #strong[Correlation parameters (if random slopes included)];: TBD (e.g., LKJ prior).

==== Implementation
<implementation-2>
The model will be fitted using the `blrm` function from the `rmsb` package, including the `cluster(id)` term for random effects. MCMC settings as per OS analysis.

#block[
```r
# Example R code for model fitting (adjust formula based on final spec)
# Assumes 'data_toh' has columns: y (state 1-5), tx (0/1), week, 
#                  yprev (factor 1-4), ypprev (factor 1-4), 
#                  id (patient identifier), ecog_fstcnt, diagnosis, albumin, c_reactive_protein

library(rmsb)
library(rms) 

# Prepare data (create lags yprev, ypprev if needed)
# Ensure factors have appropriate levels (excluding absorbing state 5 for lags)
# data_toh$yprev <- factor(data_toh$yprev, levels = 1:4)
# data_toh$ypprev <- factor(data_toh$ypprev, levels = 1:4)

model_toh <- blrm(
    formula = y ~ tx + rcs(week, 4) + yprev + ypprev + 
                week %ia% yprev + week %ia% ypprev + # Example interactions
                ecog_fstcnt + diagnosis + rcs(albumin, 3) + rcs(c_reactive_protein, 3) + 
                cluster(id), # Random intercept for patient id
    data = data_toh,
    # No ppo term unless specific non-proportional effects identified
    iter = 4000, chains = 4, seed = 1234,
    # ... other MCMC settings, potentially adjust backend/method for speed ...
    # backend = "cmdstanr" 
)
```

]
==== Derivation of the Estimand (TOH)
<derivation-of-the-estimand-toh>
+ #strong[Fit Model:] Fit the `model_toh` to each multiply imputed dataset (using the full ITT population).
+ #strong[Calculate Weekly SOPs:] For each posterior draw, calculate the SOPs, $P (Y_(i t)^a = j)$, for each week $t = 1 \, dots.h \, 26$, state $j = 1 \, dots.h \, 5$, and treatment arm $a in { 0 \, 1 }$. This requires a recursive calculation accounting for the second-order dependency and marginalizing over baseline covariates, baseline state pairs ($y_(- 1) \, y_0$), and random effects. (See Appendix D of `12-estimand.qmd` for the calculation logic).
+ #strong[Calculate Expected Weeks Alive/Out-of-Hospital per Draw:] For each posterior draw, calculate the expected number of weeks in State 1 for each treatment arm by summing the SOPs for State 1 over the 26 weeks: $ bb(E) [W^a]_(upright("draw")) = sum_(t = 1)^26 upright("SOP") (Y_(i t)^a = 1)_(upright("draw")) $
+ #strong[Calculate Difference per Draw:] For each posterior draw, compute the difference $tau_(T O H \, upright("draw")) = bb(E) [W^1]_(upright("draw")) - bb(E) [W^0]_(upright("draw"))$.
+ #strong[Summarize Posterior:] The collection of $tau_(T O H \, upright("draw"))$ values forms the posterior distribution of the primary estimand for TOH. Summarize using posterior median and 95% credible interval.

==== Simulation Study Summary (TOH)
<simulation-study-summary-toh>
A simulation study based on historical data was planned (`5-sim-datasets.qmd`).

- #strong[Data Generation:] Based on a second-order Markov model fitted to historical weekly health state data. Datasets generated under the null hypothesis (OR=1) and alternative hypotheses (e.g., OR=0.9, 0.8, 0.7) corresponding to specific differences in mean weeks alive/out-of-hospital.
- #strong[Analysis:] The planned Bayesian second-order Markov model with random effects was to be fitted to simulated trial datasets (N=270, 2:1 allocation).
- #strong[Results:] \[Specific results for bias, coverage, Type I error, and power under different effect sizes to be filled in based on simulation outputs.\]

= General Statistical Considerations
<general-statistical-considerations>
== Framework
<framework>
All primary analyses will be conducted within a #strong[Bayesian framework];. The inferential goal is to estimate the full posterior distribution for the primary estimand for each outcome ($tau_(O S)$, $tau_W^(upright("psATE"))$, $tau_(T O H)$). This allows for direct probabilistic statements about the plausible magnitude and direction of the treatment effects. We will report key summaries of the posterior distributions (e.g., median, 95% credible interval) and relevant probabilities (e.g., probability of benefit, $P (tau > 0)$ or $P (tau < 0)$ depending on endpoint direction). We will not pre-specify thresholds for dichotomous claims of success or failure based on these probabilities.

== Missing Data Handling
<sec-missing-data>
Missing data are expected for baseline covariates and longitudinal outcomes (QoL states, potentially intermediate TOH states if derived).

#strong[General Approach:] Multiple Imputation by Chained Equations (MICE) will be used, assuming data are Missing At Random (MAR). $m = 50$ completed datasets will be generated @Zhou2010-nm.

#strong[Imputation Model:]

\- #strong[Structure:] A multilevel imputation model will be used to account for clustering (repeated measures within patients). Patient identifier (`pat_id`) will be the cluster variable.

- #strong[Method:] Predictive Mean Matching (PMM) will be the primary method (`mice::mice` with appropriate `2l.pmm` or `2lonly.pmm` methods).

- #strong[Predictors:] The imputation model will include:

  - Treatment assignment (`tx`).

  - All variables included in the substantive analysis models (baseline covariates, outcome variables at different time points).

  - Auxiliary variables predictive of missingness or the outcome (e.g., patient age, gender, survival status/time or Nelson-Aalen estimate `na_est`).

- #strong[Specific Handling:]

  - #strong[Baseline Covariates:] Imputed at the patient level (`2lonly.pmm`) to ensure consistency across a patient's records within an imputed dataset.

  - #strong[Longitudinal QoL:] Imputed at the observation level (`2l.pmm`). Includes imputation for day 0 if needed for modeling.

  - #strong[Longitudinal TOH states:] For participants who chose to be treated at another hospital states will be imputed using `2l.pmm`.

- #strong[MCMC Settings:] $m = 50$ imputations, $m a x i t = 50$ iterations per imputation.

Convergence assessed via trace plots.

#strong[Analysis of Imputed Data:] Each substantive Bayesian model (OS, QoL, TOH) will be fitted to each of the $m = 50$ imputed datasets. The posterior draws for all parameters and derived estimands from the 50 analyses will be combined (stacked) to form the final posterior distribution for inference.

== Software
<software>
Analyses will be performed using R (version 4.4.3 or later) @RCoreTeam. Bayesian models will be fitted using Stan via the `rstanarm` @RStanARM and `rmsb` @rmsb packages. Multiple imputation will use the `mice` @mice and `miceadds` @miceadds packages.

= Study Hypotheses and Multiplicity
<study-hypotheses-and-multiplicity>
\[Section TBD - Needs refinement based on the Bayesian approach. Instead of strict hypothesis testing and alpha adjustments, focus might shift to interpreting posterior probabilities and credible intervals for each primary outcome, potentially with pre-specified regions of practical equivalence or clinical relevance if applicable. Decision rules based on posterior probabilities could be defined if necessary for specific claims.\]

= Limitations
<limitations>
== QoL Analysis Limitations
<sec-qol-limitations>
- #strong[Principal Stratum Assumption:] The primary QoL analysis relies on the strong, untestable assumption that selection for invitation has no effect on 6-month survival. If this assumption is violated, the analysis results apply only to the specific subgroup who would survive regardless, but this subgroup is not fully identifiable, and the estimate may be biased for the broader ITT population.
- #strong[Informative Missingness:] As QoL is collected during routine care, the ctDNA strategy might influence clinical pathways and appointment frequency (e.g., earlier BSC switch leading to fewer visits). This links the observation process to the treatment effect, potentially causing informative missingness not fully addressed by MAR-based imputation. This could bias the estimated treatment effect, likely towards the null if improved care leads to less frequent observation of improved QoL. This limitation stems from the trial design and affects all potential analysis methods.

== General Limitations
<general-limitations>
- #strong[External Control Group:] While randomization minimizes confounding at baseline, the external control group experiences standard care which may evolve over the trial period independently of the intervention.
- #strong[Single Center:] Results may not generalize perfectly to other healthcare settings.
- #strong[Intervention Complexity:] The "treatment" is selection for invitation to a complex intervention (ctDNA testing influencing care). Isolating the specific effect of ctDNA itself versus other aspects of trial participation is difficult.

= Reporting
<reporting>
Results will be reported according to CONSORT guidelines where applicable, adapted for the trial design. Analyses will follow the ITT principle (based on selection status). Posterior distributions for primary estimands will be visualized (e.g., density plots) and summarized (median, 95% CrI). Sensitivity analyses exploring key assumptions (e.g., MAR assumption, no-survival-effect assumption for QoL) will be conducted \[Details TBD\].

= References
<references>
\[References to be listed here using bibliography file\]

#pagebreak()
= Appendices
<appendices>
== Appendix A: Detailed Multiple Imputation Specification
<sec-appendix-imp>
=== Overview
<overview>
All imputations will be performed in R (version 4.4.3 or later) using the `mice` and `miceadds` package. The imputation procedure is designed to handle the multilevel structure of the longitudinal data (QoL, TOH states) and uses predictive mean matching (PMM).

=== Imputation Model Specification
<imputation-model-specification>
The imputation model will include all variables from the primary analysis models (OS, QoL, TOH), auxiliary variables, and the cluster identifier.

- #strong[Variables in the Model:] The predictor matrix for the MICE algorithm will include:
  - Treatment assignment (`tx`).
  - Baseline patient characteristics (`pat_age`, `gender`, `ecog_fstcnt`, `diagnosis`/`diagnosis_cat`/`diagnosis_lumped`, `albumin`, `c_reactive_protein`, `lactate_dehydrogenase`, etc.).
  - Longitudinal outcomes (`q30` for QoL, derived weekly state for TOH).
  - Time variable (`week` or `quest_day`).
  - Survival information: event indicators (`event_death`, `event_progression_or_death`) and Nelson-Aalen cumulative hazard estimate (`na_est`) derived from observed survival times. Raw survival times will #emph[not] be used directly as predictors @White2009-hs.
  - Other potentially relevant auxiliary variables from EHR/CDWH \[TBD\].
- #strong[Clustering:] The patient identifier (`pat_id`) will be specified as the clustering variable (`-2` in the `mice` predictor matrix).
- #strong[Interactions/Non-linearities:] \[TBD - consider including interactions or non-linear terms if strongly suspected to be important for the MAR assumption\].

=== Imputation Method by Variable Type
<imputation-method-by-variable-type>
- #strong[Baseline Covariates] (e.g., `ecog_fstcnt`, `diagnosis`, baseline lab values): Imputed using patient-level PMM (`2lonly.pmm`). Ensures a single imputed value per patient across all time points within an imputed dataset.
- #strong[Longitudinal Ordinal QoL (`q30`)];: Imputed using observation-level PMM within the multilevel framework (`2l.pmm`). Includes imputation for day 0 if required by the model.
- #strong[Longitudinal Ordinal TOH State (`y_toh`)];: If missingness occurs, imputed using observation-level PMM (`2l.pmm`). \[Confirm if TOH state derivation leads to missingness\].

=== MCMC Parameters (MICE)
<mcmc-parameters-mice>
- Number of Imputations ($m$): 50
- Number of Iterations ($m a x i t$): 50
- Convergence Assessment: Visual inspection of trace plots and density plots of imputed values versus observed values.

=== Pooling of Results
<pooling-of-results>
Bayesian model fitting results (posterior draws) from each of the 50 imputed datasets will be stacked to form the final posterior distribution for inference, following Rubin's rules adapted for Bayesian analysis @zhou2010note.

=== Example Code Snippet (Illustrative)
<example-code-snippet-illustrative>
#block[
```r
library(mice)
library(miceadds)
library(survival) # for Surv(), nelsonaalen() if not already loaded

# Assume 'long_data' is the dataset in long format with missing values
# Add Nelson-Aalen estimator (calculated on one record per patient)
# patient_level_data <- long_data[!duplicated(long_data$pat_id), ]
# na_fit <- survfit(Surv(survival_time, event_death) ~ 1, data = patient_level_data)
# na_estimates <- tibble(pat_id = patient_level_data$pat_id, na_est = summary(na_fit, times = patient_level_data$survival_time)$cumhaz)
# long_data <- merge(long_data, na_estimates, by = "pat_id", all.x = TRUE)

# Ensure patient ID is factor/integer
# long_data$pat_id <- as.integer(factor(long_data$pat_id)) 

# Set up imputation predictor matrix and methods
# init <- mice(long_data, maxit = 0)
# pred_matrix <- init$predictorMatrix
# meth <- init$method

# Set patient ID as cluster variable (-2)
# pred_matrix[, "pat_id"] <- -2
# Do not use pat_id to predict others
# pred_matrix["pat_id", ] <- 0 

# Do not use raw survival time if using na_est
# pred_matrix[, "survival_time"] <- 0 

# Specify methods
# meth["q30"] <- "2l.pmm" 
# meth["baseline_covariate1"] <- "2lonly.pmm" 
# meth["baseline_covariate2"] <- "2lonly.pmm" 
# ... etc. ...

# Run imputation
# imputed_data <- mice(long_data,
#                      m = 50,
#                      maxit = 50,
#                      predictorMatrix = pred_matrix,
#                      method = meth,
#                      seed = 1234)

# Analysis would then loop through imputed datasets 1 to 50
# results_list <- vector("list", 50)
# for (i in 1:50) {
#   completed_data <- complete(imputed_data, i)
#   # Fit Bayesian model (e.g., model_qol)
#   fit <- blrm(...) 
#   # Store posterior draws
#   results_list[[i]] <- as.data.frame(fit) 
# }
# combined_posterior <- do.call(rbind, results_list)
# Now analyze 'combined_posterior'
```

]
== Appendix B: QoL Interval Censoring Investigation
<sec-appendix-qol-censoring>
=== Background
<background>
An alternative to the Principal Stratum analysis for QoL is to model the 8-state process (7 QoL states + Death) directly, treating unobserved weeks for living patients as interval-censored (state is in `[1, 7]`). The `rmsb` package supports this via `Ocens()`.

=== Method
<method>
We tested this using a simulated dataset derived from historical data (see `qol-sap.qmd`, Appendix C), where the true state trajectories were known. We created a sparse dataset (15% observations retained post-baseline) and fitted a first-order Markov model using `Ocens(y.a, y.b)` where `y.a=1, y.b=7` for missing weeks of alive patients, and `y.a=y.b=8` at death.

=== Findings
<findings>
#figure([
#box(image("img/sop-cens-overestimate.svg"))
], caption: figure.caption(
position: bottom, 
[
Comparison of empirical SOPs (A) vs.~model-derived SOPs using interval censoring (B). The model significantly overestimates the cumulative incidence of death (State 8).
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-sop-cens-overestimate>


The interval censoring approach led to a substantial overestimation of the cumulative incidence of death (#ref(<fig-sop-cens-overestimate>, supplement: [Figure])). This bias likely arises because the absorbing death state is always observed precisely, while the intermediate living states are heavily censored. The model appears to incorrectly assign probability mass over time towards the known absorbing state. This bias propagates, underestimating time spent in living states.

=== Conclusion
<conclusion>
Due to this potential for significant bias with sparse intermediate data, the interval censoring approach was deemed unsuitable for the primary QoL analysis, leading to the adoption of the Principal Stratum approach with its specific assumptions.

== Appendix C: Model Diagnostics
<appendix-c-model-diagnostics>
\[Placeholder: This section will include standard MCMC convergence diagnostics (trace plots, $hat(R)$, effective sample size summaries) and posterior predictive checks for the primary models fitted to the actual LIQPLAT data.\]

== Appendix D: Simulation Details
<appendix-d-simulation-details>
\[Placeholder: This section will contain more detailed descriptions or links to code repositories for the simulation studies mentioned for OS, QoL, and TOH, summarizing the specific data generation parameters, model fitting code for simulations, and detailed results (tables/figures) for bias, coverage, Type I error, and power.\]

#bibliography("references.bib")

