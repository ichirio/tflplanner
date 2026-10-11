# SAMPLE-01: the sample study of tflplanner

The CDISC pilot study (Xanomeline) as the ADaM datasets of the
[pharmaverseadam](https://pharmaverse.github.io/pharmaverseadam/) R package
(Apache License 2.0): ADSL, ADAE, ADVS (systolic blood pressure after 5
minutes lying down) and ADTTE (the time to the first dermatologic event,
derived from ADSL and ADAE by data-raw/make-sample-study.R).

ITTFL, EFFFL and PPROTFL are not in pharmaverseadam's ADSL (of the
population flags it has SAFFL only); data-raw/make-sample-study.R
derives them, so step 2 has more than one flag to choose from:

| Flag | Label | Y when | Subjects |
|---|---|---|---|
| ITTFL | Intent-To-Treat Population Flag | randomized (ARM is not Screen Failure) | 254 |
| EFFFL | Efficacy Population Flag | SAFFL is Y and a post-baseline systolic blood pressure (CHG) in ADVS | 230 |
| PPROTFL | Per-Protocol Population Flag | SAFFL is Y and EOSSTT is COMPLETED | 110 |

They are not analysis sets of the study (its populations sheet has SAF,
and SCRF -- the screen failures, by ARM alone): in step 2-1 they are
offered as flags of ADSL, to make one.

| Output | Type | |
|---|---|---|
| T-14-0-1 | Table | Study information: dictionary versions and the dates of the data -- no analysis set (it counts no subjects) |
| T-14-1-1 | Table | Demographic characteristics |
| T-14-1-1S | Table | The same table, its ARD one `cards::ard_stack()` call (the analyses inside it, and the column N it makes) |
| T-14-1-2 | Table | Subject disposition |
| T-14-1-4 | Table | Demographic characteristics of the screen failures: an analysis set by a condition alone (SCRF, ARM is Screen Failure; ADSL has no flag for it) |
| T-14-1-3 | Table | Age group and sex: the ARD keeps cards' default n, N and p, the table prints n (%) (the N rows its cells do not name are left out) |
| T-14-2-1 | Table | Systolic blood pressure: change from baseline at Week 24 (SE, 95% CI of the mean) |
| T-14-2-3 | Table | Systolic blood pressure: mean change at Week 24 (95% CI) and p-value of a one-sample t-test (the ARD also holds the test's method and alternative as text) |
| T-14-2-2 | Table | Time to first dermatologic event: Kaplan-Meier estimates |
| T-14-3-1 | Table | TEAEs by SOC / PT |
| L-16-2-7 | Listing | Severe treatment-emergent adverse events |
| F-14-2-1 | User code (a figure) | Mean change from baseline in systolic blood pressure |
| F-14-2-2 | User code (a figure) | Kaplan-Meier plot of the time to first dermatologic event (number at risk from T-14-2-2's ARD) |
| F-14-2-3 | Figure (designed) | The same KM curves from the designer's KM template, with a median line added |
| F-14-2-4 | Figure (designed) | Forest plot of the hazard ratio by subgroup from the figure's own ARD (a Cox model overall and within each subgroup, cards / cardx) |

The tables are made from one study ARD (programs/ard/), the listing and
the figures from the ADaM data, in the figure style of the company
standards (programs/tfl/fig_setup.R).  Made by data-raw/make-sample-study.R
of the tflplanner repository.
