# SAMPLE-01: the sample study of tflplanner

tflspec's oncology sample study XYZ-ONC-01 (simulated ADaM data; not real
patients; see ?tflspec::adsl and the study's SAP,
system.file("sample", "onco", package = "tflspec")): Drug X vs
Control, ADSL (the 200 screened subjects: 160 randomised, 40 screen
failures), ADAE, ADVS (systolic blood pressure at baseline and on day 1 of
each 21-day cycle; week 24 is cycle 9 day 1) and ADTTE (PFS, OS, DOR and
TTSKIN, the time to the first treatment-emergent skin disorder).

ADSL's population flags, of which step 2 makes an analysis set:

| Flag | Label | Y when | Subjects |
|---|---|---|---|
| ENRLFL | Enrolled Population Flag | randomised (not a screen failure) | 160 |
| ITTFL | Intent-To-Treat Population Flag | randomised | 160 |
| SAFFL | Safety Population Flag | at least one dose | 158 |
| EFFFL | Efficacy Evaluable Population Flag | SAFFL is Y and a post-baseline tumour assessment | 157 |

The study's analysis sets are SAF, SCRF (the screen failures, by ARMNRS
alone: they have no arm and no flag) and ENR (every subject of ADSL: the
screened set).

| Output | Type | |
|---|---|---|
| T-14-0-1 | Table | Study information: dictionary versions and the dates of the data -- no analysis set (it counts no subjects) |
| T-14-1-1 | Table | Demographic characteristics |
| T-14-1-1S | Table | The same table, its ARD one `cards::ard_stack()` call (the analyses inside it, and the column N it makes) |
| T-14-1-2 | Table | Subject disposition |
| T-14-1-4 | Table | Demographic characteristics of the screen failures: an analysis set by a condition alone (SCRF, ARMNRS is SCREEN FAILURE; ADSL has no flag for it) |
| T-14-1-3 | Table | Age group and sex: the ARD keeps cards' default n, N and p, the table prints n (%) (the N rows its cells do not name are left out) |
| T-14-1-5 | Table | Demographics of the screened subjects by arm (screen failures a column of their own), Total; race's Asian sub-categories nested under Asian |
| T-14-1-6 | Table | The same, the Asian sub-categories a block of their own |
| T-14-2-1 | Table | Systolic blood pressure: change from baseline at week 24, cycle 9 day 1 (SE, 95% CI of the mean) |
| T-14-2-3 | Table | Systolic blood pressure: mean change at week 24 (95% CI) and p-value of a one-sample t-test (the ARD also holds the test's method and alternative as text) |
| T-14-2-2 | Table | Time to first skin disorder: Kaplan-Meier estimates (its ARD also holds the hazard ratio of Drug X vs Control) |
| T-14-3-1 | Table | TEAEs by SOC / PT |
| L-16-2-7 | Listing | Severe treatment-emergent adverse events |
| F-14-2-1 | Figure (R program) | Mean change from baseline in systolic blood pressure: its content step is its own R |
| F-14-2-2 | Figure (R program) | Kaplan-Meier plot of the time to first skin disorder, its own R; the number at risk from T-14-2-2's ARD (`ard`) |
| F-14-2-3 | Figure (designed) | The same KM curves from the designer's KM template, with a median line added |
| F-14-2-4 | Figure (designed) | Forest plot of the hazard ratio by subgroup from the figure's own ARD (a Cox model overall and within each subgroup, cards / cardx) |

The tables are made from one study ARD (programs/ard/), the listing and
the figures from the ADaM data, in the figure style of the company
standards (programs/tfl/fig_setup.R).  Made by data-raw/make-sample-study.R
of the tflplanner repository.
