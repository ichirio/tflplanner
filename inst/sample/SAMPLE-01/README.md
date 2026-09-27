# SAMPLE-01: the sample study of tflplanner

The CDISC pilot study (Xanomeline) as the ADaM datasets of the
[pharmaverseadam](https://pharmaverse.github.io/pharmaverseadam/) R package
(Apache License 2.0): ADSL, ADAE and ADVS (systolic blood pressure after 5
minutes lying down).

| Output | Type | |
|---|---|---|
| T-14-1-1 | Table | Demographic characteristics |
| T-14-1-2 | Table | Subject disposition |
| T-14-2-1 | Table | Systolic blood pressure: change from baseline at Week 24 (SE, 95% CI of the mean) |
| T-14-3-1 | Table | TEAEs by SOC / PT |
| L-16-2-7 | Listing | Severe adverse events |
| F-14-2-1 | Figure | Mean change from baseline in systolic blood pressure |

The tables are made from one study ARD (programs/ard/), the listing and
the figure from the ADaM data.  Made by data-raw/make-sample-study.R of
the tflplanner repository.
