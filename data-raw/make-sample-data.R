# The ADaM data of the sample study (inst/sample/SAMPLE-01/data/adam):
# adsl.rds, adae.rds, advs.rds and adtte.rds, from tflspec's oncology sample
# study XYZ-ONC-01 (simulated; Drug X vs Control; see ?tflspec::adsl).
#
#   Rscript data-raw/make-sample-data.R      (from the repository's root)
#
# Run it only when tflspec's sample data or the ADVS cut below changes.  It
# writes those four files and nothing else: the rest of SAMPLE-01 (study.yml,
# spec/, README.md, data/adam/define.xml) is edited by hand and is the one
# source of the sample (tflplanner#879).  tests/testthat/test-sample-data.R
# checks that the four files are what this script makes.
#
# ADSL, ADAE and ADTTE as they are (ADSL with its screen failures: the Safety
# set leaves them out; ADTTE: PFS, OS, DOR and the safety endpoint TTSKIN, the
# time to the first treatment-emergent skin disorder, in days).  ADVS: the
# systolic blood pressure, at baseline and on day 1 of each 21-day cycle
# (cycle 9 day 1 is week 24), with the actual arm as an event dataset has it
# (TRTA).

stopifnot(utils::packageVersion("tflspec") >= "0.0.24.9168")

adsl <- as.data.frame(tflspec::adsl)
adae <- as.data.frame(tflspec::adae)
adtte <- as.data.frame(tflspec::adtte)
advs <- as.data.frame(tflspec::advs)
advs <- advs[advs$PARAMCD == "SYSBP", ]
advs$TRTA <- structure(as.character(advs$TRT01A), label = "Actual Treatment")
advs <- advs[c("STUDYID", "USUBJID", "TRTA", "TRT01P", "TRT01A", "PARAMCD", "PARAM", "AVISIT", "AVISITN",
               "ADT", "ADY", "AVAL", "BASE", "CHG", "ABLFL", "ONTRTFL")]
rownames(advs) <- NULL

# the SAS format attributes are left out of the files
strip <- function(d) {
  for (k in names(d)) attr(d[[k]], "format.sas") <- NULL
  d
}

adam <- file.path("inst", "sample", "SAMPLE-01", "data", "adam")
stopifnot(dir.exists(adam))
saveRDS(strip(adsl), file.path(adam, "adsl.rds"))
saveRDS(strip(adae), file.path(adam, "adae.rds"))
saveRDS(strip(advs), file.path(adam, "advs.rds"))
saveRDS(strip(adtte), file.path(adam, "adtte.rds"))
cat("Wrote the 4 ADaM files to", adam, "\n")
