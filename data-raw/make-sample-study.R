# The sample study that ships with tflplanner (inst/sample/SAMPLE-01):
# one of each kind of report, on the CDISC pilot ADaM data of
# {pharmaverseadam} (Apache License 2.0).
#
#   T-14-0-1  Table    Study information (dictionary versions, the dates
#                      of the data): no analysis set -- its ARD is the
#                      study's own facts, made by code
#   T-14-1-1  Table    Demographic characteristics
#   T-14-1-1S Table    The same table, its ARD one cards::ard_stack() call
#                      (the continuous and the categorical analyses inside
#                      it, and the column N it makes itself)
#   T-14-1-2  Table    Subject disposition
#   T-14-1-4  Table    Demographic characteristics of the screen failures:
#                      an analysis set by a condition alone (SCRF, ARM is
#                      Screen Failure), no flag
#   T-14-1-5  Table    Race with its Asian sub-categories nested under Asian
#                      in one block (variables$under, plan_nest()): the
#                      same data and analyses as T-14-1-6
#   T-14-1-6  Table    Race with its Asian sub-categories, two blocks (Race;
#                      Race Sub Asian): the enrolled subjects (screen
#                      failures too: the pilot's two Asian subjects are
#                      screen failures) by ARM, with a Total column; the
#                      sub-categories are derived for demonstration
#   T-14-1-3  Table    Age group and sex: the ARD keeps cards' default
#                      statistics (n, N, p), the table prints n (%) -- the
#                      N rows the cells do not name are left out
#   T-14-2-1  Table    Change from baseline in systolic blood pressure,
#                      Week 24 (with SE and the mean's 95% CI)
#   T-14-2-3  Table    Change from baseline in systolic blood pressure at
#                      Week 24: mean (95% CI) and p-value of a one-sample
#                      t-test -- the ARD also holds the test's method and
#                      alternative as text
#   T-14-2-2  Table    Time to first dermatologic event: Kaplan-Meier
#                      estimates (median, event-free probability by day);
#                      its ARD also has the hazard ratios of a Cox model,
#                      which the table does not print (F-14-2-3 does)
#   T-14-3-1  Table    TEAEs by SOC / PT, frequency descending
#   L-16-2-7  Listing  Severe treatment-emergent adverse events
#   F-14-2-1  User code  Mean change from baseline in systolic blood pressure
#   F-14-2-2  User code  Kaplan-Meier plot of the time to first dermatologic
#                      event; its number at risk is T-14-2-2's ARD
#   F-14-2-3  Figure   The same KM curves, designed: the designer's KM
#                      template, the median line, and the medians and
#                      hazard ratios printed from T-14-2-2's ARD (its ARD
#                      source: table:T-14-2-2)
#   F-14-2-4  Figure   Forest plot of the hazard ratio by subgroup from the
#                      designer's forest template: the figure's own ARD (a
#                      Cox model of all subjects and within each subgroup,
#                      cards / cardx; ard_source: own)
#
# The figures use the company standards' figure style (programs/tfl/
# fig_setup.R: theme_tfl(), scale_colour_tfl(), tfl_km_risk() ...) and end
# with its checks (tfl_check()).
#
#   Rscript data-raw/make-sample-study.R
#
# It builds the study in a temporary home and folder -- never in a
# developer's or a user's home -- runs the official run (the ARD, then the
# reports), and fails unless every program runs without error.  Then it
# writes what a study folder needs to be registered again (study.yml, the
# .Rproj, spec/, data/) to inst/sample/SAMPLE-01; the programs, the study
# ARD and the reports are made where the sample is copied
# (create_sample_study()).

devtools::load_all(quiet = TRUE)
stopifnot(requireNamespace("pharmaverseadam", quietly = TRUE),
          requireNamespace("cards", quietly = TRUE),
          requireNamespace("cardx", quietly = TRUE))

id <- "SAMPLE-01"
tmp <- tempfile("sample")
options(tflplanner.home = file.path(tmp, "home"))
suppressMessages(setup_tflplanner(studies_root = file.path(tmp, "studies")))

tbl <- function(...) {
  rows <- list(...)
  cols <- unique(unlist(lapply(rows, names)))
  as.data.frame(lapply(stats::setNames(cols, cols), function(cn)
    vapply(rows, function(r) {
      v <- r[[cn]]
      if (is.null(v) || (length(v) == 1L && is.na(v))) NA_character_ else
        as.character(v)
    }, "")), stringsAsFactors = FALSE)
}

# A report's code list: its values in order -- "a | b | c", or named by
# value with the text each prints as
codelist <- function(output_id, variable, values) {
  if (is.null(names(values))) values <- trimws(strsplit(values, "|", fixed = TRUE)[[1L]])
  data.frame(output_id = output_id, variable = variable,
             value = if (is.null(names(values))) values else names(values),
             label = if (is.null(names(values))) NA_character_ else unname(values),
             order = as.character(seq_along(values)), stringsAsFactors = FALSE)
}
arms <- "Placebo | Xanomeline Low Dose | Xanomeline High Dose"

# ------------------------------------------------------------ the data
# The CDISC pilot study as {pharmaverseadam} has it; screen failures stay in
# ADSL (the Safety set leaves them out).  ADVS: systolic blood pressure
# after 5 minutes lying down, the post-baseline records of the analysis flag
# (they carry BASE and CHG), End of Treatment left out.

adsl <- as.data.frame(pharmaverseadam::adsl)
adae <- as.data.frame(pharmaverseadam::adae)
advs <- as.data.frame(pharmaverseadam::advs)
advs <- advs[advs$PARAMCD == "SYSBP" & advs$ANL01FL %in% "Y" &
               advs$ATPT %in% "AFTER LYING DOWN FOR 5 MINUTES" &
               !advs$AVISIT %in% "End of Treatment",
             c("STUDYID", "USUBJID", "PARAMCD", "PARAM", "AVISIT", "AVISITN",
               "ATPT", "AVAL", "BASE", "CHG", "ABLFL", "ANL01FL", "TRTA",
               "TRTP")]
rownames(advs) <- NULL
# Population flags pharmaverseadam's ADSL lacks (of those, it has SAFFL), derived here
# so that step 2 has more than one to choose from -- the README says how:
# ITTFL the randomized (not a screen failure), EFFFL the safety set with a
# post-baseline systolic blood pressure, PPROTFL the safety set that
# completed the study.  The study's analysis sets stay SAF only.
flag <- function(v, label) structure(ifelse(v, "Y", "N"), label = label)
adsl$ITTFL <- flag(adsl$ARM != "Screen Failure", "Intent-To-Treat Population Flag")
adsl$EFFFL <- flag(adsl$SAFFL %in% "Y" &
                     adsl$USUBJID %in% advs$USUBJID[!advs$ABLFL %in% "Y" & !is.na(advs$CHG)],
                   "Efficacy Population Flag")
adsl$PPROTFL <- flag(adsl$SAFFL %in% "Y" & adsl$EOSSTT %in% "COMPLETED",
                     "Per-Protocol Population Flag")
# the arm's number, as an ADaM has it (pharmaverseadam lacks it): a listing
# sorts by it, so the arms come in their order, not the alphabet's
arm_n <- c("Placebo" = 0, "Xanomeline Low Dose" = 54, "Xanomeline High Dose" = 81)
adsl$TRT01AN <- structure(unname(arm_n[adsl$TRT01A]),
                          label = "Actual Treatment for Period 01 (N)")
adae$TRT01AN <- structure(unname(arm_n[adae$TRT01A]),
                          label = "Actual Treatment for Period 01 (N)")
# ADTTE: time to the first dermatologic treatment-emergent adverse event
# (skin and subcutaneous tissue disorders, or an application site event),
# the CDISC pilot's time-to-event endpoint; censored at the end of treatment.
derm <- adae[adae$TRTEMFL %in% "Y" &
               (adae$AEBODSYS %in% "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" |
                  startsWith(adae$AEDECOD, "APPLICATION SITE")), ]
first <- aggregate(ASTDT ~ USUBJID, data = derm, FUN = min)
adtte <- merge(adsl[adsl$SAFFL == "Y",
                    c("STUDYID", "USUBJID", "TRT01A", "SAFFL", "TRTSDT", "TRTEDT")],
               first, by = "USUBJID", all.x = TRUE)
adtte$TRTEDT[is.na(adtte$TRTEDT)] <- adtte$TRTSDT[is.na(adtte$TRTEDT)]
adtte$CNSR <- ifelse(is.na(adtte$ASTDT), 1L, 0L)
adtte$ADT <- as.Date(ifelse(is.na(adtte$ASTDT), adtte$TRTEDT, adtte$ASTDT),
                     origin = "1970-01-01")
adtte$AVAL <- as.numeric(adtte$ADT - adtte$TRTSDT) + 1
adtte$PARAMCD <- "TTDE"
adtte$PARAM <- "Time to First Dermatologic Event (Days)"
adtte <- adtte[c("STUDYID", "USUBJID", "TRT01A", "SAFFL", "PARAMCD", "PARAM",
                 "TRTSDT", "ADT", "AVAL", "CNSR")]

strip <- function(d) {
  for (k in names(d)) attr(d[[k]], "format.sas") <- NULL
  d
}

# ------------------------------------------------------------ the table half
sheets <- list(
  tables = tbl(
    list(output_id = "T-14-1-1", cols = "TRT01A", rows = "group = variable",
         note = "Demographics"),
    list(output_id = "T-14-1-2", cols = "TRT01A", rows = "group = variable",
         note = "Disposition"),
    list(output_id = "T-14-2-2", cols = "TRT01A", rows = "group = variable",
         note = "KM estimates"),
    list(output_id = "T-14-2-1", cols = "TRTA", rows = "group = variable",
         note = "SYSBP at Week 24: baseline, value, change"),
    list(output_id = "T-14-0-1", cols = "context", rows = "group = variable",
         note = "Study information: one row a fact, its value text"),
    list(output_id = "T-14-1-4", cols = "ARM", rows = "group = variable",
         note = "The screen failures' demographics"),
    list(output_id = "T-14-1-3", cols = "TRT01A", rows = "group = variable",
         note = "Age group and sex: the ARD's n, N and p, the table's n (%)"),
    list(output_id = "T-14-1-5", cols = "ARM", rows = "group = variable",
         note = "Race, its Asian sub-categories nested under Asian: one block"),
    list(output_id = "T-14-1-6", cols = "ARM", rows = "group = variable",
         note = "Race, then its Asian sub-categories: two blocks"),
    list(output_id = "T-14-2-3", cols = "TRTA", rows = "group = variable",
         note = "SYSBP change at Week 24: mean (95% CI), p-value"),
    list(output_id = "T-14-3-1", cols = "TRT01A", rows = "group1 = AEBODSYS",
         label = "label = AEDECOD",
         sort = ".overall | group1 | .depth | -n | label",
         note = "TEAE by SOC / PT, frequency descending")),
  variables = tbl(
    list(output_id = "T-14-1-1", variable = "AGE", label = "Age (years)",
         order = 1),
    list(output_id = "T-14-1-1", variable = "AGEGR1",
         label = "Age group, n (%)", order = 2),
    list(output_id = "T-14-1-1", variable = "SEX", label = "Sex, n (%)",
         order = 3),
    list(output_id = "T-14-1-1", variable = "RACE", label = "Race, n (%)",
         order = 4),
    list(output_id = "T-14-1-1", variable = "ETHNIC",
         label = "Ethnicity, n (%)", order = 5),
    list(output_id = "T-14-1-2", variable = "EOSSTT",
         label = "Status at end of study, n (%)", order = 1),
    list(output_id = "T-14-2-1", variable = "BASE",
         label = "Baseline (mmHg)", order = 1),
    list(output_id = "T-14-2-1", variable = "AVAL",
         label = "Week 24 (mmHg)", order = 2),
    list(output_id = "T-14-2-1", variable = "CHG",
         label = "Change from baseline (mmHg)", order = 3),
    list(output_id = "T-14-0-1", variable = "INFO",
         label = "Dictionaries and dates of the data", order = 1),
    list(output_id = "T-14-1-4", variable = "AGE", label = "Age (years)",
         order = 1),
    list(output_id = "T-14-1-4", variable = "AGEGR1",
         label = "Age group, n (%)", order = 2),
    list(output_id = "T-14-1-4", variable = "SEX", label = "Sex, n (%)",
         order = 3),
    list(output_id = "T-14-1-4", variable = "RACE", label = "Race, n (%)",
         order = 4),
    list(output_id = "T-14-1-3", variable = "AGEGR1",
         label = "Age group, n (%)", order = 1),
    list(output_id = "T-14-1-3", variable = "SEX", label = "Sex, n (%)",
         order = 2),
    list(output_id = "T-14-2-3", variable = "CHG",
         label = "Change from baseline at Week 24 (mmHg)", order = 1),
    list(output_id = "T-14-2-2", variable = "prob",
         label = "Time to first event (days)", order = 1),
    list(output_id = "T-14-2-2", variable = "time",
         label = "Event-free probability (95% CI)", order = 2),
    list(output_id = "T-14-1-5", variable = "RACE", label = "Race, n (%)",
         order = 1),
    # its rows under RACE's "Asian" row (the level as the table prints it)
    list(output_id = "T-14-1-5", variable = "RACESUB", order = 2,
         under = "RACE: Asian"),
    list(output_id = "T-14-1-6", variable = "RACE", label = "Race, n (%)",
         order = 1),
    list(output_id = "T-14-1-6", variable = "RACESUB", label = "Race Sub Asian",
         order = 2)),
  # the code lists: each table's own (a code list is a report's).  The
  # CRF's values the data have none of are listed too: their rows print
  # with 0 (RACE's ASIAN ..., ETHNIC's NOT REPORTED / UNKNOWN)
  codelists = do.call(rbind, c(
    lapply(c("T-14-1-1", "T-14-1-2", "T-14-1-3", "T-14-2-2", "T-14-3-1"),
           function(o) codelist(o, "TRT01A", arms)),
    list(codelist("T-14-2-1", "TRTA", arms),
         codelist("T-14-2-3", "TRTA", arms),
         codelist("T-14-1-3", "SEX", c(F = "Female", M = "Male")),
         codelist("T-14-1-4", "ARM", "Screen Failure"),
         codelist("T-14-1-4", "SEX", c(F = "Female", M = "Male")),
         codelist("T-14-1-4", "AGEGR1", c("18-64" = "18-64 years",
                                          ">64" = ">64 years")),
         codelist("T-14-1-4", "RACE", c(
           "WHITE" = "White",
           "BLACK OR AFRICAN AMERICAN" = "Black or African American",
           "ASIAN" = "Asian",
           "AMERICAN INDIAN OR ALASKA NATIVE" = "American Indian or Alaska Native",
           "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" =
             "Native Hawaiian or Other Pacific Islander")),
         codelist("T-14-1-3", "AGEGR1", c("18-64" = "18-64 years",
                                          ">64" = ">64 years")),
         codelist("T-14-1-1", "SEX", c(F = "Female", M = "Male")),
         codelist("T-14-1-1", "AGEGR1", c("18-64" = "18-64 years",
                                          ">64" = ">64 years")),
         codelist("T-14-1-1", "RACE", c(
           "WHITE" = "White",
           "BLACK OR AFRICAN AMERICAN" = "Black or African American",
           "ASIAN" = "Asian",
           "AMERICAN INDIAN OR ALASKA NATIVE" = "American Indian or Alaska Native",
           "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" =
             "Native Hawaiian or Other Pacific Islander")),
         codelist("T-14-1-1", "ETHNIC", c(
           "HISPANIC OR LATINO" = "Hispanic or Latino",
           "NOT HISPANIC OR LATINO" = "Not Hispanic or Latino",
           "NOT REPORTED" = "Not reported", "UNKNOWN" = "Unknown")),
         # T-14-1-6: the enrolled subjects' arms (screen failures too), and
         # a Total column; race as the other tables; the demonstration
         # sub-categories of Asian
         codelist("T-14-1-5", "ARM", paste(arms, "| Screen Failure | Total")),
         codelist("T-14-1-5", "RACE", c(
           "WHITE" = "White",
           "BLACK OR AFRICAN AMERICAN" = "Black or African American",
           "ASIAN" = "Asian",
           "AMERICAN INDIAN OR ALASKA NATIVE" = "American Indian or Alaska Native",
           "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" =
             "Native Hawaiian or Other Pacific Islander")),
         codelist("T-14-1-5", "RACESUB", c(CHINESE = "Chinese", JAPANESE = "Japanese",
                                           KOREAN = "Korean")),
         codelist("T-14-1-6", "ARM", paste(arms, "| Screen Failure | Total")),
         codelist("T-14-1-6", "RACE", c(
           "WHITE" = "White",
           "BLACK OR AFRICAN AMERICAN" = "Black or African American",
           "ASIAN" = "Asian",
           "AMERICAN INDIAN OR ALASKA NATIVE" = "American Indian or Alaska Native",
           "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" =
             "Native Hawaiian or Other Pacific Islander")),
         codelist("T-14-1-6", "RACESUB", c(CHINESE = "Chinese", JAPANESE = "Japanese",
                                           KOREAN = "Korean")),
         # the disposition table's status at the end of the study
         codelist("T-14-1-2", "EOSSTT", c(COMPLETED = "Completed",
                                          DISCONTINUED = "Discontinued"))))),
  cells = tbl(
    list(output_id = "T-14-2-2", variable = "prob", when = "is.na(estimate)",
         template = "NE"),
    list(output_id = "T-14-2-2", variable = "prob",
         template = "{estimate:.0f} ({conf.low:.0f}, {conf.high:.0f})"),
    list(output_id = "T-14-2-2", variable = "time",
         template = "{estimate:.1f%} ({conf.low:.1f%}, {conf.high:.1f%})"),
    list(template = "{n:.0f} ({p:.1f%})", note = "every categorical cell"),
    list(output_id = "T-14-1-1", variable = "continuous", row = "n",
         template = "{N}", digits = "0"),
    list(output_id = "T-14-1-1", variable = "continuous", row = "Mean (SD)",
         template = "{mean} ({sd})", digits = "1,2"),
    list(output_id = "T-14-1-1", variable = "continuous", row = "Median",
         template = "{median}", digits = "1"),
    list(output_id = "T-14-1-1", variable = "continuous", row = "Min, Max",
         template = "{min}, {max}", digits = "0"),
    list(output_id = "T-14-2-1", variable = "continuous", row = "n",
         template = "{N}", digits = "0"),
    list(output_id = "T-14-2-1", variable = "continuous", row = "Mean (SD)",
         template = "{mean} ({sd})", digits = "1,2"),
    list(output_id = "T-14-2-1", variable = "continuous", row = "SE",
         template = "{se}", digits = "2"),
    list(output_id = "T-14-2-1", variable = "continuous",
         row = "95% CI of the mean", template = "{mean_lcl}, {mean_ucl}",
         digits = "1,1"),
    list(output_id = "T-14-2-1", variable = "continuous", row = "Median",
         template = "{median}", digits = "1"),
    list(output_id = "T-14-2-1", variable = "continuous", row = "Min, Max",
         template = "{min}, {max}", digits = "0"),
    # T-14-0-1: each fact's value as the ARD has it (text)
    list(output_id = "T-14-0-1", template = "{value}"),
    # T-14-1-4: the screen failures' age, as T-14-1-1's
    list(output_id = "T-14-1-4", variable = "continuous", row = "n",
         template = "{N}", digits = "0"),
    list(output_id = "T-14-1-4", variable = "continuous", row = "Mean (SD)",
         template = "{mean} ({sd})", digits = "1,2"),
    list(output_id = "T-14-1-4", variable = "continuous", row = "Median",
         template = "{median}", digits = "1"),
    list(output_id = "T-14-1-4", variable = "continuous", row = "Min, Max",
         template = "{min}, {max}", digits = "0"),
    # T-14-2-3: the CI analysis's numbers (its method and alternative, text
    # in the ARD, are not asked for)
    list(output_id = "T-14-2-3", variable = "continuous", row = "n",
         template = "{N}", digits = "0"),
    list(output_id = "T-14-2-3", variable = "continuous",
         row = "Mean (95% CI)", template = "{estimate} ({conf.low}, {conf.high})",
         digits = "1,1,1"),
    list(output_id = "T-14-2-3", variable = "continuous",
         row = "p-value", template = "{p.value}", digits = "3")),
  layout = tbl(
    list(blank_where = "between_groups", blank_first = "TRUE",
         blank_last = "TRUE", blank_counted = "TRUE", stub_name = "row_label",
         stub_before = "TRUE", note = "study default"),
    list(output_id = "T-14-1-1", pages_max_rows = "30",
         pages_split = "group_safe"),
    list(output_id = "T-14-1-2", pages_max_rows = "24"),
    list(output_id = "T-14-2-1", pages_max_rows = "30",
         pages_split = "group_safe"),
    list(output_id = "T-14-0-1", pages_max_rows = "24"),
    list(output_id = "T-14-1-4", pages_max_rows = "30",
         pages_split = "group_safe"),
    list(output_id = "T-14-1-3", pages_max_rows = "24"),
    list(output_id = "T-14-1-5", pages_max_rows = "30"),
    list(output_id = "T-14-1-6", pages_max_rows = "30"),
    list(output_id = "T-14-2-3", pages_max_rows = "24"),
    list(output_id = "T-14-3-1", pages_max_rows = "22",
         pages_split = "group_force")),
  columns = tbl(
    list(column = "row_label", rel_width = "40"),
    list(column = ".values", rel_width = "18")),
  style = tbl(
    list(align_count_pct = "TRUE")),
  col_header = tbl(
    list(line = "1", cols = "row_label"),
    list(line = "1", cols = ".values", span = "each", text = "{col}"),
    list(line = "2", cols = "row_label", text = "Characteristic"),
    list(line = "2", cols = ".values", span = "each", text = "(N={n})"),
    list(output_id = "T-14-3-1", line = "1", cols = "row_label"),
    list(output_id = "T-14-3-1", line = "1", cols = ".values", span = "each",
         text = "{col}\n(N={n})\nn (%)"),
    list(output_id = "T-14-3-1", line = "2", cols = "row_label",
         text = "System Organ Class\n   Preferred Term"),
    list(output_id = "T-14-3-1", line = "2", cols = ".values"),
    # T-14-0-1: no arms and no N -- the facts and their values
    list(output_id = "T-14-0-1", line = "1", cols = "row_label"),
    list(output_id = "T-14-0-1", line = "1", cols = ".values"),
    list(output_id = "T-14-0-1", line = "2", cols = "row_label", text = "Item"),
    list(output_id = "T-14-0-1", line = "2", cols = ".values", text = "Value"),
    # T-14-2-2: the arms over a blank stub (no "Characteristic" over KM rows)
    list(output_id = "T-14-2-2", line = "1", cols = "row_label"),
    list(output_id = "T-14-2-2", line = "1", cols = ".values", span = "each",
         text = "{col}"),
    list(output_id = "T-14-2-2", line = "2", cols = "row_label"),
    list(output_id = "T-14-2-2", line = "2", cols = ".values", span = "each",
         text = "(N={n})")))

# T-14-1-1S: T-14-1-1's table, row for row (its ARD differs: below)
for (sh in c("tables", "variables", "codelists", "cells", "layout")) {
  d <- sheets[[sh]]
  k <- !is.na(d$output_id) & d$output_id == "T-14-1-1"
  add <- d[k, , drop = FALSE]
  add$output_id <- "T-14-1-1S"
  sheets[[sh]] <- rbind(d, add)
}

# ----------------------------------------------------------- the report half
# each report's title and analysis set: its tokens, which the study's
# header says once ({OUTPUT_LABEL} is made from the id: Table 14.1.1)
title <- function(oid, text, set = "Safety Analysis Set") {
  list(list(output_id = oid, name = "OUTPUT_TITLE", value = text),
       list(output_id = oid, name = "OUTPUT_POPULATION", value = set))
}
sheets$report <- tbl(
  list(type = "table", file = "{output_id}.rtf", program = "{output_id}.R",
       note = "study default"),
  list(output_id = "L-16-2-7", type = "listing"),
  list(output_id = "F-14-2-1", type = "user"),
  list(output_id = "F-14-2-2", type = "user"),
  # its ARD is T-14-2-2's: it prints the medians that table has
  list(output_id = "F-14-2-3", type = "figure", ard_source = "table:T-14-2-2"),
  # its ARD is its own: the hazard ratios by subgroup (#293 phase 6)
  list(output_id = "F-14-2-4", type = "figure", ard_source = "own"))
sheets$page <- tbl(
  list(output_id = "L-16-2-7", orientation = "landscape"),
  list(output_id = "F-14-2-1", orientation = "landscape"),
  list(output_id = "F-14-2-2", orientation = "landscape"),
  list(output_id = "F-14-2-3", orientation = "landscape"),
  list(output_id = "F-14-2-4", orientation = "landscape"))
sheets$header <- tbl(
  list(line = "1", left = "Sample Pharma (tflplanner sample)", right = "DRAFT"),
  list(line = "2", left = "Protocol: SAMPLE-01 (CDISC pilot data)",
       right = "Page {PAGE} of {TOTAL_PAGES}"),
  list(line = "3"),
  list(line = "4", center = "{OUTPUT_LABEL}"),
  list(line = "5", center = "{OUTPUT_TITLE}"),
  list(line = "6", center = "<{OUTPUT_POPULATION}>"))
sheets$tokens <- do.call(tbl, c(
  title("T-14-1-1", "Demographic Characteristics"),
  title("T-14-1-1S", "Demographic Characteristics"),
  title("T-14-1-2", "Subject Disposition"),
  title("T-14-2-1", "Systolic Blood Pressure (mmHg): Change from Baseline at Week 24"),
  title("T-14-1-3", "Age Group and Sex"),
  title("T-14-0-1", "Study Information", set = "All Subjects"),
  title("T-14-1-4", "Demographic Characteristics of Screen Failures",
        set = "All Screen Failures"),
  title("T-14-1-5", "Race, with the Asian Sub-categories",
        set = "All Enrolled Subjects"),
  title("T-14-1-6", "Race and Asian Sub-categories",
        set = "All Enrolled Subjects"),
  title("T-14-2-3",
        "Systolic Blood Pressure (mmHg): Mean Change from Baseline at Week 24 (95% CI)"),
  title("T-14-3-1",
        "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term"),
  title("L-16-2-7", "Severe Treatment-Emergent Adverse Events"),
  title("F-14-2-1", "Mean (SE) Change from Baseline in Systolic Blood Pressure over Time"),
  title("T-14-2-2", "Time to First Dermatologic Event: Kaplan-Meier Estimates"),
  title("F-14-2-2", "Kaplan-Meier Plot of Time to First Dermatologic Event"),
  title("F-14-2-3", "Kaplan-Meier Curves of Time to First Dermatologic Event"),
  title("F-14-2-4", "Hazard Ratio of Time to First Dermatologic Event by Subgroup")))
sheets$footer <- tbl(
  list(line = "99", left = "{PROGRAM}       Generated on: {DATETIME}"),
  list(output_id = "T-14-1-1", line = "1",
       left = "SD = Standard Deviation."),
  list(output_id = "T-14-1-1S", line = "1",
       left = "SD = Standard Deviation."),
  list(output_id = "T-14-2-1", line = "1",
       left = "SD = Standard Deviation; SE = Standard Error; CI = Confidence Interval (t distribution)."),
  list(output_id = "T-14-0-1", line = "1",
       left = "The dictionary versions are the study's (data management plan); the dates are those of ADSL."),
  list(output_id = "T-14-1-4", line = "1",
       left = "SD = Standard Deviation.  Screen failures: subjects not randomized (ARM is Screen Failure)."),
  list(output_id = "T-14-1-5", line = "1",
       left = "Enrolled subjects: all subjects of ADSL, screen failures included.  Percentages of the column's subjects."),
  list(output_id = "T-14-1-5", line = "2",
       left = "Asian sub-categories (Chinese, Japanese, Korean) are derived for demonstration; they are not in the source data."),
  list(output_id = "T-14-1-6", line = "1",
       left = "Enrolled subjects: all subjects of ADSL, screen failures included.  Percentages of the column's subjects."),
  list(output_id = "T-14-1-6", line = "2",
       left = "Asian sub-categories (Chinese, Japanese, Korean) are derived for demonstration; they are not in the source data."),
  list(output_id = "T-14-2-3", line = "1",
       left = "CI = Confidence Interval.  95% CI and p-value: one-sample t-test of the change from baseline (H0: mean change = 0)."),
  list(output_id = "T-14-2-3", line = "2",
       left = "Systolic blood pressure after 5 minutes lying down."),
  list(output_id = "T-14-3-1", line = "1",
       left = "Subjects are counted once per system organ class and once per preferred term."),
  list(output_id = "F-14-2-1", line = "1",
       left = "Vertical bars: mean +/- SE.  Systolic blood pressure after 5 minutes lying down."),
  list(output_id = "T-14-2-1", line = "2",
       left = "Systolic blood pressure after 5 minutes lying down."),
  list(output_id = "T-14-2-2", line = "1",
       left = "Dermatologic event: first treatment-emergent skin and subcutaneous tissue disorder or application site event; censored at the end of treatment."),
  list(output_id = "T-14-2-2", line = "2",
       left = "Kaplan-Meier estimates; 95% CI of the median by the Brookmeyer-Crowley method (log transformation). NE = not estimable."),
  list(output_id = "F-14-2-2", line = "1",
       left = "x = censored.  The number at risk is that of Table 14.2.2."),
  list(output_id = "F-14-2-3", line = "1",
       left = "x = censored.  Dashed line: the median (probability 0.5).  The medians are those of Table 14.2.2; the hazard ratios (Cox model) are in its ARD."),
  list(output_id = "F-14-2-4", line = "1",
       left = "Hazard ratio of Xanomeline High Dose vs Placebo (Cox model, unstratified) with its 95% CI, overall and within each subgroup; the figure's own ARD.  NE = not estimable."),
  list(line = "98",
       left = "Source: CDISC pilot study ADaM data of the pharmaverseadam R package."))

p <- new_planner(c(rounding = "sas"))
for (s in names(sheets)) p$sheets[[s]] <- .normalize_sheet(sheets[[s]], s)

# ---------------------------------------------------------- the study ARD
p$ard$datasets <- tbl(
  list(dataset = "ADSL", level = "ADaM", path = "data/adam/adsl.rds"),
  list(dataset = "ADAE", level = "ADaM", path = "data/adam/adae.rds"),
  list(dataset = "ADVS", level = "ADaM", path = "data/adam/advs.rds"),
  list(dataset = "ADTTE", level = "ADaM", path = "data/adam/adtte.rds"))
p$ard$populations <- tbl(
  list(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\"",
       derive = "TRTA = TRT01A"),
  # the screen failures: an analysis set by a condition alone (ADSL has no
  # flag for them)
  list(population_id = "SCRF", dataset = "ADSL",
       where = "ARM == \"Screen Failure\""),
  # every subject of ADSL (no condition): the enrolled subjects
  list(population_id = "ENR", dataset = "ADSL"))
p$ard$analyses <- tbl(
  list(output_id = "T-14-1-1", analysis_id = "GROUPN", label = "Subjects per group",
       method = "categorical", data = "adsl_saf", variables = "TRT01A"),
  list(output_id = "T-14-1-1", analysis_id = "CONT", label = "Continuous",
       method = "continuous", data = "adsl_saf", by = "TRT01A",
       variables = "AGE", statistics = "N | mean | sd | median | min | max"),
  list(output_id = "T-14-1-1", analysis_id = "CAT", label = "Categorical",
       method = "categorical", data = "adsl_saf", by = "TRT01A",
       variables = "AGEGR1 | SEX | RACE | ETHNIC", statistics = "n | p"),
  # T-14-1-1's analyses as one cards::ard_stack() call: its data, analysis
  # set and groups once, the analyses inside it (`parent`); the column N
  # (.by_stats, cards' default) is its own
  list(output_id = "T-14-1-1S", analysis_id = "STACK",
       label = "Demographics, run together", method = "cards::ard_stack",
       data = "adsl_saf", by = "TRT01A"),
  list(output_id = "T-14-1-1S", analysis_id = "CONT", parent = "STACK",
       label = "Continuous", method = "continuous", variables = "AGE",
       statistics = "N | mean | sd | median | min | max"),
  list(output_id = "T-14-1-1S", analysis_id = "CAT", parent = "STACK",
       label = "Categorical", method = "categorical",
       variables = "AGEGR1 | SEX | RACE | ETHNIC", statistics = "n | p"),
  list(output_id = "T-14-1-2", analysis_id = "GROUPN", method = "categorical",
       data = "adsl_saf", variables = "TRT01A"),
  list(output_id = "T-14-1-2", analysis_id = "DISP",
       label = "Status at end of study", method = "categorical",
       data = "adsl_saf", by = "TRT01A", variables = "EOSSTT",
       statistics = "n | p"),
  list(output_id = "T-14-2-1", analysis_id = "GROUPN", method = "categorical",
       data = "adsl_saf", variables = "TRTA"),
  list(output_id = "T-14-2-1", analysis_id = "SYSBP",
       label = "SYSBP at Week 24", method = "continuous", data = "advs_w24", by = "TRTA",
       variables = "BASE | AVAL | CHG",
       statistics = "N | mean | sd | se | mean_lcl | mean_ucl | median | min | max"),
  # T-14-0-1: the study's information -- no analysis set (it counts no
  # subjects): the dictionary versions, written here, and the dates of the
  # data, from ADSL; a fact a level of one variable (INFO), its value text
  list(output_id = "T-14-0-1", analysis_id = "INFO",
       label = "Study information", method = "custom", dataset = "ADSL",
       code = paste(
         "facts <- c(",
         "  \"MedDRA version\" = \"Version 26.1\",",
         "  \"WHODrug version\" = \"Global B3 March 2023\",",
         "  \"First dose\" = format(min(data$TRTSDT, na.rm = TRUE)),",
         "  \"Last dose\" = format(max(data$TRTEDT, na.rm = TRUE)),",
         "  \"Data cut-off\" = format(max(data$LSTALVDT, na.rm = TRUE))",
         ")",
         "cards::as_card(dplyr::tibble(",
         "  variable = \"INFO\", variable_level = as.list(names(facts)),",
         "  context = \"study_info\", stat_name = \"value\", stat_label = \"Value\",",
         "  stat = as.list(unname(facts)), fmt_fun = list(NULL),",
         "  warning = list(NULL), error = list(NULL)",
         "))", sep = "\n")),
  # T-14-1-4: the screen failures (SCRF, an analysis set by a condition)
  list(output_id = "T-14-1-4", analysis_id = "GROUPN", method = "categorical",
       data = "adsl_scrf", variables = "ARM"),
  list(output_id = "T-14-1-4", analysis_id = "CONT", label = "Continuous",
       method = "continuous", data = "adsl_scrf", by = "ARM",
       variables = "AGE", statistics = "N | mean | sd | median | min | max"),
  list(output_id = "T-14-1-4", analysis_id = "CAT", label = "Categorical",
       method = "categorical", data = "adsl_scrf", by = "ARM",
       variables = "AGEGR1 | SEX | RACE", statistics = "n | p"),
  # T-14-1-3: cards' default statistics of a count (n, N and p) -- the
  # table's cells print n (%) and leave the N rows out
  list(output_id = "T-14-1-3", analysis_id = "GROUPN", method = "categorical",
       data = "adsl_saf", variables = "TRT01A"),
  list(output_id = "T-14-1-3", analysis_id = "CAT",
       label = "Age group and sex", method = "categorical",
       data = "adsl_saf", by = "TRT01A", variables = "AGEGR1 | SEX"),
  # T-14-2-3: the mean change and its 95% CI by a one-sample t-test, every
  # statistic the function gives (the test's method and alternative are
  # text in the ARD)
  list(output_id = "T-14-2-3", analysis_id = "GROUPN", method = "categorical",
       data = "adsl_saf", variables = "TRTA"),
  list(output_id = "T-14-2-3", analysis_id = "N", label = "Subjects",
       method = "continuous", data = "advs_w24", by = "TRTA",
       variables = "CHG", statistics = "N"),
  list(output_id = "T-14-2-3", analysis_id = "CI",
       label = "Mean change (95% CI), one-sample t-test", method = "mean_ci",
       data = "advs_w24", by = "TRTA", variables = "CHG"),
  list(output_id = "T-14-2-2", analysis_id = "GROUPN", method = "categorical",
       data = "adsl_saf", variables = "TRT01A"),
  list(output_id = "T-14-2-2", analysis_id = "KM",
       label = "Kaplan-Meier estimates", method = "custom", data = "adtte_ttde",
       code = paste(
         "fit <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRT01A, data = data)",
         "cards::bind_ard(",
         "  cardx::ard_survival_survfit(fit, probs = 0.5),",
         "  cardx::ard_survival_survfit(fit, times = c(0, 30, 60, 90, 120, 150, 180)),",
         "  .quiet = TRUE)", sep = "\n")),
  # in the ARD, not in the table (its cells name prob and time only): the
  # hazard ratios against placebo, which F-14-2-3 prints (#311)
  list(output_id = "T-14-2-2", analysis_id = "HR",
       label = "Hazard ratio vs placebo (Cox), not printed in the table",
       method = "custom", data = "adtte_ttde",
       code = paste(
         "cardx::ard_regression(",
         "  survival::coxph(survival::Surv(AVAL, 1 - CNSR) ~ TRT01A, data = data),",
         "  exponentiate = TRUE)", sep = "\n")),
  # T-14-1-5 and T-14-1-6: race and its Asian sub-categories of the
  # enrolled subjects -- the same analyses, laid out nested (T-14-1-5) or
  # in two blocks (T-14-1-6)
  list(output_id = "T-14-1-5", analysis_id = "GROUPN", method = "categorical",
       data = "adsl_enr_tot", variables = "ARM"),
  list(output_id = "T-14-1-5", analysis_id = "RACE", label = "Race",
       method = "categorical", data = "adsl_enr_tot", by = "ARM",
       variables = "RACE", denominator = "adsl_enr_tot", statistics = "n | p"),
  list(output_id = "T-14-1-5", analysis_id = "RACESUB", label = "Race Sub Asian",
       method = "categorical", data = "adsl_enr_tot", by = "ARM",
       variables = "RACESUB", denominator = "adsl_enr_tot", statistics = "n | p"),
  # T-14-1-6: race and its Asian sub-categories of the enrolled subjects
  # (adsl_enr_tot: each subject in their arm and in Total), percentages of
  # the column's subjects
  list(output_id = "T-14-1-6", analysis_id = "GROUPN", method = "categorical",
       data = "adsl_enr_tot", variables = "ARM"),
  list(output_id = "T-14-1-6", analysis_id = "RACE", label = "Race",
       method = "categorical", data = "adsl_enr_tot", by = "ARM",
       variables = "RACE", denominator = "adsl_enr_tot", statistics = "n | p"),
  list(output_id = "T-14-1-6", analysis_id = "RACESUB", label = "Race Sub Asian",
       method = "categorical", data = "adsl_enr_tot", by = "ARM",
       variables = "RACESUB", denominator = "adsl_enr_tot", statistics = "n | p"),
  # T-14-3-1 reads analysis data: the TEAEs of the safety set (adae_saf,
  # kept to adsl_saf's subjects), its percents of adsl_saf
  list(output_id = "T-14-3-1", analysis_id = "TEAE",
       label = "TEAE by SOC / PT", method = "hierarchical", data = "adae_saf",
       by = "TRT01A", variables = "AEBODSYS | AEDECOD", denominator = "adsl_saf",
       args = "over_variables = TRUE"))
# every table reads analysis data -- each table's own (an analysis data is
# a report's): the safety set (adsl_saf) in each, and the records of other
# datasets kept to its subjects where a table reads them
saf <- function(o) list(output_id = o, data_id = "adsl_saf", label = "Safety set",
                        from = "ADSL", population_id = "SAF")
p$ard$analysis_data <- tbl(
  saf("T-14-1-1"), saf("T-14-1-1S"), saf("T-14-1-2"), saf("T-14-1-3"),
  list(output_id = "T-14-1-4", data_id = "adsl_scrf", label = "Screen failures",
       from = "ADSL", population_id = "SCRF"),
  list(output_id = "T-14-1-5", data_id = "adsl_enr", label = "Enrolled subjects",
       from = "ADSL", population_id = "ENR",
       derive = paste0(
         "RACESUB = dplyr::case_when(USUBJID == \"01-703-1396\" ~ \"CHINESE\", ",
         "USUBJID == \"01-708-1104\" ~ \"JAPANESE\")")),
  list(output_id = "T-14-1-5", data_id = "adsl_enr_tot",
       label = "Enrolled subjects, and all of them again as Total",
       from = "adsl_enr",
       code = "dplyr::bind_rows(adsl_enr, dplyr::mutate(adsl_enr, ARM = \"Total\"))"),
  # T-14-1-6: the enrolled subjects with a demonstration sub-race of Asian
  # (the pilot data has none: derived, by subject, for the sample only),
  # then each subject again in a Total column
  list(output_id = "T-14-1-6", data_id = "adsl_enr", label = "Enrolled subjects",
       from = "ADSL", population_id = "ENR",
       derive = paste0(
         "RACESUB = dplyr::case_when(USUBJID == \"01-703-1396\" ~ \"CHINESE\", ",
         "USUBJID == \"01-708-1104\" ~ \"JAPANESE\")")),
  list(output_id = "T-14-1-6", data_id = "adsl_enr_tot",
       label = "Enrolled subjects, and all of them again as Total",
       from = "adsl_enr",
       code = "dplyr::bind_rows(adsl_enr, dplyr::mutate(adsl_enr, ARM = \"Total\"))"),
  saf("T-14-2-1"),
  list(output_id = "T-14-2-1", data_id = "advs_w24",
       label = "Systolic blood pressure at Week 24, safety set",
       from = "ADVS", subjects = "adsl_saf",
       where = "PARAMCD == \"SYSBP\" & AVISIT == \"Week 24\""),
  saf("T-14-2-3"),
  list(output_id = "T-14-2-3", data_id = "advs_w24",
       label = "Systolic blood pressure at Week 24, safety set",
       from = "ADVS", subjects = "adsl_saf",
       where = "PARAMCD == \"SYSBP\" & AVISIT == \"Week 24\""),
  saf("T-14-2-2"),
  list(output_id = "T-14-2-2", data_id = "adtte_ttde",
       label = "Time to first dermatologic event, safety set",
       from = "ADTTE", subjects = "adsl_saf", where = "PARAMCD == \"TTDE\""),
  saf("T-14-3-1"),
  list(output_id = "T-14-3-1", data_id = "adae_saf",
       label = "Treatment-emergent AEs of the safety set",
       from = "ADAE", subjects = "adsl_saf", where = "TRTEMFL == \"Y\""))
for (sh in names(p$ard)) p$ard[[sh]] <- .normalize_ard_sheet(p$ard[[sh]], sh)

# ------------------------------------------------ the listing, the figure
p$lf$listings <- .normalize_lf_sheet(tbl(
  list(output_id = "L-16-2-7", type = "multiline", dataset = "ADAE",
       where = "AESEV == \"SEVERE\" & TRTEMFL == \"Y\"",
       sort = "TRT01AN | USUBJID | ASTDT",
       max_rows = "22")), "listings")
p$lf$listing_cols <- .normalize_lf_sheet(tbl(
  list(output_id = "L-16-2-7", vars = "TRT01A", label = "Treatment",
       width = "22"),
  list(output_id = "L-16-2-7", vars = "USUBJID", label = "Subject",
       width = "14", collapse_repeats = "TRUE"),
  list(output_id = "L-16-2-7", vars = "AEBODSYS | AEDECOD",
       label = "System Organ Class/\\nPreferred Term", width = "40"),
  list(output_id = "L-16-2-7", vars = "ASTDT | AENDT", label = "Start/\\nEnd",
       width = "14"),
  list(output_id = "L-16-2-7", vars = "AESER", label = "Serious", width = "8"),
  list(output_id = "L-16-2-7", vars = "AEREL", label = "Relation", width = "9"),
  list(output_id = "L-16-2-7", vars = "AEOUT", label = "Outcome", width = "24")),
  "listing_cols")
p$lf$figures <- .normalize_lf_sheet(tbl(
  list(output_id = "F-14-2-1", datasets = "ADSL | ADVS"),
  list(output_id = "F-14-2-2", datasets = "ADTTE")), "figures")

# F-14-2-3, as the designer makes it: the KM template on the study's ADTTE
# (its parameter, analysis set, group and time unit), then layers added --
# the median line, and the medians printed from T-14-2-2's ARD (its KM
# analysis: prob 0.5, the estimate by arm; NE when not reached)
km_design <- tflspec::tfl_fig_template("km_simple", data = "ADTTE",
                                       param = "TTDE", pop = "SAFFL",
                                       group = "TRT01A", time_unit = "days")
km_design$plot$y_label <- "Probability of No Dermatologic Event"
km_design$layers <- c(km_design$layers, list(list(
  layer = "hline", yintercept = 0.5, linetype = "dashed", colour = "grey50",
  linewidth = 0.3)))
arms <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
km_design$layers <- c(km_design$layers, lapply(seq_along(arms), function(i) list(
  layer = "ard_number", analysis_id = "KM", variable = "prob", level = 0.5,
  stat = "estimate", group = paste("TRT01A =", arms[i]),
  label = paste0("Median (days), ", arms[i], ": {value}"), digits = 0,
  x = 0, y = "-Inf", hjust = 0, vjust = -0.6 - 1.5 * (length(arms) - i))))
# and the hazard ratios (T-14-2-2's HR analysis, in its ARD and not in the
# table), each with its CI on one line, right, between the curves
km_design$layers <- c(km_design$layers, lapply(2:3, function(i) list(
  layer = "ard_number", analysis_id = "HR", variable = "TRT01A", level = arms[i],
  stat = "estimate", label = paste0("HR, ", arms[i], " vs Placebo: {value} (95% CI {conf.low}, {conf.high})"),
  digits = 2, x = "Inf", y = 0.3, hjust = 1.02, vjust = -0.6 - 1.5 * (3 - i))))

plot_code <- c(
  "library(ggplot2)",
  "saf <- adsl$USUBJID[adsl$SAFFL == \"Y\"]",
  "d <- advs[advs$PARAMCD == \"SYSBP\" & advs$USUBJID %in% saf &",
  "            advs$AVISITN > 0 & !is.na(advs$CHG), ]",
  "m <- aggregate(CHG ~ TRTA + AVISITN, data = d, FUN = function(x)",
  "  c(mean = mean(x), se = stats::sd(x) / sqrt(length(x))))",
  "m <- data.frame(m[c(\"TRTA\", \"AVISITN\")], m$CHG)",
  "arms <- c(\"Placebo\", \"Xanomeline Low Dose\", \"Xanomeline High Dose\")",
  "m$TRTA <- factor(m$TRTA, levels = arms)",
  "plot <- ggplot(m, aes(AVISITN, mean, colour = TRTA)) +",
  "  geom_hline(yintercept = 0, colour = tfl_setting(\"zero_line_colour\"),",
  "             linewidth = tfl_num(\"line_width\")) +",
  "  geom_line(position = position_dodge(width = 0.6), linewidth = tfl_num(\"line_width\")) +",
  "  geom_point(position = position_dodge(width = 0.6)) +",
  "  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.4,",
  "                position = position_dodge(width = 0.6)) +",
  "  scale_colour_tfl(\"treatment\", arms) +",
  "  scale_x_continuous(breaks = sort(unique(m$AVISITN))) +",
  "  labs(x = \"Week\", y = \"Mean change from baseline (mmHg)\",",
  "       colour = NULL) +",
  "  theme_tfl() + theme(legend.position = \"bottom\")",
  "tfl_check(plot)",
  "content <- plot")

km_code <- c(
  "library(ggplot2)",
  "library(ggsurvfit)",
  "library(patchwork)",
  "arms <- c(\"Placebo\", \"Xanomeline Low Dose\", \"Xanomeline High Dose\")",
  "km_df <- adtte[adtte$PARAMCD == \"TTDE\" & adtte$SAFFL == \"Y\", ]",
  "km_df$TRT01A <- factor(km_df$TRT01A, levels = arms)",
  "km_fit <- survfit2(Surv(AVAL, 1 - CNSR) ~ TRT01A, data = km_df)",
  "",
  "# the number at risk: the KM table's (T-14-2-2), from the study ARD --",
  "# checked against the curve's own count",
  "km_ard <- readRDS(file.path(path_ard, \"ard.rds\"))",
  "risk_df <- tfl_km_risk(km_ard[km_ard$output_id == \"T-14-2-2\", ], km_fit)",
  "x_breaks <- sort(unique(risk_df$time))",
  "",
  "pal <- tfl_colours(\"treatment\", arms)",
  "p_km <- ggsurvfit(km_fit, linewidth = tfl_num(\"line_width\", \"km\")) +",
  "  add_censor_mark(shape = tfl_marker(\"censor\")$shape,",
  "                  size = tfl_num(\"censor_size\", \"km\"),",
  "                  stroke = tfl_num(\"censor_stroke\", \"km\")) +",
  "  geom_hline(yintercept = 0.5, linetype = tfl_setting(\"median_linetype\", \"km\"),",
  "             colour = tfl_setting(\"median_colour\", \"km\"),",
  "             linewidth = tfl_num(\"median_line_width\", \"km\")) +",
  "  scale_colour_manual(values = pal, breaks = names(pal)) +",
  "  scale_x_continuous(breaks = x_breaks) +",
  "  scale_y_continuous(breaks = seq(0, 1, by = tfl_num(\"y_by\", \"km\"))) +",
  "  coord_cartesian(xlim = range(x_breaks), ylim = c(0, 1)) +",
  "  labs(x = \"Days since first dose\",",
  "       y = \"Probability without a dermatologic event\", colour = NULL) +",
  "  theme_tfl(\"km\") + theme(legend.position = \"bottom\")",
  "p_risk <- ggplot(risk_df, aes(time, factor(strata, levels = rev(arms)),",
  "                              label = n_risk, colour = strata)) +",
  "  geom_text(size = tfl_num(\"text_size\")) +",
  "  scale_colour_manual(values = pal, guide = \"none\") +",
  "  scale_x_continuous(breaks = x_breaks) +",
  "  coord_cartesian(xlim = range(x_breaks)) +",
  "  labs(title = tfl_setting(\"risk_title\", \"km\"), x = NULL, y = NULL) +",
  "  theme_void(base_size = tfl_num(\"base_size\")) +",
  "  theme(plot.title = element_text(hjust = 0, size = rel(0.9)),",
  "        axis.text.y = element_text(hjust = 1, margin = margin(r = 5)))",
  "plot <- p_km / p_risk +",
  "  plot_layout(heights = c(1, tfl_num(\"risk_height\", \"km\")))",
  "tfl_check(plot)",
  "content <- plot")

desc <- c("T-14-0-1" = "Study information: dictionary versions and the dates of the data (no analysis set)",
          "T-14-1-4" = "Demographic characteristics of the screen failures (an analysis set by a condition alone)",
          "T-14-1-5" = "Race with its Asian sub-categories nested under Asian, one block (enrolled subjects by arm, Total; the sub-categories derived for demonstration)",
          "T-14-1-6" = "Race and its Asian sub-categories, two blocks (enrolled subjects by arm, Total; the sub-categories derived for demonstration)",
          "T-14-1-1" = "Demographic characteristics",
          "T-14-1-1S" = "Demographic characteristics (its ARD one ard_stack call)",
          "T-14-1-2" = "Subject disposition",
          "T-14-2-1" = "Systolic blood pressure: change from baseline at Week 24",
          "T-14-1-3" = paste("Age group and sex: the ARD keeps cards' default n, N",
                             "and p; the table prints n (%) and leaves the N rows out"),
          "T-14-2-3" = paste("Systolic blood pressure: mean change at Week 24 (95% CI),",
                             "p-value; the ARD also holds the test's method and",
                             "alternative as text"),
          "T-14-3-1" = "TEAEs by SOC / PT",
          "T-14-2-2" = "Time to first dermatologic event: KM estimates",
          "L-16-2-7" = "Listing of severe treatment-emergent adverse events",
          "F-14-2-1" = "Mean change from baseline in systolic blood pressure",
          "F-14-2-2" = "KM plot of the time to first dermatologic event",
          "F-14-2-3" = "KM curves of the time to first dermatologic event (designed)",
          "F-14-2-4" = "Hazard ratio of the time to first dermatologic event by subgroup (forest plot, its own ARD)")
types <- c("T-14-0-1" = "table", "T-14-1-4" = "table", "T-14-1-5" = "table", "T-14-1-6" = "table", "T-14-1-1" = "table", "T-14-1-1S" = "table", "T-14-1-2" = "table",
           "T-14-1-3" = "table", "T-14-2-3" = "table", "T-14-2-1" = "table", "T-14-3-1" = "table", "T-14-2-2" = "table", "L-16-2-7" = "listing",
           "F-14-2-1" = "user", "F-14-2-2" = "user", "F-14-2-3" = "figure", "F-14-2-4" = "figure")
process <- list(
  "T-14-2-2" = c(
    "data <- normalize_ard(ard)",
    "# row labels: the median, and the event-free probability by day (day 0 left out)",
    "is_time <- data$variable == \"time\"",
    "data$.label[data$variable == \"prob\"] <- \"Median (95% CI)\"",
    "data$.label[is_time] <- paste(\"Day\", as.character(data$variable_level[is_time]))",
    "data <- data[!(is_time & as.character(data$variable_level) == \"0\"), ]"),
  "T-14-3-1" = c(
    "data <- normalize_ard(ard, hierarchy = c(\"AEBODSYS\", \"AEDECOD\"),",
    "                      overall = \"Any TEAE\")"))
for (o in names(desc)) {
  p <- add_output(p, o, description = desc[[o]],
                  data_code = if (o == "F-14-2-1")
                    paste(plot_code, collapse = "\n") else if (o == "F-14-2-2")
                    paste(km_code, collapse = "\n") else NA,
                  process_code = if (!is.null(process[[o]]))
                    paste(process[[o]], collapse = "\n") else NA,
                  type = types[[o]])
}
p <- set_fig_design(p, "F-14-2-3", km_design)
# F-14-2-4: the forest template, whose numbers are the figure's own ARD --
# a Cox model of all subjects and one within each subgroup (its analyses
# come with the design; the population is the study's SAF)
forest_design <- tflspec::tfl_fig_template(
  "forest_hr", data = "ADTTE", param = "TTDE", pop = "SAFFL", group = "TRT01A",
  subgroups = "SEX, AGEGR1", comparison = "Xanomeline High Dose")
forest_design$plot$x_label <- "Hazard Ratio (95% CI), Xanomeline High Dose vs Placebo"
p <- set_fig_design(p, "F-14-2-4", forest_design)
p <- set_fig_own_analyses(p, "F-14-2-4", attr(forest_design, "analyses"),
                          population_id = "SAF")
# the tables' analysis set (the report list's), as step 2 sets it
for (o in setdiff(unique(p$ard$analyses$output_id), c("T-14-0-1", "T-14-1-4", "T-14-1-5", "T-14-1-6"))) {
  p <- set_report_population(p, o, "SAF")
}
p <- set_report_population(p, "T-14-1-4", "SCRF")
p <- set_report_population(p, "T-14-1-5", "ENR")
p <- set_report_population(p, "T-14-1-6", "ENR")

# ------------------------------------------------------------ the study
s <- create_study(id, title = "Sample study (CDISC pilot data, pharmaverseadam)",
                  compound = "Xanomeline", phase = "2",
                  description = paste(
                    "The sample study of tflplanner: the CDISC pilot ADaM",
                    "data of the pharmaverseadam package; one study ARD and",
                    "tables, a listing and a figure made from it."),
                  planner = p)
adam <- file.path(s$path, study_layout()[["adam"]])
saveRDS(strip(adsl), file.path(adam, "adsl.rds"))
saveRDS(strip(adae), file.path(adam, "adae.rds"))
saveRDS(strip(advs), file.path(adam, "advs.rds"))
saveRDS(strip(adtte), file.path(adam, "adtte.rds"))

b <- run_batch(s, c("ard", "tfl"), code = FALSE)
cat(b$output, sep = "\n")
if (!b$ok) {
  for (f in file.path(b$batch, b$result$log[b$result$status != "OK"])) {
    cat("\n====", f, "\n")
    cat(utils::tail(readLines(f, warn = FALSE), 40), sep = "\n")
  }
  quit(status = 1L)
}

# ------------------------------------------------------- into the package
out <- file.path("inst", "sample", id)
unlink(out, recursive = TRUE)
dir.create(out, recursive = TRUE)
keep <- c(.study_file, paste0(id, ".Rproj"),
          file.path(study_layout()[["spec"]],
                    c(.table_file, .report_file, .ard_json, .lf_file,
                      file.path(.fig_design_dir, c("F-14-2-3.yml", "F-14-2-4.yml")))),
          file.path(study_layout()[["adam"]],
                    c("adsl.rds", "adae.rds", "advs.rds", "adtte.rds")))
for (f in keep) {
  dir.create(file.path(out, dirname(f)), recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(file.path(s$path, f), file.path(out, f)))
}
writeLines(c(
  "# SAMPLE-01: the sample study of tflplanner",
  "",
  "The CDISC pilot study (Xanomeline) as the ADaM datasets of the",
  "[pharmaverseadam](https://pharmaverse.github.io/pharmaverseadam/) R package",
  "(Apache License 2.0): ADSL, ADAE, ADVS (systolic blood pressure after 5",
  "minutes lying down) and ADTTE (the time to the first dermatologic event,",
  "derived from ADSL and ADAE by data-raw/make-sample-study.R).",
  "",
  "ITTFL, EFFFL and PPROTFL are not in pharmaverseadam's ADSL (of the",
  "population flags it has SAFFL only); data-raw/make-sample-study.R",
  "derives them, so step 2 has more than one flag to choose from:",
  "",
  "| Flag | Label | Y when | Subjects |",
  "|---|---|---|---|",
  "| ITTFL | Intent-To-Treat Population Flag | randomized (ARM is not Screen Failure) | 254 |",
  "| EFFFL | Efficacy Population Flag | SAFFL is Y and a post-baseline systolic blood pressure (CHG) in ADVS | 230 |",
  "| PPROTFL | Per-Protocol Population Flag | SAFFL is Y and EOSSTT is COMPLETED | 110 |",
  "",
  "They are not analysis sets of the study (its populations sheet has SAF,",
  "and SCRF -- the screen failures, by ARM alone): in step 2-1 they are",
  "offered as flags of ADSL, to make one.",
  "",
  "| Output | Type | |",
  "|---|---|---|",
  "| T-14-0-1 | Table | Study information: dictionary versions and the dates of the data -- no analysis set (it counts no subjects) |",
  "| T-14-1-1 | Table | Demographic characteristics |",
  "| T-14-1-1S | Table | The same table, its ARD one `cards::ard_stack()` call (the analyses inside it, and the column N it makes) |",
  "| T-14-1-2 | Table | Subject disposition |",
  "| T-14-1-4 | Table | Demographic characteristics of the screen failures: an analysis set by a condition alone (SCRF, ARM is Screen Failure; ADSL has no flag for it) |",
  "| T-14-1-3 | Table | Age group and sex: the ARD keeps cards' default n, N and p, the table prints n (%) (the N rows its cells do not name are left out) |",
  "| T-14-2-1 | Table | Systolic blood pressure: change from baseline at Week 24 (SE, 95% CI of the mean) |",
  "| T-14-2-3 | Table | Systolic blood pressure: mean change at Week 24 (95% CI) and p-value of a one-sample t-test (the ARD also holds the test's method and alternative as text) |",
  "| T-14-2-2 | Table | Time to first dermatologic event: Kaplan-Meier estimates |",
  "| T-14-3-1 | Table | TEAEs by SOC / PT |",
  "| L-16-2-7 | Listing | Severe treatment-emergent adverse events |",
  "| F-14-2-1 | User code (a figure) | Mean change from baseline in systolic blood pressure |",
  "| F-14-2-2 | User code (a figure) | Kaplan-Meier plot of the time to first dermatologic event (number at risk from T-14-2-2's ARD) |",
  "| F-14-2-3 | Figure (designed) | The same KM curves from the designer's KM template, with a median line added |",
  "| F-14-2-4 | Figure (designed) | Forest plot of the hazard ratio by subgroup from the figure's own ARD (a Cox model overall and within each subgroup, cards / cardx) |",
  "",
  "The tables are made from one study ARD (programs/ard/), the listing and",
  "the figures from the ADaM data, in the figure style of the company",
  "standards (programs/tfl/fig_setup.R).  Made by data-raw/make-sample-study.R",
  "of the tflplanner repository."), file.path(out, "README.md"))
cat("\nWrote", out, "\n")
print(file.info(list.files(out, recursive = TRUE, full.names = TRUE))["size"])
