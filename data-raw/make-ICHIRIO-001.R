# The test study ICHIRIO-001: every kind of report tflplanner knows, on the
# CDISC pilot ADaM data that ships with {cards}.
#
#   T-14-1-1  Table    Demographic characteristics         table_spec.xlsx
#   T-14-1-2  Table    Subject disposition                 table_spec.xlsx
#   T-14-3-1  Table    TEAEs by SOC / PT, frequency desc   table_spec.xlsx
#   L-16-2-7  Listing  Severe adverse events               program
#   F-14-2-1  Figure   Kaplan-Meier, time to first derm.  program
#
#   Rscript data-raw/make-ICHIRIO-001.R [root]
#
# A development test study: it needs the development home (see
# data-raw/dev-home.R).  root: where the study folder goes (default: the
# development home's studies_root).  The study is written from scratch: an existing
# ICHIRIO-001 -- its folder and its saved state in tflplanner's home -- is
# replaced.  Afterwards every report is run and must end "ok".

devtools::load_all(quiet = TRUE)
source("data-raw/dev-home.R")   # the development home, never a user's
args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[1L] else studies_root()
id <- "ICHIRIO-001"

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
arms <- "Placebo | Xanomeline Low Dose | Xanomeline High Dose"

# ------------------------------------------------------------ the table half
sheets <- list(
  tables = tbl(
    list(output_id = "T-14-1-1", cols = "TRT01A", rows = "group = variable",
         note = "Demographics"),
    list(output_id = "T-14-1-2", cols = "TRT01A", rows = "group = variable",
         note = "Disposition: reason for discontinuation"),
    list(output_id = "T-14-3-1", cols = "TRTA", rows = "group1 = AEBODSYS",
         label = "label = AEDECOD",
         sort = ".overall | group1 | .depth | -n | label",
         note = "TEAE by SOC / PT, frequency descending")),
  variables = tbl(
    list(output_id = "T-14-1-1", variable = "TRT01A", levels = arms),
    list(output_id = "T-14-1-1", variable = "AGE", label = "Age (years)",
         order = 1),
    list(output_id = "T-14-1-1", variable = "AGEGR1",
         label = "Age group, n (%)", order = 2, levels = "<65 | 65-80 | >80"),
    list(output_id = "T-14-1-1", variable = "SEX", label = "Sex, n (%)",
         order = 3, levels = "F | M"),
    list(output_id = "T-14-1-1", variable = "RACE", label = "Race, n (%)",
         order = 4),
    list(output_id = "T-14-1-1", variable = "WEIGHTBL",
         label = "Baseline weight (kg)", order = 5),
    list(output_id = "T-14-1-1", variable = "HEIGHTBL",
         label = "Baseline height (cm)", order = 6),
    list(output_id = "T-14-1-2", variable = "TRT01A", levels = arms),
    list(output_id = "T-14-1-2", variable = "DCDECOD",
         label = "Status at end of study, n (%)", order = 1,
         levels = paste("COMPLETED | ADVERSE EVENT | DEATH |",
                        "LACK OF EFFICACY | LOST TO FOLLOW-UP |",
                        "PHYSICIAN DECISION | PROTOCOL VIOLATION |",
                        "STUDY TERMINATED BY SPONSOR | WITHDRAWAL BY SUBJECT")),
    list(output_id = "T-14-3-1", variable = "TRTA", levels = arms)),
  cells = tbl(
    list(template = "{n:.0f} ({p:.1f%})", note = "every categorical cell"),
    list(output_id = "T-14-1-1", variable = "continuous", row = "n",
         template = "{N}", digits = "0"),
    list(output_id = "T-14-1-1", variable = "continuous", row = "Mean (SD)",
         template = "{mean} ({sd})", digits = "1,2"),
    list(output_id = "T-14-1-1", variable = "continuous", row = "Median",
         template = "{median}", digits = "1"),
    list(output_id = "T-14-1-1", variable = "continuous", row = "Min, Max",
         template = "{min}, {max}", digits = "0")),
  layout = tbl(
    list(blank_where = "between_groups", blank_first = "TRUE",
         blank_last = "TRUE", blank_counted = "TRUE", stub_into = "row_label",
         stub_before = "TRUE", note = "study default"),
    list(output_id = "T-14-1-1", pages_max_rows = "24",
         pages_split = "group_safe"),
    list(output_id = "T-14-1-2", pages_max_rows = "24"),
    list(output_id = "T-14-3-1", pages_max_rows = "22",
         pages_split = "group_force")),
  columns = tbl(
    list(column = "row_label", width = "40"),
    list(column = ".values", width = "18")),
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
    list(output_id = "T-14-3-1", line = "2", cols = ".values")))

# ----------------------------------------------------------- the report half
title <- function(oid, number, text, set) {
  list(list(output_id = oid, line = "3"),
       list(output_id = oid, line = "4", center = number),
       list(output_id = oid, line = "5", center = text),
       list(output_id = oid, line = "6", center = set))
}
sheets$report <- tbl(
  list(type = "table", file = "{output_id}.rtf", program = "{output_id}.R",
       note = "study default"),
  list(output_id = "L-16-2-7", type = "listing"),
  list(output_id = "F-14-2-1", type = "figure"))
sheets$page <- tbl(
  list(output_id = "L-16-2-7", orientation = "landscape"),
  list(output_id = "F-14-2-1", orientation = "landscape"))
sheets$header <- do.call(tbl, c(
  list(list(line = "1", left = "ICHIRIO Pharma", right = "DRAFT"),
       list(line = "2", left = "Protocol: ICHIRIO-001",
            right = "Page {PAGE} of {TOTAL_PAGES}")),
  title("T-14-1-1", "Table 14.1.1", "Demographic Characteristics",
        "<Safety Analysis Set>"),
  title("T-14-1-2", "Table 14.1.2", "Subject Disposition",
        "<Safety Analysis Set>"),
  title("T-14-3-1", "Table 14.3.1",
        "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
        "<Safety Analysis Set>"),
  title("L-16-2-7", "Listing 16.2.7", "Severe Adverse Events",
        "<Safety Analysis Set>"),
  title("F-14-2-1", "Figure 14.2.1",
        "Kaplan-Meier Plot of Time to First Dermatologic Event",
        "<Safety Analysis Set>")))
sheets$footer <- tbl(
  list(line = "99", left = "{PROGRAM}       Generated on: {DATETIME}"),
  list(output_id = "T-14-1-1", line = "1",
       left = "SD = Standard Deviation."),
  list(output_id = "T-14-3-1", line = "1",
       left = "Subjects are counted once per system organ class and once per preferred term."),
  list(output_id = "T-14-3-1", line = "2", left = "MedDRA version 27.1."),
  list(output_id = "F-14-2-1", line = "1",
       left = "Event: first dermatologic adverse event; subjects without an event are censored."))

p <- new_planner(c(rounding = "sas"))
for (s in names(sheets)) p$sheets[[s]] <- .normalize_sheet(sheets[[s]], s)

# ---------------------------------------------------------------- data code
p$setup <- paste(
  "library(cards)",
  "adsl <- readRDS(\"data/adam/adsl.rds\")",
  "adsl <- adsl[adsl$SAFFL == \"Y\", ]", sep = "\n")
code <- list(
  "F-14-2-1" = c(
    "library(ggplot2)",
    "library(survival)",
    "fit <- survfit(Surv(AVAL, 1 - CNSR) ~ TRTA, data = adtte)",
    "km <- data.frame(time = c(0, fit$time), surv = c(1, fit$surv),",
    "                 arm = c(NA, rep(sub(\"TRTA=\", \"\", names(fit$strata)),",
    "                                 fit$strata)))",
    "km <- km[-1, ]",
    "start <- data.frame(time = 0, surv = 1, arm = unique(km$arm))",
    "km <- rbind(start, km)",
    "plot <- ggplot(km, aes(time, surv, colour = arm)) +",
    "  geom_step() +",
    "  scale_y_continuous(limits = c(0, 1)) +",
    "  labs(x = \"Days since first dose\", y = \"Probability without event\",",
    "       colour = NULL) +",
    "  theme_bw() + theme(legend.position = \"bottom\")"))

# ---------------------------------------------------------- the study ARD
# The tables' data: one study ARD from spec/ard_spec.xlsx (made by
# programs/ard/<output_id>.R); each table takes its part, so they have no data
# code of their own.
arow <- function(...) list(...)
p$ard$datasets <- tbl(
  list(dataset = "ADSL", level = "ADaM", path = "data/adam/adsl.rds"),
  list(dataset = "ADAE", level = "ADaM", path = "data/adam/adae.rds"),
  list(dataset = "ADLB", level = "ADaM", path = "data/adam/adlb.rds"),
  list(dataset = "ADTTE", level = "ADaM", path = "data/adam/adtte.rds"))
p$ard$populations <- tbl(
  list(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\"",
       derive = "TRTA = TRT01A"))
p$ard$analyses <- tbl(
  list(output_id = "T-14-1-1", analysis_id = "BIGN", label = "Subjects per arm",
       method = "categorical", population_id = "SAF", variables = "TRT01A"),
  list(output_id = "T-14-1-1", analysis_id = "TOTAL", method = "total_n",
       population_id = "SAF"),
  list(output_id = "T-14-1-1", analysis_id = "CONT", label = "Continuous",
       method = "continuous", population_id = "SAF", by = "TRT01A",
       variables = "AGE | WEIGHTBL | HEIGHTBL",
       statistics = "N | mean | sd | median | min | max"),
  list(output_id = "T-14-1-1", analysis_id = "CAT", label = "Categorical",
       method = "categorical", population_id = "SAF", by = "TRT01A",
       variables = "AGEGR1 | SEX | RACE", statistics = "n | p"),
  list(output_id = "T-14-1-2", analysis_id = "BIGN", method = "categorical",
       population_id = "SAF", variables = "TRT01A"),
  list(output_id = "T-14-1-2", analysis_id = "DISP",
       label = "Status at end of study", method = "categorical",
       population_id = "SAF", by = "TRT01A", variables = "DCDECOD",
       statistics = "n | p"),
  list(output_id = "T-14-3-1", analysis_id = "TEAE",
       label = "TEAE by SOC / PT", method = "hierarchical", dataset = "ADAE",
       population_id = "SAF", where = "TRTEMFL == \"Y\"", by = "TRTA",
       variables = "AEBODSYS | AEDECOD", args = "over_variables = TRUE"))
for (sh in names(p$ard)) p$ard[[sh]] <- .normalize_ard_sheet(p$ard[[sh]], sh)

# ------------------------------------------------ the listing, the figure
# The listing is defined in rows (rtfreporter's multiline); the
# figure reads ADTTE and its data code is the plot alone.
p$lf$listings <- .normalize_lf_sheet(tbl(
  list(output_id = "L-16-2-7", type = "multiline", dataset = "ADAE",
       where = "AESEV == \"SEVERE\"", sort = "TRTA | USUBJID | ASTDT",
       max_rows = "22")), "listings")
p$lf$listing_cols <- .normalize_lf_sheet(tbl(
  list(output_id = "L-16-2-7", vars = "TRTA", label = "Treatment", width = "22"),
  list(output_id = "L-16-2-7", vars = "USUBJID", label = "Subject", width = "14",
       collapse_repeats = "TRUE"),
  list(output_id = "L-16-2-7", vars = "AEBODSYS | AEDECOD",
       label = "System Organ Class/\\nPreferred Term", width = "44"),
  list(output_id = "L-16-2-7", vars = "ASTDT | AENDT", label = "Start/\\nEnd",
       width = "12"),
  list(output_id = "L-16-2-7", vars = "AESER", label = "Serious", width = "7"),
  list(output_id = "L-16-2-7", vars = "AEREL", label = "Relation", width = "9"),
  list(output_id = "L-16-2-7", vars = "AEOUT", label = "Outcome", width = "26")),
  "listing_cols")
p$lf$figures <- .normalize_lf_sheet(tbl(
  list(output_id = "F-14-2-1", datasets = "ADTTE")), "figures")

desc <- c("T-14-1-1" = "Demographic characteristics",
          "T-14-1-2" = "Subject disposition",
          "T-14-3-1" = "TEAEs by SOC / PT",
          "L-16-2-7" = "Listing of severe adverse events",
          "F-14-2-1" = "KM plot, time to first dermatologic event")
types <- c("T-14-1-1" = "table", "T-14-1-2" = "table", "T-14-3-1" = "table",
           "L-16-2-7" = "listing", "F-14-2-1" = "figure")
# the second step, normalize and rework (blank: data <- ard_normalize(ard))
process <- list(
  "T-14-3-1" = c(
    "data <- ard_normalize(ard, hierarchy = c(\"AEBODSYS\", \"AEDECOD\"),",
    "                      overall = \"Any TEAE\")"))
for (o in names(desc)) {
  p <- add_output(p, o, description = desc[[o]],
                  data_code = if (!is.null(code[[o]]))
                    paste(code[[o]], collapse = "\n") else NA,
                  process_code = if (!is.null(process[[o]]))
                    paste(process[[o]], collapse = "\n") else NA,
                  type = types[[o]])
}

# ------------------------------------------------------------ the study
unlink(file.path(root, id), recursive = TRUE)
if (!is.null(.read_state(id))) unregister_study(id)
s <- create_study(id, root = root, title = "Test study for tflplanner",
                  compound = "Xanomeline", phase = "2",
                  description = paste(
                    "CDISC pilot ADaM data ({cards}) relabelled as",
                    "ICHIRIO-001; one of each report type."),
                  planner = p)

relabel <- function(d) {
  d$STUDYID <- id
  d$USUBJID <- sub("^01-", "ICH001-", d$USUBJID)
  d
}
adam <- file.path(s$path, study_layout()[["adam"]])
saveRDS(relabel(cards::ADSL), file.path(adam, "adsl.rds"))
saveRDS(relabel(cards::ADAE), file.path(adam, "adae.rds"))
saveRDS(relabel(cards::ADLB), file.path(adam, "adlb.rds"))
saveRDS(relabel(cards::ADTTE), file.path(adam, "adtte.rds"))
if (requireNamespace("haven", quietly = TRUE)) {
  haven::write_xpt(relabel(cards::ADSL), file.path(adam, "adsl.xpt"))
}

st <- run_study(s)
print(st[c("output_id", "type", "program_state", "status")])
# what each table's ARD holds, for the app's input assistance
for (o in names(types)[types == "table"]) fetch_ard(s, o)

print(check_planner(s$planner))

if (!all(st$status == "ok")) {
  for (f in stats::na.omit(st$log[st$status != "ok"])) {
    cat("\n====", f, "\n")
    cat(utils::tail(readLines(f, warn = FALSE), 25), sep = "\n")
  }
  quit(status = 1L)
}
