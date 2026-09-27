# The sample study that ships with tflplanner (inst/sample/SAMPLE-01):
# one of each kind of report, on the CDISC pilot ADaM data of
# {pharmaverseadam} (Apache License 2.0).
#
#   T-14-1-1  Table    Demographic characteristics
#   T-14-1-2  Table    Subject disposition
#   T-14-2-1  Table    Change from baseline in systolic blood pressure,
#                      Week 24 (with SE and the mean's 95% CI)
#   T-14-2-2  Table    Time to first dermatologic event: Kaplan-Meier
#                      estimates (median, event-free probability by day)
#   T-14-3-1  Table    TEAEs by SOC / PT, frequency descending
#   L-16-2-7  Listing  Severe adverse events
#   F-14-2-1  Figure   Mean change from baseline in systolic blood pressure
#   F-14-2-2  Figure   Kaplan-Meier plot of the time to first dermatologic
#                      event; its number at risk is T-14-2-2's ARD
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
    list(output_id = "T-14-3-1", cols = "TRT01A", rows = "group1 = AEBODSYS",
         label = "label = AEDECOD",
         sort = ".overall | group1 | .depth | -n | label",
         note = "TEAE by SOC / PT, frequency descending")),
  variables = tbl(
    list(output_id = "T-14-1-1", variable = "TRT01A", levels = arms),
    list(output_id = "T-14-1-1", variable = "AGE", label = "Age (years)",
         order = 1),
    list(output_id = "T-14-1-1", variable = "AGEGR1",
         label = "Age group, n (%)", order = 2, levels = "18-64 | >64"),
    list(output_id = "T-14-1-1", variable = "SEX", label = "Sex, n (%)",
         order = 3, levels = "F | M"),
    list(output_id = "T-14-1-1", variable = "RACE", label = "Race, n (%)",
         order = 4),
    list(output_id = "T-14-1-1", variable = "ETHNIC",
         label = "Ethnicity, n (%)", order = 5),
    list(output_id = "T-14-1-2", variable = "TRT01A", levels = arms),
    list(output_id = "T-14-1-2", variable = "EOSSTT",
         label = "Status at end of study, n (%)", order = 1,
         levels = "COMPLETED | DISCONTINUED"),
    list(output_id = "T-14-2-1", variable = "TRTA", levels = arms),
    list(output_id = "T-14-2-1", variable = "BASE",
         label = "Baseline (mmHg)", order = 1),
    list(output_id = "T-14-2-1", variable = "AVAL",
         label = "Week 24 (mmHg)", order = 2),
    list(output_id = "T-14-2-1", variable = "CHG",
         label = "Change from baseline (mmHg)", order = 3),
    list(output_id = "T-14-3-1", variable = "TRT01A", levels = arms),
    list(output_id = "T-14-2-2", variable = "TRT01A", levels = arms),
    list(output_id = "T-14-2-2", variable = "prob",
         label = "Time to first event (days)", order = 1),
    list(output_id = "T-14-2-2", variable = "time",
         label = "Event-free probability (95% CI)", order = 2)),
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
         template = "{min}, {max}", digits = "0")),
  layout = tbl(
    list(blank_where = "between_groups", blank_first = "TRUE",
         blank_last = "TRUE", blank_counted = "TRUE", stub_into = "row_label",
         stub_before = "TRUE", note = "study default"),
    list(output_id = "T-14-1-1", pages_max_rows = "24",
         pages_split = "group_safe"),
    list(output_id = "T-14-1-2", pages_max_rows = "24"),
    list(output_id = "T-14-2-1", pages_max_rows = "30",
         pages_split = "group_safe"),
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
  list(output_id = "F-14-2-1", type = "figure"),
  list(output_id = "F-14-2-2", type = "figure"))
sheets$page <- tbl(
  list(output_id = "L-16-2-7", orientation = "landscape"),
  list(output_id = "F-14-2-1", orientation = "landscape"),
  list(output_id = "F-14-2-2", orientation = "landscape"))
sheets$header <- do.call(tbl, c(
  list(list(line = "1", left = "Sample Pharma (tflplanner sample)",
            right = "DRAFT"),
       list(line = "2", left = "Protocol: SAMPLE-01 (CDISC pilot data)",
            right = "Page {PAGE} of {TOTAL_PAGES}")),
  title("T-14-1-1", "Table 14.1.1", "Demographic Characteristics",
        "<Safety Analysis Set>"),
  title("T-14-1-2", "Table 14.1.2", "Subject Disposition",
        "<Safety Analysis Set>"),
  title("T-14-2-1", "Table 14.2.1",
        "Systolic Blood Pressure (mmHg): Change from Baseline at Week 24",
        "<Safety Analysis Set>"),
  title("T-14-3-1", "Table 14.3.1",
        "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
        "<Safety Analysis Set>"),
  title("L-16-2-7", "Listing 16.2.7", "Severe Adverse Events",
        "<Safety Analysis Set>"),
  title("F-14-2-1", "Figure 14.2.1",
        "Mean (SE) Change from Baseline in Systolic Blood Pressure over Time",
        "<Safety Analysis Set>"),
  title("T-14-2-2", "Table 14.2.2",
        "Time to First Dermatologic Event: Kaplan-Meier Estimates",
        "<Safety Analysis Set>"),
  title("F-14-2-2", "Figure 14.2.2",
        "Kaplan-Meier Plot of Time to First Dermatologic Event",
        "<Safety Analysis Set>")))
sheets$footer <- tbl(
  list(line = "99", left = "{PROGRAM}       Generated on: {DATETIME}"),
  list(output_id = "T-14-1-1", line = "1",
       left = "SD = Standard Deviation."),
  list(output_id = "T-14-2-1", line = "1",
       left = "SD = Standard Deviation; SE = Standard Error; CI = Confidence Interval (t distribution)."),
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
       derive = "TRTA = TRT01A"))
p$ard$analyses <- tbl(
  list(output_id = "T-14-1-1", analysis_id = "BIGN", label = "Subjects per arm",
       method = "categorical", population_id = "SAF", variables = "TRT01A"),
  list(output_id = "T-14-1-1", analysis_id = "TOTAL", method = "total_n",
       population_id = "SAF"),
  list(output_id = "T-14-1-1", analysis_id = "CONT", label = "Continuous",
       method = "continuous", population_id = "SAF", by = "TRT01A",
       variables = "AGE", statistics = "N | mean | sd | median | min | max"),
  list(output_id = "T-14-1-1", analysis_id = "CAT", label = "Categorical",
       method = "categorical", population_id = "SAF", by = "TRT01A",
       variables = "AGEGR1 | SEX | RACE | ETHNIC", statistics = "n | p"),
  list(output_id = "T-14-1-2", analysis_id = "BIGN", method = "categorical",
       population_id = "SAF", variables = "TRT01A"),
  list(output_id = "T-14-1-2", analysis_id = "DISP",
       label = "Status at end of study", method = "categorical",
       population_id = "SAF", by = "TRT01A", variables = "EOSSTT",
       statistics = "n | p"),
  list(output_id = "T-14-2-1", analysis_id = "BIGN", method = "categorical",
       population_id = "SAF", variables = "TRTA"),
  list(output_id = "T-14-2-1", analysis_id = "SYSBP",
       label = "SYSBP at Week 24", method = "continuous", dataset = "ADVS",
       population_id = "SAF", where = "AVISIT == \"Week 24\"", by = "TRTA",
       variables = "BASE | AVAL | CHG",
       statistics = "N | mean | sd | se | mean_lcl | mean_ucl | median | min | max",
       formats = "mean=xx.x | sd=xx.xx | se=xx.xx"),
  list(output_id = "T-14-2-2", analysis_id = "BIGN", method = "categorical",
       population_id = "SAF", variables = "TRT01A"),
  list(output_id = "T-14-2-2", analysis_id = "KM",
       label = "Kaplan-Meier estimates", method = "custom", dataset = "ADTTE",
       population_id = "SAF", where = "PARAMCD == \"TTDE\"",
       code = paste(
         "fit <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRT01A, data = data)",
         "cards::bind_ard(",
         "  cardx::ard_survival_survfit(fit, probs = 0.5),",
         "  cardx::ard_survival_survfit(fit, times = c(0, 30, 60, 90, 120, 150, 180)),",
         "  .quiet = TRUE)", sep = "\n")),
  list(output_id = "T-14-3-1", analysis_id = "TEAE",
       label = "TEAE by SOC / PT", method = "hierarchical", dataset = "ADAE",
       population_id = "SAF", where = "TRTEMFL == \"Y\"", by = "TRT01A",
       variables = "AEBODSYS | AEDECOD", args = "over_variables = TRUE"))
for (sh in names(p$ard)) p$ard[[sh]] <- .normalize_ard_sheet(p$ard[[sh]], sh)

# ------------------------------------------------ the listing, the figure
p$lf$listings <- .normalize_lf_sheet(tbl(
  list(output_id = "L-16-2-7", type = "multiline", dataset = "ADAE",
       where = "AESEV == \"SEVERE\"", sort = "TRT01A | USUBJID | ASTDT",
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

plot_code <- c(
  "library(ggplot2)",
  "saf <- adsl$USUBJID[adsl$SAFFL == \"Y\"]",
  "d <- advs[advs$USUBJID %in% saf & advs$AVISITN > 0 & !is.na(advs$CHG), ]",
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
  "  theme_tfl() + theme(legend.position = \"bottom\")")

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
  "km_ard <- readRDS(\"output/ard/ard.rds\")",
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
  "  plot_layout(heights = c(1, tfl_num(\"risk_height\", \"km\")))")

desc <- c("T-14-1-1" = "Demographic characteristics",
          "T-14-1-2" = "Subject disposition",
          "T-14-2-1" = "Systolic blood pressure: change from baseline at Week 24",
          "T-14-3-1" = "TEAEs by SOC / PT",
          "T-14-2-2" = "Time to first dermatologic event: KM estimates",
          "L-16-2-7" = "Listing of severe adverse events",
          "F-14-2-1" = "Mean change from baseline in systolic blood pressure",
          "F-14-2-2" = "KM plot of the time to first dermatologic event")
types <- c("T-14-1-1" = "table", "T-14-1-2" = "table", "T-14-2-1" = "table",
           "T-14-3-1" = "table", "T-14-2-2" = "table", "L-16-2-7" = "listing",
           "F-14-2-1" = "figure", "F-14-2-2" = "figure")
process <- list(
  "T-14-2-2" = c(
    "data <- ard_normalize(ard)",
    "# row labels: the median, and the event-free probability by day (day 0 left out)",
    "is_time <- data$variable == \"time\"",
    "data$.label[data$variable == \"prob\"] <- \"Median (95% CI)\"",
    "data$.label[is_time] <- paste(\"Day\", as.character(data$variable_level[is_time]))",
    "data <- data[!(is_time & as.character(data$variable_level) == \"0\"), ]"),
  "T-14-3-1" = c(
    "data <- ard_normalize(ard, hierarchy = c(\"AEBODSYS\", \"AEDECOD\"),",
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
                    c(.table_file, .report_file, .ard_json, .lf_file)),
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
  "| Output | Type | |",
  "|---|---|---|",
  "| T-14-1-1 | Table | Demographic characteristics |",
  "| T-14-1-2 | Table | Subject disposition |",
  "| T-14-2-1 | Table | Systolic blood pressure: change from baseline at Week 24 (SE, 95% CI of the mean) |",
  "| T-14-2-2 | Table | Time to first dermatologic event: Kaplan-Meier estimates |",
  "| T-14-3-1 | Table | TEAEs by SOC / PT |",
  "| L-16-2-7 | Listing | Severe adverse events |",
  "| F-14-2-1 | Figure | Mean change from baseline in systolic blood pressure |",
  "| F-14-2-2 | Figure | Kaplan-Meier plot of the time to first dermatologic event (number at risk from T-14-2-2's ARD) |",
  "",
  "The tables are made from one study ARD (programs/ard/), the listing and",
  "the figures from the ADaM data, in the figure style of the company",
  "standards (programs/tfl/fig_setup.R).  Made by data-raw/make-sample-study.R",
  "of the tflplanner repository."), file.path(out, "README.md"))
cat("\nWrote", out, "\n")
print(file.info(list.files(out, recursive = TRUE, full.names = TRUE))["size"])
