# EXPERIMENTAL: does ard_spec.xlsx describe the analyses behind the usual
# clinical tables?
#
# Builds the verification study ICHIRIO-002 on the pharmaverseadam data
# (ADSL, ADAE, ADLB, ADVS, ADEG, ADEX, ADCM, ADMH, ADTTE, ADRS, ADPC, ADPP),
# writes an ard_spec.xlsx with one output per usual display, makes the study
# ARD from it, and checks
#   1. that every analysis runs (each on its own, so a failure names its row),
#   2. that the key numbers equal an independent computation (base R,
#      stats, survival), and
#   3. that each output's part of the ARD normalizes (rtfreporter), the
#      first step of laying out its table.
# The findings go to output/verification.csv in the study folder.
#
#   Rscript data-raw/verify-ard-spec.R

devtools::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(rtfreporter))
library(pharmaverseadam)

id <- "ICHIRIO-002"
root <- studies_root()
unlink(file.path(root, id), recursive = TRUE)
unregister_study(id)
s <- create_study(id, root = root,
                  title = "Verification study for ard_spec (pharmaverseadam)",
                  compound = "Xanomeline", phase = "2")
adam <- file.path(s$path, "data", "adam")
for (d in c("adsl", "adae", "adlb", "advs", "adeg", "adex", "adcm", "admh",
            "adtte_onco", "adrs_onco", "adpc", "adpp")) {
  saveRDS(get(d), file.path(adam, paste0(d, ".rds")))
}

row <- function(...) list(...)
tbl <- function(rows) {
  cols <- unique(unlist(lapply(rows, names)))
  as.data.frame(lapply(stats::setNames(cols, cols), function(cn)
    vapply(rows, function(r) as.character(r[[cn]] %||% NA), "")),
    stringsAsFactors = FALSE)
}
ds <- function(name, file, derive = NA) {
  row(dataset = name, path = paste0("data/adam/", file, ".rds"),
      derive = derive)
}
A <- function(output_id, analysis_id, method, ..., label = NA) {
  row(output_id = output_id, analysis_id = analysis_id, method = method,
      label = label, ...)
}
S <- "SAF"
lab <- "ANL01FL == \"Y\" & PARAMCD == \"ALT\""

spec <- list(
  study = tbl(list(row(key = "id", value = "USUBJID"),
                   row(key = "output", value = "output/ard/ard.rds"))),
  datasets = tbl(list(
    ds("ADSL", "adsl"),
    ds("ADAE", "adae", "TRTA = TRT01A"),
    ds("ADLB", "adlb"), ds("ADVS", "advs"), ds("ADEG", "adeg"),
    ds("ADEX", "adex", "TRTA = TRT01A"),
    ds("ADCM", "adcm"), ds("ADMH", "admh"),
    ds("ADTTE", "adtte_onco", "EVENT = 1 - CNSR"),
    ds("ADRS", "adrs_onco", "RESP = AVALC == \"Y\""),
    ds("ADPC", "adpc"), ds("ADPP", "adpp"))),
  populations = tbl(list(
    row(population_id = "ALL", dataset = "ADSL",
        note = "every subject, screen failures too"),
    row(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\"",
        derive = "TRTA = TRT01A"))),
  analyses = tbl(list(
    # ---- subjects
    A("T-POP", "N", "categorical", population_id = "ALL", variables = "TRT01A",
      label = "Subjects per arm"),
    A("T-POP", "SAF", "dichotomous", population_id = "ALL", by = "TRT01A",
      variables = "SAFFL", args = "value = list(SAFFL = \"Y\")",
      label = "In the safety set"),
    A("T-DISP", "N", "categorical", population_id = S, variables = "TRT01A"),
    A("T-DISP", "EOS", "categorical", population_id = S, by = "TRT01A",
      variables = "EOSSTT | DTHFL", label = "End of study status, death"),
    # ---- demographics
    A("T-DM", "N", "categorical", population_id = S, variables = "TRT01A"),
    A("T-DM", "TOTAL", "total_n", population_id = S),
    A("T-DM", "CONT", "continuous", population_id = S, by = "TRT01A",
      variables = "AGE | TRTDURD",
      statistics = "N | mean | sd | median | p25 | p75 | min | max"),
    A("T-DM", "CAT", "categorical", population_id = S, by = "TRT01A",
      variables = "AGEGR1 | SEX | RACE"),
    A("T-DM", "MISS", "missing", population_id = S, by = "TRT01A",
      variables = "AGE | TRTDURD", label = "Missing values"),
    A("T-DM", "MEANCI", "mean_ci", population_id = S, by = "TRT01A",
      variables = "AGE", label = "Mean with 95% CI"),
    A("T-DM-SEX", "AGE", "continuous", population_id = S,
      by = "TRT01A | SEX", variables = "AGE",
      label = "Subgroup: two grouping variables"),
    # ---- exposure
    A("T-EX", "DUR", "continuous", dataset = "ADEX", population_id = S,
      where = "PARAMCD == \"TDURD\"", by = "TRTA", variables = "AVAL",
      label = "Treatment duration (days)"),
    A("T-EX", "DOSE", "continuous", dataset = "ADEX", population_id = S,
      where = "PARAMCD == \"TDOSE\"", by = "TRTA", variables = "AVAL",
      label = "Total dose"),
    # ---- adverse events
    A("T-AE-OV", "N", "categorical", population_id = S, variables = "TRTA"),
    A("T-AE-OV", "ANY", "subjects", dataset = "ADAE", population_id = S,
      where = "TRTEMFL == \"Y\"", by = "TRTA", variables = "ANY_TEAE"),
    A("T-AE-OV", "SER", "subjects", dataset = "ADAE", population_id = S,
      where = "TRTEMFL == \"Y\" & AESER == \"Y\"", by = "TRTA",
      variables = "ANY_SERIOUS"),
    A("T-AE-OV", "REL", "subjects", dataset = "ADAE", population_id = S,
      where = "TRTEMFL == \"Y\" & AREL %in% c(\"POSSIBLE\", \"PROBABLE\")",
      by = "TRTA", variables = "ANY_RELATED"),
    A("T-AE-OV", "SEV", "subjects", dataset = "ADAE", population_id = S,
      where = "TRTEMFL == \"Y\" & ASEV == \"SEVERE\"", by = "TRTA",
      variables = "ANY_SEVERE"),
    A("T-AE-OV", "DTH", "subjects", dataset = "ADAE", population_id = S,
      where = "AESDTH == \"Y\"", by = "TRTA", variables = "ANY_FATAL"),
    A("T-AE-SOCPT", "TEAE", "hierarchical", dataset = "ADAE",
      population_id = S, where = "TRTEMFL == \"Y\"", by = "TRTA",
      variables = "AEBODSYS | AEDECOD", args = "over_variables = TRUE"),
    A("T-AE-PT", "TEAE", "hierarchical", dataset = "ADAE", population_id = S,
      where = "TRTEMFL == \"Y\"", by = "TRTA", variables = "AEDECOD",
      args = "over_variables = TRUE"),
    A("T-AE-SER", "TEAE", "hierarchical", dataset = "ADAE",
      population_id = S, where = "TRTEMFL == \"Y\" & AESER == \"Y\"",
      by = "TRTA", variables = "AEBODSYS | AEDECOD",
      args = "over_variables = TRUE"),
    A("T-AE-SEV", "MAX", "max", dataset = "ADAE", population_id = S,
      where = "TRTEMFL == \"Y\"", by = "TRTA", variables = "ASEV",
      args = "strata = AEBODSYS",
      label = "Worst severity per subject, by SOC"),
    A("T-AE-SEV", "MAXALL", "max", dataset = "ADAE", population_id = S,
      where = "TRTEMFL == \"Y\"", by = "TRTA", variables = "ASEV",
      label = "Worst severity per subject"),
    A("T-AE-3LVL", "TEAE", "hierarchical", dataset = "ADAE",
      population_id = S, where = "TRTEMFL == \"Y\"", by = "TRTA",
      variables = "AEBODSYS | AEDECOD | ASEV",
      label = "Three levels: SOC / PT / severity"),
    A("T-AE-RATE", "IR", "custom", dataset = "ADAE", population_id = S,
      where = "TRTEMFL == \"Y\"",
      code = paste(
        "# subject level: events and years at risk, then the rate",
        "n_ev <- table(data$USUBJID)",
        "population$N_EVENTS <- as.numeric(n_ev[population$USUBJID])",
        "population$N_EVENTS[is.na(population$N_EVENTS)] <- 0",
        "population$YEARS <- population$TRTDURD / 365.25",
        "cardx::ard_incidence_rate(population, time = YEARS,",
        "  count = N_EVENTS, id = USUBJID, by = TRTA)", sep = "\n"),
      label = "Exposure-adjusted event rate"),
    # ---- laboratory
    A("T-LB", "ALT", "continuous", dataset = "ADLB", population_id = S,
      where = paste(lab, "& !is.na(AVISITN) & AVISITN %in% c(0, 2, 4, 8)"),
      by = "TRTA | AVISIT", variables = "AVAL | CHG",
      label = "ALT by visit"),
    A("T-LB-MULTI", "CHEM", "continuous", dataset = "ADLB", population_id = S,
      where = paste("ANL01FL == \"Y\" & PARAMCD %in% c(\"ALT\", \"AST\",",
                    "\"BILI\") & AVISIT %in% c(\"Baseline\", \"Week 8\")"),
      by = "TRTA | PARAMCD | AVISIT", variables = "AVAL",
      label = "Three grouping variables"),
    A("T-LB-SHIFT", "ALT", "categorical", dataset = "ADLB", population_id = S,
      where = "PARAMCD == \"ALT\" & AVISIT == \"POST-BASELINE MAXIMUM\"",
      by = "TRTA | BNRIND", variables = "ANRIND",
      label = "Shift, baseline to worst post-baseline"),
    A("T-LB-TOX", "ALT", "max", dataset = "ADLB", population_id = S,
      where = "PARAMCD == \"ALT\" & ONTRTFL == \"Y\" & !is.na(ATOXGR)",
      by = "TRTA", variables = "ATOXGR",
      label = "Worst toxicity grade"),
    A("T-LB-ABN", "ALT", "cardx::ard_tabulate_abnormal", dataset = "ADLB",
      population_id = S, where = "PARAMCD == \"ALT\" & ONTRTFL == \"Y\"",
      by = "TRTA",
      args = "postbaseline = ANRIND, baseline = BNRIND, id = USUBJID",
      label = "Abnormal post-baseline, by baseline"),
    A("T-LB-ABNSUBJ", "HIGH", "subjects", dataset = "ADLB",
      population_id = S,
      where = "PARAMCD == \"ALT\" & ONTRTFL == \"Y\" & ANRIND == \"HIGH\"",
      by = "TRTA", variables = "ALT_HIGH",
      label = "Subjects with a high ALT"),
    # ---- vital signs, ECG
    A("T-VS", "BP", "continuous", dataset = "ADVS", population_id = S,
      where = paste("ANL01FL == \"Y\" & PARAMCD %in% c(\"SYSBP\", \"DIABP\")",
                    "& AVISIT %in% c(\"Baseline\", \"Week 8\")"),
      by = "TRTA | PARAMCD | AVISIT", variables = "AVAL | CHG"),
    A("T-EG", "INT", "categorical", dataset = "ADEG", population_id = S,
      where = "PARAMCD == \"EGINTP\" & !is.na(AVISIT)",
      by = "TRTA | AVISIT", variables = "AVALC"),
    # ---- medications, history
    A("T-CM", "ONT", "hierarchical", dataset = "ADCM", population_id = S,
      where = "ONTRTFL == \"Y\"", by = "TRTA", variables = "CMCLAS | CMDECOD",
      args = "over_variables = TRUE", label = "Concomitant"),
    A("T-CM-PRIOR", "PRIOR", "hierarchical", dataset = "ADCM",
      population_id = S, where = "PREFL == \"Y\"", by = "TRTA",
      variables = "CMCLAS | CMDECOD", label = "Prior"),
    A("T-MH", "MH", "hierarchical", dataset = "ADMH", population_id = S,
      where = "!is.na(MHBODSYS)", by = "TRTA",
      variables = "MHBODSYS | MHDECOD"),
    # ---- efficacy: continuous comparisons
    A("T-EFF", "DESC", "continuous", dataset = "ADVS", population_id = S,
      where = "PARAMCD == \"SYSBP\" & ANL01FL == \"Y\" & AVISIT == \"Week 8\" & ATPTN == 815",
      by = "TRTA", variables = "CHG"),
    A("T-EFF", "TTEST", "cardx::ard_stats_t_test", dataset = "ADVS",
      population_id = S,
      where = "PARAMCD == \"SYSBP\" & ANL01FL == \"Y\" & AVISIT == \"Week 8\" & ATPTN == 815 & TRTA != \"Xanomeline Low Dose\"",
      by = "TRTA", variables = "CHG", label = "t-test, high dose vs placebo"),
    A("T-EFF", "WILCOX", "cardx::ard_stats_wilcox_test", dataset = "ADVS",
      population_id = S,
      where = "PARAMCD == \"SYSBP\" & ANL01FL == \"Y\" & AVISIT == \"Week 8\" & ATPTN == 815 & TRTA != \"Xanomeline Low Dose\"",
      by = "TRTA", variables = "CHG"),
    A("T-EFF", "KW", "cardx::ard_stats_kruskal_test", dataset = "ADVS",
      population_id = S,
      where = "PARAMCD == \"SYSBP\" & ANL01FL == \"Y\" & AVISIT == \"Week 8\" & ATPTN == 815",
      by = "TRTA", variables = "CHG", label = "Kruskal-Wallis, three arms"),
    A("T-EFF", "ANOVA", "cardx::ard_stats_aov", dataset = "ADVS",
      population_id = S,
      where = "PARAMCD == \"SYSBP\" & ANL01FL == \"Y\" & AVISIT == \"Week 8\" & ATPTN == 815",
      args = "formula = CHG ~ TRTA"),
    A("T-EFF", "LSMEAN", "cardx::ard_emmeans_mean_difference",
      dataset = "ADVS", population_id = S,
      where = "PARAMCD == \"SYSBP\" & ANL01FL == \"Y\" & AVISIT == \"Week 8\" & ATPTN == 815",
      args = "formula = CHG ~ TRTA + BASE, method = \"lm\", response_type = \"continuous\"",
      label = "ANCOVA: LS mean differences"),
    A("T-EFF", "REG", "custom", dataset = "ADVS", population_id = S,
      where = "PARAMCD == \"SYSBP\" & ANL01FL == \"Y\" & AVISIT == \"Week 8\" & ATPTN == 815",
      code = "cardx::ard_regression(lm(CHG ~ TRTA + BASE, data = data))",
      label = "Regression coefficients"),
    # ---- efficacy: categorical comparisons
    A("T-CAT-TEST", "CHISQ", "cardx::ard_stats_chisq_test",
      population_id = S, by = "TRT01A", variables = "SEX"),
    A("T-CAT-TEST", "FISHER", "cardx::ard_stats_fisher_test",
      population_id = S, by = "TRT01A", variables = "AGEGR1"),
    # ---- response
    A("T-RESP", "BOR", "categorical", dataset = "ADRS", population_id = S,
      where = "PARAMCD == \"BOR\"", by = "TRT01A", variables = "AVALC",
      label = "Best overall response"),
    A("T-RESP", "CI", "proportion_ci", dataset = "ADRS", population_id = S,
      where = "PARAMCD == \"CB\"", by = "TRT01A", variables = "RESP",
      args = "method = \"clopper-pearson\"",
      label = "Clinical benefit rate, exact 95% CI"),
    A("T-RESP", "DIFF", "cardx::ard_stats_prop_test", dataset = "ADRS",
      population_id = S,
      where = "PARAMCD == \"CB\" & TRT01A != \"Xanomeline Low Dose\"",
      by = "TRT01A", variables = "RESP",
      label = "Difference in rates, high dose vs placebo"),
    A("T-RESP", "CMH", "cardx::ard_stats_mantelhaen_test", dataset = "ADRS",
      population_id = S, where = "PARAMCD == \"CB\"", by = "TRT01A",
      variables = "AVALC", args = "strata = SEX",
      label = "CMH, stratified by sex"),
    A("T-RESP", "LOGIT", "custom", dataset = "ADRS", population_id = S,
      where = "PARAMCD == \"CB\"",
      code = paste("cardx::ard_regression(",
                   "  glm(RESP ~ TRT01A + SEX, data = data, family = binomial),",
                   "  exponentiate = TRUE)", sep = "\n"),
      label = "Odds ratios"),
    # ---- time to event
    A("T-TTE", "KM", "cardx::ard_survival_survfit", dataset = "ADTTE",
      population_id = S, where = "PARAMCD == \"PFS\"", variables = "ARM",
      args = "y = \"survival::Surv(AVAL, EVENT)\", probs = c(0.25, 0.5, 0.75)",
      label = "Median and quartiles"),
    A("T-TTE", "KMT", "cardx::ard_survival_survfit", dataset = "ADTTE",
      population_id = S, where = "PARAMCD == \"PFS\"", variables = "ARM",
      args = "y = \"survival::Surv(AVAL, EVENT)\", times = c(30, 60, 90)",
      label = "Event-free rates at days 30, 60, 90"),
    A("T-TTE", "LOGRANK", "cardx::ard_survival_survdiff", dataset = "ADTTE",
      population_id = S, where = "PARAMCD == \"PFS\"",
      args = "formula = survival::Surv(AVAL, EVENT) ~ ARM"),
    A("T-TTE", "COX", "custom", dataset = "ADTTE", population_id = S,
      where = "PARAMCD == \"PFS\"",
      code = paste("cardx::ard_regression(",
                   "  survival::coxph(survival::Surv(AVAL, EVENT) ~ ARM, data = data),",
                   "  exponentiate = TRUE)", sep = "\n"),
      label = "Hazard ratios"),
    # ---- PK
    A("T-PC", "CONC", "continuous", dataset = "ADPC", population_id = S,
      where = "PARAMCD == \"XAN\" & AVISIT == \"Day 1\" & ATPTN %in% c(0.5, 1, 2, 4)",
      by = "TRT01A | ATPT", variables = "AVAL",
      args = paste("statistic = ~ cards::continuous_summary_fns(",
                   "c(\"N\", \"mean\", \"sd\", \"median\", \"min\", \"max\"),",
                   "other_stats = list(geo_mean = function(x) exp(mean(log(x[x > 0])))))"),
      label = "Concentrations, with geometric mean"),
    A("T-PP", "PARAM", "continuous", dataset = "ADPP", population_id = S,
      where = "PARAMCD %in% c(\"CMAX\", \"AUCLST\")",
      by = "TRT01A | PARAMCD", variables = "AVAL"))))

f <- file.path(s$path, "spec", "ard_spec.xlsx")
writexl::write_xlsx(c(spec, list(`_methods` = ard_methods())), f)
x <- read_ard_spec(f)
# the study keeps its definition (the app shows it); saving writes
# spec/ard_spec.xlsx and programs/ard/ from it
s$planner$ard <- lapply(stats::setNames(names(.ard_spec_sheets),
                                        names(.ard_spec_sheets)),
                        function(sh) .normalize_ard_sheet(x[[sh]], sh))
s <- save_study(s)

# 1. every analysis on its own
a <- x$analyses
runs <- lapply(seq_len(nrow(a)), function(i) {
  one <- x
  one$analyses <- a[i, ]
  t0 <- Sys.time()
  r <- tryCatch(suppressWarnings(suppressMessages(
    build_ard(one, dir = s$path, save = FALSE))),
    error = function(e) e)
  list(ard = if (!inherits(r, "error")) r, error = if (inherits(r, "error"))
    conditionMessage(r), secs = as.numeric(difftime(Sys.time(), t0,
                                                    units = "secs")))
})
ard <- do.call(dplyr::bind_rows, lapply(runs, `[[`, "ard"))
saveRDS(ard, file.path(s$path, "output", "ard", "ard.rds"))

# 2. the key numbers, computed another way
saf <- adsl[adsl$SAFFL == "Y", ]
teae <- adae[adae$TRTEMFL %in% "Y" & adae$USUBJID %in% saf$USUBJID, ]
stat <- function(out, an, name, ...) {
  d <- ard[ard$output_id == out & ard$analysis_id == an &
             ard$stat_name == name, , drop = FALSE]
  cond <- list(...)
  for (k in names(cond)) {
    col <- if (k %in% names(d)) d[[k]] else NULL
    lv <- vapply(col, function(e) paste(unlist(e), collapse = ""), "")
    d <- d[lv == cond[[k]], , drop = FALSE]
  }
  if (nrow(d) != 1L) return(NA_real_)
  as.numeric(unlist(d$stat))
}
pl <- "Placebo"
vs8 <- advs[advs$PARAMCD == "SYSBP" & advs$ANL01FL %in% "Y" &
              advs$AVISIT %in% "Week 8" & advs$ATPTN %in% 815 &
              advs$USUBJID %in% saf$USUBJID, ]
hi <- "Xanomeline High Dose"
tt <- t.test(CHG ~ TRTA, data = vs8[vs8$TRTA != "Xanomeline Low Dose", ])
cb <- adrs_onco[adrs_onco$PARAMCD == "CB" &
                  adrs_onco$USUBJID %in% saf$USUBJID, ]
pfs <- adtte_onco[adtte_onco$PARAMCD == "PFS" &
                    adtte_onco$USUBJID %in% saf$USUBJID, ]
km <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ ARM, data = pfs)
kmq <- quantile(km, probs = 0.5)$quantile
alt <- adlb[adlb$PARAMCD == "ALT" & adlb$AVISIT %in% "POST-BASELINE MAXIMUM" &
              adlb$USUBJID %in% saf$USUBJID, ]
checks <- list(
  c("T-POP", "N", stat("T-POP", "N", "n", variable_level = pl),
    sum(adsl$TRT01A == pl)),
  c("T-DM", "CONT", stat("T-DM", "CONT", "mean", variable = "AGE",
                         group1_level = pl),
    mean(saf$AGE[saf$TRT01A == pl])),
  c("T-DM", "CAT", stat("T-DM", "CAT", "n", variable = "SEX",
                        variable_level = "F", group1_level = pl),
    sum(saf$SEX == "F" & saf$TRT01A == pl)),
  c("T-DM-SEX", "AGE", stat("T-DM-SEX", "AGE", "median", group1_level = pl,
                            group2_level = "M"),
    median(saf$AGE[saf$TRT01A == pl & saf$SEX == "M"])),
  c("T-AE-OV", "SER", stat("T-AE-OV", "SER", "n", group1_level = pl),
    length(unique(teae$USUBJID[teae$AESER == "Y" & teae$TRT01A == pl]))),
  c("T-AE-SOCPT", "TEAE", stat("T-AE-SOCPT", "TEAE", "n", group1_level = pl,
                               variable = "AEDECOD",
                               variable_level = "APPLICATION SITE PRURITUS"),
    length(unique(teae$USUBJID[teae$TRT01A == pl &
                                 teae$AEDECOD == "APPLICATION SITE PRURITUS"]))),
  c("T-AE-SEV", "MAXALL", stat("T-AE-SEV", "MAXALL", "n", group1_level = pl,
                               variable_level = "SEVERE"),
    length(unique(teae$USUBJID[teae$TRT01A == pl & teae$ASEV == "SEVERE"]))),
  c("T-LB-SHIFT", "ALT", stat("T-LB-SHIFT", "ALT", "n", group1_level = pl,
                              group2_level = "NORMAL",
                              variable_level = "HIGH"),
    sum(alt$TRTA == pl & alt$BNRIND %in% "NORMAL" & alt$ANRIND %in% "HIGH")),
  c("T-EFF", "TTEST", stat("T-EFF", "TTEST", "statistic"),
    unname(tt$statistic)),
  c("T-CAT-TEST", "CHISQ", stat("T-CAT-TEST", "CHISQ", "p.value"),
    chisq.test(table(saf$TRT01A, saf$SEX))$p.value),
  c("T-RESP", "CI", stat("T-RESP", "CI", "conf.low", group1_level = pl),
    binom.test(sum(cb$AVALC[cb$TRT01A == pl] == "Y"),
               sum(cb$TRT01A == pl))$conf.int[1L]),
  c("T-TTE", "KM", stat("T-TTE", "KM", "estimate", variable_level = pl,
                        variable = "ARM", stat_name = "estimate",
                        context = "survival_survfit") ,
    NA_real_),
  c("T-TTE", "LOGRANK", stat("T-TTE", "LOGRANK", "statistic"),
    survival::survdiff(survival::Surv(AVAL, 1 - CNSR) ~ ARM,
                       data = pfs)$chisq))
# the KM median: its row is the one for probability 0.5
k <- ard[ard$output_id == "T-TTE" & ard$analysis_id == "KM" &
           ard$stat_name == "estimate", , drop = FALSE]
lv <- vapply(k$variable_level, function(e) paste(unlist(e), collapse = ""), "")
p <- vapply(k$variable_level, function(e) paste(unlist(e), collapse = ""), "")
mid <- k[grepl(pl, lv) | grepl(pl, vapply(k$group1_level, function(e)
  paste(unlist(e), collapse = ""), "")), , drop = FALSE]
checks[[12]] <- c("T-TTE", "KM",
                  NA_real_, unname(kmq[paste0("ARM=", pl), 1L]))
if (nrow(mid)) {
  pr <- vapply(mid$variable_level, function(e) paste(unlist(e), collapse = ""), "")
  cand <- suppressWarnings(as.numeric(unlist(mid$stat)))
  checks[[12]][3] <- cand[which(grepl("0.5", pr))[1L]] %||% NA
}

# 3. does each output's part normalize?
norm <- vapply(unique(a$output_id), function(o) {
  r <- tryCatch({
    d <- suppressMessages(ard_normalize(ard_for(ard, o)))
    sprintf("ok (%d rows)", nrow(d))
  }, error = function(e) paste("fails:", conditionMessage(e)))
  r
}, "")

report <- data.frame(
  output_id = a$output_id, analysis_id = a$analysis_id, method = a$method,
  label = a$label,
  runs = vapply(runs, function(r) if (is.null(r$error)) "ok" else "ERROR", ""),
  rows = vapply(runs, function(r) if (is.null(r$ard)) 0L else nrow(r$ard), 1L),
  seconds = round(vapply(runs, `[[`, 1, "secs"), 1),
  error = vapply(runs, function(r) r$error %||% "", ""),
  stringsAsFactors = FALSE)
chk <- do.call(rbind, lapply(checks, function(c) data.frame(
  output_id = c[1], analysis_id = c[2], ard = as.numeric(c[3]),
  independent = as.numeric(c[4]), stringsAsFactors = FALSE)))
chk$same <- abs(chk$ard - chk$independent) < 1e-8
report$check <- ""
for (i in seq_len(nrow(chk))) {
  j <- which(report$output_id == chk$output_id[i] &
               report$analysis_id == chk$analysis_id[i])
  report$check[j] <- if (isTRUE(chk$same[i])) "equal" else
    sprintf("DIFFERS (%s vs %s)", chk$ard[i], chk$independent[i])
}
report$normalize <- norm[report$output_id]
utils::write.csv(report, file.path(s$path, "output", "verification.csv"),
                 row.names = FALSE)
options(width = 200)
print(report[c("output_id", "analysis_id", "method", "runs", "rows",
               "check", "normalize")], right = FALSE)
cat("\nruns:", sum(report$runs == "ok"), "/", nrow(report),
    "  checks equal:", sum(chk$same, na.rm = TRUE), "/", nrow(chk),
    "  outputs normalizing:", sum(startsWith(norm, "ok")), "/", length(norm),
    "\n")
for (i in which(report$runs != "ok")) {
  cat("\n", report$output_id[i], report$analysis_id[i], ":", report$error[i],
      "\n")
}
