# Company standards: the defaults and code lists tflplanner holds itself.
#
# Everything the app offers or fills in by itself -- the dropdowns, the
# presets, the statistics and their decimals, the ARD methods, the listing
# types, the analysis sets and data a new study starts with, the header and
# footer lines every report of a new study inherits -- comes from here.
# A company writes its own in a workbook (company_standards.xlsx, see
# standards_template()) and sets it up once (setup_tflplanner(standards =));
# without one, the built-in draft below applies.
#
# What is NOT here: what comes from the data (variables, levels, labels:
# the ARD and the source data say those) and rtfreporter's own defaults
# (which apply wherever a sheet says nothing).

.bool <- c("TRUE", "FALSE")

.df <- function(...) data.frame(..., stringsAsFactors = FALSE)

# The statistics an ARD analysis may ask for.  `kind` says which methods
# give it: continuous (in statistic = of ard_continuous()), categorical
# (ard_categorical(), ard_dichotomous(), hierarchical ...), missing
# (ard_missing()) or result (what a CI / test / model gives: statistics
# keeps some of them).  A continuous statistic without `fun` is cards' own
# (continuous_summary_fns()); one with `fun` is computed by that function
# of the non-missing values, written into the ARD program.  `fmt` is the
# format stat_fmt gets unless the method or the analysis says another:
# xx.x (1 decimal), xx.x% (a proportion as a percent), a number of
# decimals, or pvalue (<0.001 / 0.xxx).
.ard_statistics_builtin <- function() {
  q <- function(p) sprintf(
    "function(x) stats::quantile(x, %s, type = 2, names = FALSE)", p)
  ci <- function(sign, tr = "x", back = "") sprintf(
    "function(x) %s(mean(%s) %s stats::qt(0.975, length(x) - 1) * stats::sd(%s) / sqrt(length(x)))",
    back, tr, sign, tr)
  pos <- function(body) paste0("function(x) if (any(x <= 0)) NA_real_ else ",
                               body)
  gci <- function(sign) pos(sprintf(
    "exp(mean(log(x)) %s stats::qt(0.975, length(x) - 1) * stats::sd(log(x)) / sqrt(length(x)))",
    sign))
  rows <- list(
    # continuous: cards
    c("N", "continuous", "cards", "n (non-missing values)", "xx", ""),
    c("mean", "continuous", "cards", "Mean", "xx.x", ""),
    c("sd", "continuous", "cards", "SD", "xx.xx", ""),
    c("median", "continuous", "cards", "Median", "xx.x", ""),
    c("p25", "continuous", "cards", "Q1 (25th percentile)", "xx.x", ""),
    c("p75", "continuous", "cards", "Q3 (75th percentile)", "xx.x", ""),
    c("min", "continuous", "cards", "Min", "xx", ""),
    c("max", "continuous", "cards", "Max", "xx", ""),
    # continuous: computed
    c("se", "continuous", "spread", "SE of the mean", "xx.xx",
      "function(x) stats::sd(x) / sqrt(length(x))"),
    c("var", "continuous", "spread", "Variance", "xx.xx",
      "function(x) stats::var(x)"),
    c("cv", "continuous", "spread", "CV (%)", "xx.x",
      "function(x) stats::sd(x) / mean(x) * 100"),
    c("range", "continuous", "spread", "Range (max - min)", "xx",
      "function(x) max(x) - min(x)"),
    c("iqr", "continuous", "spread", "IQR (Q3 - Q1)", "xx.x",
      "function(x) diff(stats::quantile(x, c(0.25, 0.75), type = 2, names = FALSE))"),
    c("sum", "continuous", "spread", "Sum", "xx", "function(x) sum(x)"),
    c("p5", "continuous", "percentiles", "5th percentile", "xx.x", q("0.05")),
    c("p10", "continuous", "percentiles", "10th percentile", "xx.x", q("0.10")),
    c("p90", "continuous", "percentiles", "90th percentile", "xx.x", q("0.90")),
    c("p95", "continuous", "percentiles", "95th percentile", "xx.x", q("0.95")),
    c("mean_lcl", "continuous", "confidence limits",
      "Mean: lower 95% confidence limit (t)", "xx.x", ci("-")),
    c("mean_ucl", "continuous", "confidence limits",
      "Mean: upper 95% confidence limit (t)", "xx.x", ci("+")),
    c("geo_mean", "continuous", "log scale", "Geometric mean", "xx.xx",
      pos("exp(mean(log(x)))")),
    c("geo_sd", "continuous", "log scale", "Geometric SD", "xx.xx",
      pos("exp(stats::sd(log(x)))")),
    c("geo_cv", "continuous", "log scale", "Geometric CV (%)", "xx.x",
      pos("sqrt(exp(stats::sd(log(x))^2) - 1) * 100")),
    c("geo_lcl", "continuous", "log scale",
      "Geometric mean: lower 95% confidence limit", "xx.xx", gci("-")),
    c("geo_ucl", "continuous", "log scale",
      "Geometric mean: upper 95% confidence limit", "xx.xx", gci("+")),
    c("log_mean", "continuous", "log scale", "Mean of log values", "xx.xxx",
      pos("mean(log(x))")),
    c("log_sd", "continuous", "log scale", "SD of log values", "xx.xxx",
      pos("stats::sd(log(x))")),
    # categorical
    c("n", "categorical", "counts", "n (subjects / records in the level)",
      "xx", ""),
    c("N", "categorical", "counts", "N (the denominator)", "xx", ""),
    c("p", "categorical", "counts", "Percent (n / N)", "xx.x%", ""),
    c("n_cum", "categorical", "counts", "Cumulative n", "xx", ""),
    c("p_cum", "categorical", "counts", "Cumulative percent", "xx.x%", ""),
    # missing
    c("N_obs", "missing", "missing", "Number of records", "xx", ""),
    c("N_miss", "missing", "missing", "Missing", "xx", ""),
    c("N_nonmiss", "missing", "missing", "Non-missing", "xx", ""),
    c("p_miss", "missing", "missing", "Percent missing", "xx.x%", ""),
    c("p_nonmiss", "missing", "missing", "Percent non-missing", "xx.x%", ""),
    # results of CIs, tests, models, survival
    c("estimate", "result", "estimates", "Estimate", "xx.xx", ""),
    c("conf.low", "result", "estimates", "Lower confidence limit", "xx.xx", ""),
    c("conf.high", "result", "estimates", "Upper confidence limit", "xx.xx", ""),
    c("std.error", "result", "estimates", "Standard error", "xx.xxx", ""),
    c("estimate1", "result", "estimates", "Estimate of group 1", "xx.xx", ""),
    c("estimate2", "result", "estimates", "Estimate of group 2", "xx.xx", ""),
    c("statistic", "result", "tests", "Test statistic", "xx.xx", ""),
    c("parameter", "result", "tests", "Degrees of freedom", "xx.x", ""),
    c("p.value", "result", "tests", "p-value", "pvalue", ""),
    c("conf.level", "result", "estimates", "Confidence level", "xx.xx", ""),
    c("n.risk", "result", "survival", "Number at risk", "xx", ""),
    c("n.event", "result", "survival", "Number of events", "xx", ""))
  m <- do.call(rbind, rows)
  d <- .df(statistic = m[, 1], kind = m[, 2], group = m[, 3],
           label = m[, 4], fmt = m[, 5], fun = m[, 6])
  d$note <- ""
  d$note[d$group == "log scale"] <- "computed on log(x); NA when a value is 0 or below"
  d$note[d$statistic == "N" & d$kind == "continuous"] <- "the missing are counted by the method `missing`"
  d$fun[!nzchar(d$fun)] <- NA
  d$note[!nzchar(d$note)] <- NA
  d
}

# the draft: what the app held before it read standards
.builtin_standards <- function() {
  ch <- function(sheet, column, values) .df(sheet = sheet, column = column,
                                            value = values)
  default <- function(sheet, ...) {
    d <- .empty_sheet(sheet)
    d$output_id <- NULL
    rows <- list(...)
    for (r in rows) {
      d[nrow(d) + 1L, ] <- NA
      for (k in names(r)) d[[k]][nrow(d)] <- r[[k]]
    }
    d
  }
  s <- list(
    about = .df(key = c("name", "version", "owner", "updated"),
                value = c("tflplanner draft standards", "0.1", "",
                          "2026-09-27")),
    settings = .df(
      key = c("language", "rounding", "subject_id", "ard_output",
              "listing_type", "max_levels"),
      value = c("en", "", "USUBJID", "output/ard/ard.rds", "multiline",
                "30"),
      note = c("the app's language: en or ja",
               "a new study's rounding: r, sas, or blank (rtfreporter's)",
               "the subject key of the ARD definition",
               "where the study ARD goes (relative to the study folder)",
               "the listing type a new listing starts with",
               "a key with more levels gets no levels list when filled from the ARD")),
    choices = rbind(
      ch("tables", "stats", c("cells", "rows")),
      ch("tables", "value", c("stat", "stat_fmt")),
      ch("tables", "sort", c(".overall | group1 | .depth | -n | label",
                             "FALSE")),
      ch("layout", "pages_split", c("group_safe", "group_force")),
      ch("layout", "blank_where", "between_groups"),
      ch("layout", c("group_page", "group_show", "blank_first", "blank_last",
                     "blank_counted", "stub_before")[rep(1:6, each = 2)],
         rep(.bool, 6)),
      ch("columns", c("row_title", "decimal_split", "hide")[rep(1:3, each = 2)],
         rep(.bool, 3)),
      ch("style", c("align_count_pct", "auto_width")[rep(1:2, each = 2)],
         rep(.bool, 2)),
      ch("col_header", "bold", .bool),
      ch("col_header", "align", c("left", "center", "right")),
      ch("col_header", "text", c("{col}", "(N={n})", "{col}\n(N={n})",
                                 "{col}\n(N={n})\nn (%)", "Characteristic",
                                 "{col1}", "{col2}", "(N={n:sum})")),
      ch("cells", "digits", c("0", "1", "2", "1,2", "0,1", "2,3")),
      ch("report", "type", c("table", "listing", "figure")),
      ch("report", c("auto_section", "auto_title", "page_header",
                     "page_footer")[rep(1:4, each = 2)], rep(.bool, 4)),
      ch("page", "orientation", c("portrait", "landscape")),
      ch("page", "paper_size", c("letter", "A4"))),
    cell_presets = rbind(
      .df(preset = "Continuous: n / Mean (SD) / Median / Min, Max",
          variable = "continuous", row = c("n", "Mean (SD)", "Median", "Min, Max"),
          template = c("{N}", "{mean} ({sd})", "{median}", "{min}, {max}"),
          digits = c("0", "1,2", "1", "0")),
      .df(preset = "Continuous: n / Mean (SD) / Median / Q1, Q3 / Min, Max",
          variable = "continuous",
          row = c("n", "Mean (SD)", "Median", "Q1, Q3", "Min, Max"),
          template = c("{N}", "{mean} ({sd})", "{median}", "{p25}, {p75}",
                       "{min}, {max}"),
          digits = c("0", "1,2", "1", "1", "0")),
      .df(preset = "Continuous: n / Mean (SD) / Median (Q1, Q3) / Min, Max",
          variable = "continuous",
          row = c("n", "Mean (SD)", "Median (Q1, Q3)", "Min, Max"),
          template = c("{N}", "{mean} ({sd})", "{median} ({p25}, {p75})",
                       "{min}, {max}"),
          digits = c("0", "1,2", "1", "0")),
      .df(preset = "Categorical: n (%)", variable = "categorical", row = NA,
          template = "{n:.0f} ({p:.1f%})", digits = NA),
      .df(preset = "Categorical: n/N (%)", variable = "categorical", row = NA,
          template = "{n:.0f}/{N:.0f} ({p:.1f%})", digits = NA),
      .df(preset = "Categorical: n", variable = "categorical", row = NA,
          template = "{n:.0f}", digits = NA)),
    header_presets = rbind(
      .df(preset = "Arm / (N=n)", line = c("1", "1", "2", "2"),
          cols = c("row_label", ".values", "row_label", ".values"),
          span = c(NA, "each", NA, "each"),
          text = c(NA, "{col}", "Characteristic", "(N={n})")),
      .df(preset = "Arm (N=n) n (%)", line = c("1", "1"),
          cols = c("row_label", ".values"), span = c(NA, "each"),
          text = c(NA, "{col}\n(N={n})\nn (%)")),
      .df(preset = "Arm (N=n) n (%), SOC / PT", line = c("1", "1", "2", "2"),
          cols = c("row_label", ".values", "row_label", ".values"),
          span = c(NA, "each", NA, NA),
          text = c(NA, "{col}\n(N={n})\nn (%)",
                   "System Organ Class\n   Preferred Term", NA))),
    statistics = .df(
      key = c("n", "mean_sd", "median", "q1q3", "median_q1q3", "min_max"),
      row = c("n", "Mean (SD)", "Median", "Q1, Q3", "Median (Q1, Q3)",
              "Min, Max"),
      template = c("{N}", "{mean} ({sd})", "{median}", "{p25}, {p75}",
                   "{median} ({p25}, {p75})", "{min}, {max}"),
      digits = c("0", "d+1,d+2", "d+1", "d+1", "d+1", "d"),
      note = c("", "d = the decimals the data are collected with", "", "",
               "", "")),
    categorical_formats = .df(
      key = c("npct", "nNpct", "n"),
      label = c("n (%)", "n/N (%)", "n"),
      template = c("{n:.0f} ({p:.<p>f%})", "{n:.0f}/{N:.0f} ({p:.<p>f%})",
                   "{n:.0f}"),
      note = c("<p> = the decimals of the percent", "", "")),
    ard_methods = .df(
      method = c("continuous", "categorical", "dichotomous", "missing",
                 "hierarchical", "max", "subjects", "total_n",
                 "proportion_ci", "mean_ci", "ttest", "wilcox", "chisq",
                 "fisher", "custom"),
      call = c("cards::ard_continuous", "cards::ard_categorical",
               "cards::ard_dichotomous", "cards::ard_missing",
               "cards::ard_stack_hierarchical", "cardx::ard_tabulate_max",
               "(subjects)", "cards::ard_total_n",
               "cardx::ard_categorical_ci", "cardx::ard_continuous_ci",
               "cardx::ard_stats_t_test", "cardx::ard_stats_wilcox_test",
               "cardx::ard_stats_chisq_test", "cardx::ard_stats_fisher_test",
               "(code)"),
      kind = c("continuous", "categorical", "categorical", "missing",
               "categorical", "categorical", "categorical", "none", "none",
               "none", "none", "none", "none", "none", "none"),
      defaults = c("", "", "", "", "denominator = population, id = <id>",
                   "denominator = population, id = <id>", "", "", "", "",
                   "", "", "", "", ""),
      statistics = c("", "", "", "", "", "", "", "", "", "", "", "", "",
                     "", ""),
      formats = c("", "", "", "", "", "", "", "", paste(
        "estimate=xx.x% | conf.low=xx.x% | conf.high=xx.x%"),
        "estimate=xx.x | conf.low=xx.x | conf.high=xx.x", "", "", "", "",
        ""),
      note = c(
        "summary statistics of numeric variables",
        "counts and percents of each level",
        "counts of one level (args: value = list(VAR = \"Y\"))",
        "missing and non-missing counts",
        "nested subject counts, outermost variable first (SOC | PT)",
        "the worst level per subject; variables = the graded variable",
        "subjects with a record of the data (after where); variables = a name for the count",
        "number of subjects",
        "confidence interval of a proportion (args: method = \"wilson\" ...); statistics keep some of estimate, conf.low, conf.high",
        "confidence interval of a mean (args: conf.level = 0.9 ...); statistics keep some of estimate, conf.low, conf.high",
        "two-sample t test between the `by` groups (two of them)",
        "Wilcoxon rank-sum test between the `by` groups (two of them)",
        "chi-square test of variables x by",
        "Fisher's exact test of variables x by",
        "any R code in `code`; data and population are bound")),
    ard_statistics = .ard_statistics_builtin(),
    listing_types = .df(type = "multiline", label = "multiline",
                        note = "rtfreporter's listing type: / separator, gutters, a blank row per record"),
    populations = .df(
      population_id = c("SAF", "FAS", "PPS", "ENR"),
      dataset = "ADSL",
      where = c("SAFFL == \"Y\"", "FASFL == \"Y\"", "PPROTFL == \"Y\"",
                "ENRLFL == \"Y\""),
      derive = c("TRTA = TRT01A", "TRTP = TRT01P", "TRTP = TRT01P",
                 "TRTP = TRT01P"),
      note = c("Safety set", "Full analysis set", "Per-protocol set",
               "Enrolled")),
    datasets = .df(
      dataset = c("ADSL", "ADAE", "ADLB", "ADVS", "ADEG", "ADEX", "ADCM",
                  "ADMH", "ADTTE", "ADRS", "DM", "AE", "CM", "MH", "EX",
                  "LB", "VS", "DS"),
      level = c(rep("ADaM", 10), rep("SDTM", 8)),
      path = c(paste0("data/adam/", tolower(c("ADSL", "ADAE", "ADLB", "ADVS",
                                               "ADEG", "ADEX", "ADCM", "ADMH",
                                               "ADTTE", "ADRS")), ".rds"),
               paste0("data/sdtm/", tolower(c("DM", "AE", "CM", "MH", "EX",
                                               "LB", "VS", "DS")), ".rds")),
      derive = NA_character_,
      note = NA_character_),
    default_header = default("header",
      list(line = "1", left = "Company", right = "DRAFT"),
      list(line = "2", left = "Protocol: {STUDY_ID}",
           right = "Page {PAGE} of {TOTAL_PAGES}")),
    default_footer = default("footer",
      list(line = "99", left = "{PROGRAM}       Generated on: {DATETIME}")),
    default_cells = default("cells",
      list(template = "{n:.0f} ({p:.1f%})", note = "every categorical cell")),
    default_layout = default("layout",
      list(blank_where = "between_groups", blank_first = "TRUE",
           blank_last = "TRUE", blank_counted = "TRUE",
           stub_into = "row_label", stub_before = "TRUE")),
    default_columns = default("columns",
      list(column = "row_label", width = "40"),
      list(column = ".values", width = "18")),
    default_style = default("style", list(align_count_pct = "TRUE")),
    default_col_header = default("col_header",
      list(line = "1", cols = "row_label"),
      list(line = "1", cols = ".values", span = "each", text = "{col}"),
      list(line = "2", cols = "row_label", text = "Characteristic"),
      list(line = "2", cols = ".values", span = "each", text = "(N={n})")),
    default_report = default("report",
      list(type = "table", file = "{output_id}.rtf",
           program = "{output_id}.R")),
    default_page = default("page"),
    default_titles = default("titles"),
    default_footnotes = default("footnotes"))
  s
}

.standard_sheets <- function() names(.builtin_standards())

.standards_readme <- function() {
  .df(sheet = c("about", "settings", "choices", "cell_presets",
                "header_presets", "statistics", "categorical_formats",
                "ard_methods", "ard_statistics", "listing_types",
                "populations", "datasets",
                "default_<sheet>"),
      description = c(
        "who keeps these standards, and which version",
        "single values: the app's language, a new study's rounding, the subject key, the study ARD's place ...",
        "the values a grid column offers as a dropdown (sheet, column, value; one row per value)",
        "cell presets of the table definition: rows of the cells sheet, grouped by preset",
        "column header presets: rows of the col_header sheet, grouped by preset",
        "the statistics the table builder offers; digits use d = the decimals of the data",
        "the categorical formats the table builder offers; <p> = the decimals of the percent",
        "the ARD methods (keywords): the function, its kind (continuous / categorical / missing / none, for statistic =), default arguments (<id> = the subject key), statistics and formats",
        "the statistics an ARD analysis may ask for: kind, label, default format of stat_fmt (xx.x, xx.x%, pvalue), and the R function of those tflplanner computes",
        "the listing types a listing may use (rtfreporter's)",
        "the analysis sets a new study's ARD definition starts with",
        "the data catalog a new study starts with: ADaM and SDTM datasets and their files",
        "the study-default rows a new study starts with, one sheet per definition sheet (default_header, default_footer, default_cells ...); {STUDY_ID} becomes the study's id"),
      ja = c(
        "\u6a19\u6e96\u306e\u7ba1\u7406\u8005\u30fb\u7248",
        "\u5358\u4e00\u306e\u5024\uff1a\u8a00\u8a9e\u3001\u65b0\u898f\u8a66\u9a13\u306e\u4e38\u3081\u65b9\u3001\u88ab\u9a13\u8005\u30ad\u30fc\u3001\u8a66\u9a13 ARD \u306e\u7f6e\u304d\u5834\u6240\u306a\u3069",
        "\u8868\u306e\u5217\u306e\u30d7\u30eb\u30c0\u30a6\u30f3\u5019\u88dc\uff08\u30b7\u30fc\u30c8\u30fb\u5217\u30fb\u5024\u30011 \u884c 1 \u5024\uff09",
        "\u8868\u306e\u5b9a\u7fa9\u306e\u30bb\u30eb\u306e\u30d7\u30ea\u30bb\u30c3\u30c8\uff08cells \u30b7\u30fc\u30c8\u306e\u884c\u3092\u30d7\u30ea\u30bb\u30c3\u30c8\u3054\u3068\u306b\uff09",
        "\u5217\u898b\u51fa\u3057\u306e\u30d7\u30ea\u30bb\u30c3\u30c8\uff08col_header \u30b7\u30fc\u30c8\u306e\u884c\u3092\u30d7\u30ea\u30bb\u30c3\u30c8\u3054\u3068\u306b\uff09",
        "\u8868\u30d3\u30eb\u30c0\u30fc\u306e\u7d71\u8a08\u91cf\u3002digits \u306e d = \u30c7\u30fc\u30bf\u306e\u5c0f\u6570\u6841\u6570",
        "\u8868\u30d3\u30eb\u30c0\u30fc\u306e\u30ab\u30c6\u30b4\u30ea\u5909\u6570\u306e\u66f8\u5f0f\u3002<p> = % \u306e\u5c0f\u6570\u6841\u6570",
        "ARD \u306e\u624b\u6cd5\u30ad\u30fc\u30ef\u30fc\u30c9\uff1a\u95a2\u6570\u3001\u7a2e\u985e\uff08statistic \u306e\u5f62\uff09\u3001\u65e2\u5b9a\u306e\u5f15\u6570\uff08<id> = \u88ab\u9a13\u8005\u30ad\u30fc\uff09\u3001\u7d71\u8a08\u91cf\u3001\u66f8\u5f0f",
        "ARD \u306e\u7d71\u8a08\u91cf\uff1a\u7a2e\u985e\u3001\u30e9\u30d9\u30eb\u3001stat_fmt \u306e\u65e2\u5b9a\u306e\u66f8\u5f0f\uff08xx.x, xx.x%, pvalue\uff09\u3001tflplanner \u304c\u8a08\u7b97\u3059\u308b\u3082\u306e\u306e R \u95a2\u6570",
        "Listing \u306e\u7a2e\u985e\uff08rtfreporter \u306e\u3082\u306e\uff09",
        "\u65b0\u898f\u8a66\u9a13\u306e ARD \u5b9a\u7fa9\u304c\u6700\u521d\u306b\u6301\u3064\u89e3\u6790\u5bfe\u8c61\u96c6\u56e3",
        "\u65b0\u898f\u8a66\u9a13\u304c\u6700\u521d\u306b\u6301\u3064\u30c7\u30fc\u30bf\u30ab\u30bf\u30ed\u30b0\uff08ADaM / SDTM \u3068\u30d5\u30a1\u30a4\u30eb\uff09",
        "\u65b0\u898f\u8a66\u9a13\u306e\u300c\u8a66\u9a13\u5171\u901a\u306e\u65e2\u5b9a\u300d\u306e\u884c\u3002\u5b9a\u7fa9\u30b7\u30fc\u30c8\u3054\u3068\u306b 1 \u30b7\u30fc\u30c8\u3002{STUDY_ID} \u306f\u8a66\u9a13 ID \u306b\u7f6e\u304d\u63db\u308f\u308b"))
}

#' Company standards
#'
#' tflplanner's own defaults and code lists -- dropdowns, presets,
#' statistics and decimals, ARD methods, listing types, the analysis sets,
#' data catalog and study-default rows a new study starts with -- are a
#' company's standards.  `standards_template()` writes them to a workbook
#' to edit (the built-in draft, or the standards in use);
#' `read_standards()` reads one; [setup_tflplanner()]`(standards = )`
#' installs one in tflplanner's home, and `company_standards()` is what
#' applies: the installed workbook, else the built-in draft.
#'
#' A sheet the workbook leaves out keeps the built-in one.
#'
#' @param path A workbook.
#' @param from `"builtin"` for the draft, `"current"` for the standards in
#'   use.
#' @param home tflplanner's home.
#' @return `standards_template()`: `path`, invisibly.  `read_standards()`
#'   and `company_standards()`: a named list of data frames.
#' @export
standards_template <- function(path, from = c("builtin", "current")) {
  from <- match.arg(from)
  s <- if (from == "builtin") .builtin_standards() else company_standards()
  writexl::write_xlsx(c(list(`_README` = .standards_readme()), s), path)
  invisible(path)
}

#' @rdname standards_template
#' @export
read_standards <- function(path) {
  b <- .builtin_standards()
  have <- readxl::excel_sheets(path)
  out <- lapply(names(b), function(s) {
    if (!s %in% have) return(b[[s]])
    d <- .read_sheet_text(path, s)
    # a column added to the standards later may be left out
    for (k in intersect(.std_optional[[s]], setdiff(names(b[[s]]), names(d)))) {
      d[[k]] <- NA_character_
    }
    miss <- setdiff(names(b[[s]]), names(d))
    if (length(miss)) {
      stop("Standards sheet `", s, "` lacks column(s): ",
           paste(miss, collapse = ", "), call. = FALSE)
    }
    d <- d[names(b[[s]])]
    d[] <- lapply(d, function(v) {
      v <- as.character(v)
      v[!is.na(v) & !nzchar(trimws(v))] <- NA
      v
    })
    d <- d[rowSums(!is.na(d)) > 0, , drop = FALSE]
    rownames(d) <- NULL
    d
  })
  stats::setNames(out, names(b))
}

.std_optional <- list(ard_methods = "formats")

.standards_file <- function(home = tflplanner_home()) {
  file.path(home, "standards", "company_standards.xlsx")
}

.std_cache <- new.env()

#' @rdname standards_template
#' @export
company_standards <- function(home = tflplanner_home()) {
  f <- .standards_file(home)
  if (!file.exists(f)) return(.builtin_standards())
  key <- paste(f, file.mtime(f))
  if (!identical(.std_cache$key, key)) {
    .std_cache$value <- read_standards(f)
    .std_cache$key <- key
  }
  .std_cache$value
}

.std_setting <- function(key, default = NA_character_) {
  s <- company_standards()$settings
  v <- s$value[match(key, s$key)]
  if (length(v) && !is.na(v) && nzchar(v)) v else default
}

# a grid column's dropdown values from the standards
.std_choices <- function(sheet) {
  ch <- company_standards()$choices
  ch <- ch[!is.na(ch$sheet) & ch$sheet == sheet, , drop = FALSE]
  split(ch$value, factor(ch$column, levels = unique(ch$column)))
}

# the rows a new study starts with: its study defaults, its ARD definition's
# analysis sets and data catalog
.standard_planner <- function(study_id) {
  s <- company_standards()
  p <- new_planner()
  r <- .std_setting("rounding")
  if (!is.na(r)) p$study[["rounding"]] <- r
  for (sh in c(table_sheets(), report_sheets())) {
    d <- s[[paste0("default_", sh)]]
    if (is.null(d) || !nrow(d)) next
    d[] <- lapply(d, function(v) gsub("{STUDY_ID}", study_id, v, fixed = TRUE))
    d$output_id <- NA_character_
    p$sheets[[sh]] <- .normalize_sheet(d, sh)
  }
  p$ard$study$value[p$ard$study$key == "id"] <- .std_setting("subject_id",
                                                             "USUBJID")
  p$ard$study$value[p$ard$study$key == "output"] <-
    .std_setting("ard_output", "output/ard/ard.rds")
  p$ard$populations <- .normalize_ard_sheet(s$populations, "populations")
  p$ard$datasets <- .normalize_ard_sheet(s$datasets, "datasets")
  p
}
