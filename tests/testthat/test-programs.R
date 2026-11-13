sample_planner <- function() {
  d <- system.file("extdata", "ard-spec", package = "tflspec")
  read_planner(file.path(d, c("report.xlsx", "study.xlsx")))
}

test_that("a table program writes out its plan and report (its data not saved) and parses", {
  skip_on_cran()
  p <- sample_planner()
  code <- program_code(p, "PK")
  expect_true(any(grepl('report_id <- "PK"', code, fixed = TRUE)))
  # the definition written out, not read at run time
  expect_false(any(grepl("tfl_read_report_spec", code, fixed = TRUE)))
  expect_false(any(grepl("spec = spec", code, fixed = TRUE)))
  # one pipe from the ARD: its normalization, the plan (tflspec #302)
  expect_true("plan <- ard |>" %in% code)
  expect_true(any(startsWith(trimws(code), "normalize_ard(")))
  expect_true(any(startsWith(trimws(code), "table_plan(")))
  expect_true(any(startsWith(trimws(code), "plan_digits(")))
  expect_true(any(startsWith(code, "doc <- rtf_document(")))
  # one pipe: the document, its tables
  expect_true(any(startsWith(trimws(code), "rtf_tables(plan")))
  expect_true('generate_rtfreport(doc, file.path("output", paste0(report_id, ".rtf")), overwrite = TRUE)' %in% code)
  expect_true(any(grepl('file.exists("study.yml")', code, fixed = TRUE)))
  expect_false(any(grepl("saveRDS(", code, fixed = TRUE)))
  # no data code: the company template takes its rows of the study ARD
  expect_true(any(grepl("filter(output_id == report_id)", code, fixed = TRUE)))
  expect_false(any(grepl("data <- normalize_ard(", code, fixed = TRUE)))
  expect_silent(parse(text = code))
  expect_equal(report_info(p, "PK")$file, "output/PK.rtf")
  expect_silent(parse(text = autoexec_code(p)))

  p <- set_step_code(p, "PK", "data", "data <- my_pk()")
  code <- program_code(p, "PK")
  expect_true("data <- my_pk()" %in% code)
  expect_false(any(grepl("TODO", code)))

  p$sheets$report <- rbind(p$sheets$report, NA)
  p$sheets$report[nrow(p$sheets$report), c("output_id", "program")] <-
    c("AE", "t_ae_{output_id}")
  expect_equal(report_info(p, "AE")$program, "t_ae_AE.R")
})

test_that("listings and figures make `content` and skip the table plan", {
  p <- add_output(new_planner(), "L1", type = "listing")
  p <- add_output(p, "F1", type = "figure")
  expect_equal(report_info(p, "L1")$type, "listing")
  expect_equal(p$sheets$report$type, c("listing", "figure"))
  for (id in c("L1", "F1")) {
    code <- program_code(p, id)
    expect_silent(parse(text = code))
    expect_true(any(code == "  rtf_tables(content)" |
                      startsWith(code, "  rtf_figures(content") |
                      startsWith(code, "  rtf_figures(plot")))
    expect_false(any(grepl("table_plan(", code, fixed = TRUE)))
  }
  expect_error(add_output(p, "X", type = "chart"))
})

test_that("a definition that does not hold yet still gives a program, which says why", {
  p <- sample_planner()
  # a statistic's display format (no template) in a table that is not
  # stats = rows: tflspec refuses it
  p$sheets$cells <- rbind(p$sheets$cells, NA)
  p$sheets$cells[nrow(p$sheets$cells), c("output_id", "row", "digits")] <-
    c("DM", "n", "0")
  code <- program_code(p, "DM")
  expect_silent(parse(text = code))
  hit <- grep("tflplanner: the definition of DM does not hold", code, fixed = TRUE)
  expect_length(hit, 1L)
})

test_that("the report list names the datasets a report reads, and its title", {
  skip_on_cran()
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  p <- suppressMessages(create_sample_study(run = FALSE))$planner
  # a table: its analyses' data and their population's
  expect_identical(.report_datasets(p, "T-14-1-1"), "ADSL")
  expect_identical(.report_datasets(p, "T-14-3-1"), c("ADAE", "ADSL"))
  # a listing: its dataset
  expect_identical(.report_datasets(p, "L-16-2-7"), "ADAE")
  # a figure: the datasets its row names, or its design reads
  expect_identical(.report_datasets(p, "F-14-2-2"), "ADTTE")
  d <- tflspec::tfl_fig_template("mean_ci", data = "ADVS", param = "SYSBP",
                                  value = "CHG")
  # (the sample's F-14-2-1 is user code: a figure again, to be designed)
  p2 <- set_fig_design(.set_report_type(p, "F-14-2-1", "figure"), "F-14-2-1", d)
  expect_identical(.report_datasets(p2, "F-14-2-1"), c("ADVS", "ADSL"))
  # the title: the titles sheet's own lines, not the running study lines
  expect_identical(.report_title(p, "T-14-1-1"), "")
  p$sheets$titles <- data.frame(
    output_id = c(NA, "T-14-1-1", "T-14-1-1"), line = c("1", "1", "2"),
    left = NA, center = c("Study {PAGE}", "Table 14.1.1", "Demographics"),
    right = NA, note = NA, stringsAsFactors = FALSE)
  expect_identical(.report_title(p, "T-14-1-1"), "Table 14.1.1 / Demographics")
})

test_that("the report list's cells are escaped, cut short, with the whole on hover", {
  expect_identical(.cell_tip("a<b"), "a&lt;b")
  expect_identical(.cell_tip("a<b", 'x"y'), '<span title="x&quot;y">a&lt;b</span>')
  long <- strrep("a", 80)
  expect_identical(nchar(.ellipsis(long, 70L)), 70L)
  expect_identical(.ellipsis("short", 70L), "short")
})

test_that("the report types stay English in the Japanese app", {
  expect_identical(tr(c("Table", "Listing", "Figure"), "ja"),
                   c("Table", "Listing", "Figure"))
  expect_identical(tr("Report title", "ja"), "タイトル")
})

test_that("a table's normalization is what its definitions say: the hierarchy, the any-event row (tflspec #236)", {
  p <- add_output(new_planner(), "AE")
  p$ard$datasets <- .normalize_ard_sheet(data.frame(dataset = "ADAE", path = "data/adam/adae.rds"), "datasets")
  p <- set_ard_rows(p, "analyses", "AE", data.frame(
    analysis_id = "TEAE", method = "hierarchical", dataset = "ADAE", by = "TRTA",
    variables = "AEBODSYS | AEDECOD", args = "over_variables = TRUE"))
  p <- set_sheet_rows(p, "tables", "AE", data.frame(
    cols = "TRTA", rows = "group1 = AEBODSYS",
    overall_label = "Subjects with at least one TEAE"))
  code <- program_code(p, "AE")
  # in the table's pipe (tflspec #302)
  i <- which(code == "plan <- ard |>")
  expect_length(i, 1L)
  expect_identical(code[i + 1:5], c(
    "  normalize_ard(",
    "    hierarchy = c(\"AEBODSYS\", \"AEDECOD\"),",
    "    overall = \"Subjects with at least one TEAE\"",
    "  ) |>",
    "  table_plan(cols = \"TRTA\", rows = c(group1 = \"AEBODSYS\"))"))
  expect_false(any(grepl("data <- normalize_ard(", code, fixed = TRUE)))
  expect_false(any(startsWith(code, "saveRDS(")))
  # a table without them: normalize_ard() with none
  q <- add_output(new_planner(), "T1")
  code <- program_code(q, "T1")
  expect_true("plan <- ard |>" %in% code)
  expect_true("  normalize_ard() |>" %in% code)
  # a Rework (process code) starts from `data`, the normalized ARD: the
  # table's normalize_ard() first, then the code, then the plan from `data`
  q <- add_output(new_planner(), "T2", process_code = "data <- data |> dplyr::mutate(x = 1)")
  code <- program_code(q, "T2")
  i <- match("data <- ard |> normalize_ard()", code)
  expect_false(is.na(i))
  expect_identical(code[i + 1L], "data <- data |> dplyr::mutate(x = 1)")
  expect_true(match("plan <- table_plan(data)", code) > i + 1L)
  expect_false(any(startsWith(code, "saveRDS(")))
  expect_silent(parse(text = code))
})

test_that("a Rework follows the table's own normalize_ard() arguments", {
  p <- add_output(new_planner(), "AE")
  p$ard$datasets <- .normalize_ard_sheet(data.frame(dataset = "ADAE", path = "data/adam/adae.rds"), "datasets")
  p <- set_ard_rows(p, "analyses", "AE", data.frame(
    analysis_id = "TEAE", method = "hierarchical", dataset = "ADAE", by = "TRTA",
    variables = "AEBODSYS | AEDECOD", args = "over_variables = TRUE"))
  p <- set_sheet_rows(p, "tables", "AE", data.frame(
    cols = "TRTA", rows = "group1 = AEBODSYS",
    overall_label = "Subjects with at least one TEAE"))
  p$outputs$process_code[p$outputs$output_id == "AE"] <- "data <- data[data$variable != \"AGE\", ]"
  code <- program_code(p, "AE")
  i <- match("# normalize and rework", code)
  expect_identical(code[i + 1:6], c(
    "data <- ard |>",
    "  normalize_ard(",
    "    hierarchy = c(\"AEBODSYS\", \"AEDECOD\"),",
    "    overall = \"Subjects with at least one TEAE\"",
    "  )",
    "data <- data[data$variable != \"AGE\", ]"))
  expect_true(which(startsWith(code, "plan <- table_plan(data, cols = \"TRTA\""))[1L] > i)
  expect_silent(parse(text = code))
  # the arguments the table's plan says when it has no Rework
  q <- p
  q$outputs$process_code[q$outputs$output_id == "AE"] <- NA_character_
  expect_true(all(code[i + 2:4] %in% program_code(q, "AE")))
})

test_that("the sample's Rework is written as tflspec writes it: normalize_ard() first", {
  skip_on_cran()
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  p <- suppressMessages(create_sample_study(run = FALSE))$planner
  # (the Rework is one element, as it is written: split into lines)
  code <- unlist(strsplit(program_code(p, "T-14-2-2"), "
", fixed = TRUE))
  i <- match("# normalize and rework", code)
  expect_identical(code[i + 1L], "data <- ard |> normalize_ard()")
  # the lines tflspec's content code has before the plan, the Rework as written
  y <- .code_output(p, "T-14-2-2")
  want <- tflspec::tfl_content_code(.code_content(y), pipe = "|>")
  want <- want[seq_len(match("", want) - 1L)]
  expect_identical(code[i + seq_along(want)], want)
  expect_false("data <- normalize_ard(ard)" %in% code)
  expect_true(any(startsWith(code, "plan <- table_plan(data")))
})

test_that("an old Rework that normalizes itself gives the data the new form gives", {
  ard <- cards::ard_stack(cards::ADSL, .by = ARM, cards::ard_tabulate(variables = c(SEX, RACE)))
  made <- function(proc) {
    q <- add_output(new_planner(), "T2", process_code = proc)
    code <- program_code(q, "T2")
    expect_silent(parse(text = code))
    # (from the table's normalization to its plan)
    i <- match("# normalize and rework", code)
    env <- new.env()
    env$ard <- ard
    eval(parse(text = c("library(rtfreporter)",
                        code[(i + 1L):(match("plan <- table_plan(data)", code) - 1L)])), env)
    env$data
  }
  new <- made("data <- data[data$variable != \"RACE\", ]")
  old <- made("data <- normalize_ard(ard)\ndata <- data[data$variable != \"RACE\", ]")
  expect_false("RACE" %in% new$variable)
  expect_identical(old, new)
  # (one spare line: written after the table's own, it overwrites it)
  q <- add_output(new_planner(), "T2", process_code = "data <- normalize_ard(ard)")
  code <- program_code(q, "T2")
  i <- match("# normalize and rework", code)
  expect_identical(code[i + 1:2], c("data <- ard |> normalize_ard()", "data <- normalize_ard(ard)"))
})

test_that("a Rework that leaves no `data` is a definition that does not hold, in the program", {
  q <- add_output(new_planner(), "T2", process_code = "ard <- ard[ard$variable != \"RACE\", ]")
  code <- program_code(q, "T2")
  expect_silent(parse(text = code))
  expect_true(any(grepl("the definition of T2 does not hold: the data.steps leave no `data`",
                        code, fixed = TRUE)))
  # `data` anywhere is enough; code that does not parse is R's to say
  q$outputs$process_code <- "ard <- ard\ndata <- ard"
  expect_false(any(grepl("does not hold", program_code(q, "T2"), fixed = TRUE)))
  q$outputs$process_code <- "ard <- (ard"
  expect_false(any(grepl("does not hold", program_code(q, "T2"), fixed = TRUE)))
})

test_that("a report's ARD read by its own name (data.object), ard by default", {
  p <- sample_planner()
  expect_identical(.ard_object(p, "PK"), "ard")
  code <- .fill_template("table_data", p, "PK")
  expect_true(any(startsWith(strsplit(code, "\n")[[1]], "ard <- readRDS(")))
  p$spec$outputs$PK <- list(data = list(object = "ard_pk"))
  expect_identical(.ard_object(p, "PK"), "ard_pk")
  lines <- strsplit(.fill_template("table_data", p, "PK"), "\n")[[1]]
  expect_true(any(startsWith(lines, "ard_pk <- readRDS(")))
  expect_false(any(grepl("(^|[^[:alnum:]_./])ard([^[:alnum:]_./]|$)", lines)))
  # (the study ARD's path, output/ard/ard.rds, kept)
  expect_true(any(grepl("ard.rds", lines, fixed = TRUE)))
  # (a name R cannot use: ard)
  p$spec$outputs$PK$data$object <- "1x"
  expect_identical(.ard_object(p, "PK"), "ard")
})
