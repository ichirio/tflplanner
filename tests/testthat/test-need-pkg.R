# A package of Suggests that is needed and not installed: tflplanner stops
# and names it, with how to install it -- never skips silently (#682).
# The package is said to be missing by mocking .pkg_installed().

missing_pkgs <- function(pkgs, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .pkg_installed = function(pkg) !pkg %in% pkgs, .env = env)
}

test_that(".need_pkg() names the package and how to install it, in en and ja", {
  missing_pkgs("haven")
  expect_true(.need_pkg(c("stats", "utils")))
  e <- tryCatch(.need_pkg(c("haven", "stats"), "Reading .xpt files"), error = function(e) e)
  expect_s3_class(e, "tflplanner_need_pkg")
  expect_identical(e$packages, "haven")
  expect_identical(conditionMessage(e), paste(
    "Reading .xpt files needs a package that is not installed.",
    "Install the haven package: install.packages(\"haven\")"))
  # without `what`: the install sentence alone; several: all named
  expect_identical(.need_pkg_message("haven"), "Install the haven package: install.packages(\"haven\")")
  expect_identical(.need_pkg_message(c("survival", "ggsurvfit"), lang = "en"),
                   "Install the survival & ggsurvfit packages: install.packages(c(\"survival\", \"ggsurvfit\"))")
  ja <- .need_pkg_message("haven", "X", lang = "ja")
  expect_match(ja, "install.packages(\"haven\")", fixed = TRUE)
  expect_false(grepl("Install the", ja, fixed = TRUE))
  # every text has its Japanese
  st <- .strings()
  keys <- c("Install the %s package: install.packages(%s)",
            "Install the %s packages: install.packages(%s)",
            "%s needs a package that is not installed.", "Reading .%s files",
            "The report %s", "The reports %s")
  expect_true(all(nzchar(st$ja[match(keys, st$en)])))
})

test_that("read_data_head(): a .xpt, .sas7bdat or .parquet file names haven / arrow", {
  missing_pkgs(c("haven", "arrow"))
  d <- withr_tempdir()
  for (ext in c("xpt", "sas7bdat", "parquet")) {
    f <- file.path(d, paste0("adsl.", ext))
    writeLines("x", f)
    pkg <- if (ext == "parquet") "arrow" else "haven"
    e <- tryCatch(read_data_head(f), error = function(e) e)
    expect_s3_class(e, "tflplanner_need_pkg")
    expect_identical(e$packages, pkg)
    expect_match(conditionMessage(e), sprintf("Reading .%s files", ext), fixed = TRUE)
    expect_match(conditionMessage(e), sprintf("install.packages(\"%s\")", pkg), fixed = TRUE)
  }
  # an .rds file needs neither
  f <- file.path(d, "adsl.rds")
  saveRDS(data.frame(AGE = 1:3), f)
  expect_identical(names(read_data_head(f)), "AGE")
})

test_that("the siera ARS's columns: a .xpt file with haven missing stops, not skipped", {
  d <- withr_tempdir()
  dir.create(file.path(d, "data"))
  writeLines("x", file.path(d, "data", "adsl.xpt"))
  saveRDS(data.frame(AGE = 1), file.path(d, "data", "adae.rds"))
  a <- list(datasets = data.frame(dataset = c("ADAE", "ADSL"),
                                  path = c("data/adae.rds", "data/adsl.xpt"),
                                  stringsAsFactors = FALSE))
  study <- list(path = d)
  missing_pkgs("haven")
  expect_error(.ars_columns(study, a), "install.packages(\"haven\")", fixed = TRUE,
               class = "tflplanner_need_pkg")
  # no .xpt: the columns as before
  a$datasets <- a$datasets[1, ]
  expect_identical(.ars_columns(study, a), list(ADAE = "AGE"))
})

test_that("the ARD functions of a package not installed stay listed, marked", {
  cat <- data.frame(call = c("cards::ard_summary", "cardx::ard_continuous_ci",
                             "cardx::ard_survival_survfit"), stringsAsFactors = FALSE)
  missing_pkgs("cardx")
  ch <- .ard_function_choices(cat)
  expect_true("cards::ard_summary" %in% ch)
  expect_true(all(c("cardx::ard_continuous_ci", "cardx::ard_survival_survfit") %in% ch))
  # the form's list: kept, with the packages it needs
  f <- data.frame(call = cat$call, installed = c(TRUE, FALSE, FALSE),
                  stringsAsFactors = FALSE)
  o <- .ard_functions_offered(f)
  expect_identical(o$call, cat$call)
  expect_identical(o$needs, c("", "cardx", "cardx"))
  # cardx there, survival not: ard_survival_*() needs survival
  missing_pkgs("survival")
  f$installed <- TRUE
  o <- .ard_functions_offered(f)
  expect_identical(o$needs, c("", "", "survival"))
  # (a function the installed version lacks, its package there: not listed)
  f$installed[2] <- FALSE
  expect_false("cardx::ard_continuous_ci" %in% .ard_functions_offered(f)$call)
  # choosing one says what to install
  expect_true(.need_ard_methods(c("cards::ard_summary", NA)))
  expect_error(.need_ard_methods("cardx::ard_survival_survfit"),
               "Install the survival package", class = "tflplanner_need_pkg")
})

test_that("the analysis form: a function needing a missing package is marked, choosing it says so", {
  skip_on_cran()
  local_home()
  s <- create_study("NP")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADSL, file.path(s$path, "data/adam/adsl.rds"))
  s$planner <- first_table(s$planner, "T1", "data/adam/adsl.rds", cards::ADSL,
                           "SAFFL", "TRT01A", c("AGE", "SEX"))
  save_study(s)
  missing_pkgs("survival")
  notes <- character()
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) notes <<- c(notes, paste(as.character(ui), collapse = "")),
    .package = "shiny")
  suppressWarnings(shiny::testServer(server_for("NP"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "ard", target = "T1")
    session$setInputs(hot_ard_analyses_select = list(select = list(r = 2L)))
    st_env <- session$userData$st_env
    id <- function(x) paste0("st", st_env$n, "_", x)
    session$setInputs(ard_fn_opened = 1)
    inp <- list(); inp[[id("fn_q")]] <- "survfit"
    do.call(session$setInputs, inp)
    h <- output$ard_fn_list$html
    # listed (a choice), with the package to install
    expect_match(h, "value=\"cardx::ard_survival_survfit\"", fixed = TRUE)
    expect_match(h, "Install the survival package", fixed = TRUE)
    before <- output$ard_fn_now$html
    inp <- list(); inp[[id("fn_pick")]] <- "cardx::ard_survival_survfit"
    do.call(session$setInputs, inp)
    expect_true(any(grepl("install.packages(&quot;survival&quot;)", notes, fixed = TRUE) |
                      grepl("install.packages(\"survival\")", notes, fixed = TRUE)))
    # the method left as it was
    expect_identical(output$ard_fn_now$html, before)
  }))
})

test_that("a template's packages: its own, through the template it is made from", {
  tt <- data.frame(name = c("km", "km_survival", "co_km", "demog", "ancova"),
                   from = c(NA, "km", "km_survival", NA, NA), stringsAsFactors = FALSE)
  expect_identical(.template_needs("km_survival", tt), c("survival", "ggsurvfit"))
  expect_identical(.template_needs("co_km", tt), c("survival", "ggsurvfit"))
  expect_identical(.template_needs("ancova", tt), "emmeans")
  expect_identical(.template_needs("demog", tt), character())
  expect_identical(.template_needs(NA_character_, tt), character())
  # the real catalog: tflspec's km variants
  real <- tflspec::tfl_templates()
  skip_if_not("km_cumulative" %in% real$name)
  expect_true(all(c("survival", "ggsurvfit") %in% .template_needs("km_cumulative", real)))
  # code that names a package, or a model's package
  expect_identical(.pkgs_in_code("fit <- mmrm::mmrm(AVAL ~ ARM, data = d)"), "mmrm")
  expect_identical(.pkgs_in_code("cardx::ard_regression(x, method = \"lmer\", package = \"mmrm\")"), "mmrm")
  expect_identical(.pkgs_in_code("cardx::ard_emmeans_contrast(data)"), "emmeans")
  expect_identical(.pkgs_in_code("my_ard_survival_x(data)"), character())
})

test_that("a template report whose package is missing stops before it is run", {
  p <- add_output(add_output(new_planner(), "F1"), "T2")
  p$spec <- list(outputs = list(F1 = list(template = list(name = "km_survival", args = list()))))
  tt <- data.frame(name = c("km", "km_survival"), from = c(NA, "km"), stringsAsFactors = FALSE)
  expect_true(all(c("survival", "ggsurvfit") %in% .report_needs(p, "F1", tt)))
  expect_identical(.report_needs(p, "T2", tt), character())
  missing_pkgs("ggsurvfit")
  testthat::local_mocked_bindings(tfl_templates = function(...) tt, .package = "tflspec")
  e <- tryCatch(.need_report_pkgs(p, c("F1", "T2")), error = function(e) e)
  expect_s3_class(e, "tflplanner_need_pkg")
  expect_identical(e$packages, "ggsurvfit")
  expect_match(conditionMessage(e), "The report F1 needs a package", fixed = TRUE)
  expect_true(.need_report_pkgs(p, "T2"))
  # the reports a batch runs
  expect_identical(.batch_report_ids(p), c("F1", "T2"))
  expect_identical(.batch_report_ids(p, only = "T2"), "T2")
  expect_identical(.batch_report_ids(p, exclude = "T2"), "F1")
})

test_that("run_study(), run_batch() and preview_code() check the reports' packages first", {
  local_home()
  p <- add_output(new_planner(), "F1", process_code = "content <- 1")
  s <- create_study("PK", planner = p)
  missing_pkgs("ggsurvfit")
  testthat::local_mocked_bindings(.report_needs = function(p, output_id, templates = NULL)
    if (identical(output_id, "F1")) c("survival", "ggsurvfit") else character())
  expect_error(run_study(s, "F1", wait = FALSE), "Install the ggsurvfit package",
               class = "tflplanner_need_pkg")
  expect_error(preview_code(s, "F1"), "install.packages(\"ggsurvfit\")", fixed = TRUE,
               class = "tflplanner_need_pkg")
  # the official run: said before the run starts (no batch folder made)
  dir.create(file.path(s$path, "programs", "tfl"), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(f <- file.path(s$path, "programs", "tfl", "autoexec_report.R"))) {
    writeLines("", f)
  }
  expect_error(run_batch(s, "tfl", only = "F1"), "The report F1",
               class = "tflplanner_need_pkg")
  expect_length(list_batches(s)$batch, 0L)
})
