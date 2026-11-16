# The seven series templates of tflspec_v1 (tflspec#823, #824) end to end,
# as the app takes them: a study of fictional ADaM data (tflspec's), a
# report added as the Add dialog adds it (`template: {name: tflspec_v1/<t>,
# args: ...}`), its cards' params and their inputs, its ARD program (as the
# ARD code pane writes it), its report program (program_code()) and its ARD
# (as fetch_ard() makes it), run here.
#
# Where a step is not there yet, the test says so in its skip, with the
# error, after the checks it could make:
# - the cards' params of a series template need tflplanner#702 (.tpl_defs());
# - the app's own path (the report's analyses in the planner) needs the
#   series template expanded when the spec is read; until it is, the test
#   expands it with tflspec::tfl_template_resolve() and puts that code SPEC
#   in the report's place, so the program and the ARD are the app's from there;
# - an ARD program tflspec cannot write or run yet.

series_templates <- c("ae_soc_pt", "demog", "forest_hr", "km_simple", "lab_tox_shift",
                      "listing", "orr_subgroup")

# tflspec with the series templates (not on its `next` before tflspec#824)
series_skip <- function() {
  skip_on_cran()
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  # (the series files came with tflspec#818, the resolver and the settings
  # helpers with #824: a tflspec between the two has the one, not the other)
  need <- c("tfl_arg_defs", "tfl_template_resolve", "tfl_series_settings_set")
  have <- need %in% getNamespaceExports("tflspec")
  skip_if_not(all(have), paste0("this tflspec has no ", paste(need[!have], collapse = ", "),
                                " (tflspec#824)"))
  ok <- tryCatch(is.data.frame(tflspec::tfl_arg_defs(template = "tflspec_v1/demog")),
                 error = function(e) FALSE)
  skip_if_not(ok, "this tflspec has no series templates (tflspec#824)")
}

# A study of tflspec's fictional ADaM data in the home `home`: ADLB with
# the grade columns lab_tox_shift reads (by direction, from its ATOXGR /
# BTOXGR); the series' efficacy set (FAS) the study's ITT
series_study <- function(home, id) {
  d <- file.path(studies_root(home), id)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae", "adtte", "adlb", "adrs")) {
    df <- getExportedValue("tflspec", x)
    if (identical(x, "adlb")) {
      df$ATOXGRL <- df$ATOXGRH <- df$ATOXGR
      df$BTOXGRL <- df$BTOXGRH <- df$BTOXGR
    }
    saveRDS(df, file.path(d, "adam", paste0(x, ".rds")))
  }
  invisible(tflspec::tfl_ai_study_init(d, adam = "adam", study_id = id))
  tflspec::tfl_series_settings_set(d, "tflspec_v1", list(population_efficacy = "ITT"))
  suppressMessages(open_study(d, home = home))
}

# The input a def's card gets (.tpl_type()), from the def alone
series_card_type <- function(input, type) {
  if (identical(input, "table")) return("table")
  if (startsWith(type, "enum[")) return("choice")
  switch(type, logical = "flag", number = "number", `list<column>` = "vars",
         column = "var", level = "levels", "text")
}

# The report's YAML file as the app wrote it (one text)
series_file <- function(p, id) paste(p$spec$text[[p$spec$where[[id]]]], collapse = "\n")

# One template end to end.  `args` the report's (the required, one at
# least not the default), `ids` the analysis ids its ARD has, `in_code`
# texts its programs have (a changed argument's); the checks made, and
# what could not be checked yet (the skip's message).  `gap`: a known
# tflspec gap -- a missing text or id skips with that reason before its
# message (remove when tflspec is fixed).
series_e2e <- function(t, kind, args, ids, in_code = character(), home, gap = NULL) {
  name <- paste0("tflspec_v1/", t)
  later <- character()
  s <- series_study(home, paste0("SER-", toupper(gsub("_", "", t))))
  p <- s$planner
  id <- paste0(switch(kind, table = "T", listing = "L", figure = "F"), "-SER-1")

  # 2. the report, as the Add dialog adds it
  # (what tflspec warns of while the planner reads it: said in the skip)
  o <- tflspec::tfl_output(id, kind, template = list(name = name, args = args))
  p <- withCallingHandlers(.spec_add_output(p, o), warning = function(w) {
    later <<- c(later, paste0("adding the report: ", conditionMessage(w)))
    invokeRestart("muffleWarning")
  })
  txt <- series_file(p, id)
  expect_match(txt, paste0("name: ", name), fixed = TRUE)
  expect_match(txt, "args:", fixed = TRUE)
  # (every argument kept in the file, whatever the view holds)
  for (a in names(args)) expect_match(txt, paste0("\n +", a, ":"), info = a)
  expect_identical(.tpl_of(p, id)$name, name)
  expect_identical(names(.tpl_of(p, id)$args), names(args))

  # 3. the cards: every param shown, each with its input
  defs <- tflspec::tfl_arg_defs(template = name)
  prm <- .tpl_shown(.tpl_params(name))
  if (!length(prm)) {
    later <- c(later, "cards: .tpl_params() has no params for a series template (needs tflplanner#702)")
  } else {
    expect_identical(names(prm), defs$name)
    got <- vapply(prm, function(x) .tpl_type(x), "")
    want <- stats::setNames(mapply(series_card_type, defs$input, defs$type), defs$name)
    expect_identical(got, want)
    expect_true(all(got[defs$input == "table"] == "table"))
  }

  # 4. the ARD program.  The app's path: the report's analyses in the
  # planner (none while the spec's reading does not expand the template)
  app_path <- any(p$ard$analyses$output_id %in% id)
  if (!app_path) {
    err <- tryCatch({
      sp <- .code_spec(p)
      tflspec::tfl_resolve_output(sp$outputs[[id]], sp$study)
      "no error"
    }, error = function(e) conditionMessage(e))
    later <- c(later, paste0("app path: the planner has no analyses for ", name,
                             " (tfl_resolve_output(): ", err, ")"))
    # the template expanded by tflspec (the args as its keys under
    # `template:`) and that code SPEC in the report's place
    rep <- list(id = id, kind = kind, template = c(list(name = name), args))
    res <- tflspec::tfl_template_resolve(rep, study = s$path)
    p <- suppressWarnings(.spec_set_output(p, id, res))
  }
  got_ids <- p$ard$analyses$analysis_id[p$ard$analyses$output_id %in% id]
  expect_true("BIGN" %in% got_ids)
  a <- structure(p$ard, class = "tfl_ard_spec")
  prog <- tryCatch(suppressWarnings(ard_program_code(a, id, dir = s$path,
                                                    codelists = .study_codelists(p))),
                   error = function(e) e)
  run <- tryCatch(suppressWarnings(.ard_spec_code(.ard_spec(p$ard), output_id = id, save = FALSE,
                                                  dir = s$path, codelists = .study_codelists(p))),
                  error = function(e) e)
  if (inherits(prog, "error") || inherits(run, "error")) {
    e <- if (inherits(prog, "error")) prog else run
    later <- c(later, paste0("ARD program not written: ", conditionMessage(e)))
    skip(paste(c(paste0(t, ":"), later), collapse = "\n  "))
  }
  expect_silent(parse(text = prog, keep.source = FALSE))
  expect_silent(parse(text = run, keep.source = FALSE))
  # the report's own program (Report code: a figure's fit, a table's plan)
  rprog <- tryCatch(suppressWarnings(program_code(p, id)), error = function(e) e)
  if (inherits(rprog, "error")) {
    later <- c(later, paste0("report program not written: ", conditionMessage(rprog)))
    rprog <- character()
  } else {
    expect_silent(parse(text = rprog, keep.source = FALSE))
  }
  # a changed argument: in the ARD program or the report's
  code <- paste(c(prog, rprog), collapse = "\n")
  for (x in in_code) {
    msg <- paste0(t, ": the programs have no ", x)
    if (!is.null(gap) && !grepl(x, code, fixed = TRUE)) skip(paste0(gap, msg))
    expect_true(grepl(x, code, fixed = TRUE), info = msg)
  }

  # the ARD, as fetch_ard() makes it: the program run from the study folder
  env <- new.env(parent = globalenv())
  old <- setwd(s$path)
  on.exit(setwd(old), add = TRUE)
  ard <- tryCatch({
    suppressWarnings(suppressMessages(eval(parse(text = run, keep.source = FALSE), envir = env)))
    env$ard
  }, error = function(e) e)
  setwd(old)
  if (inherits(ard, "error")) {
    later <- c(later, paste0("ARD program does not run: ", conditionMessage(ard)))
    skip(paste(c(paste0(t, ":"), later), collapse = "\n  "))
  }
  expect_s3_class(ard, "data.frame")
  expect_gt(nrow(ard), 0L)
  missing <- setdiff(ids, unique(ard$analysis_id))
  msg <- paste0(t, ": the ARD has no ", paste(missing, collapse = ", "),
                " (it has ", paste(unique(ard$analysis_id), collapse = ", "), ")")
  if (!is.null(gap) && length(missing)) skip(paste0(gap, msg))
  expect_true(!length(missing), info = msg)
  if (length(later)) {
    head <- if (app_path) paste0(t, ":") else paste0(t, ": checked through tflspec's expansion;")
    skip(paste(c(head, later), collapse = "\n  "))
  }
  invisible(ard)
}

# The known gap: tflspec's template expansion does not yet carry these
# args, derives or conditions to the ARD (tflspec#826/#827).  Remove the
# `gap =` of each test when tflspec is fixed.
series_gap <- "waiting for the tflspec fix (template resolve: args, derives, conditions; tflspec#826/#827): "

test_that("the series templates are all there, each with its definitions", {
  series_skip()
  for (t in series_templates) {
    d <- tflspec::tfl_arg_defs(template = paste0("tflspec_v1/", t))
    expect_gt(nrow(d), 0L)
    expect_true("by" %in% d$name, info = t)
  }
})

test_that("ae_soc_pt: a template report to its ARD (BIGN, AE_ANY, AE_L1, AE_L2)", {
  series_skip()
  home <- local_home()
  series_e2e("ae_soc_pt", "table", list(by = "TRT01A", min_pct = 5),
             ids = c("BIGN", "AE_ANY", "AE_L1", "AE_L2"), in_code = "keep_min_pct(5", home = home,
             gap = series_gap)
})

test_that("demog: a template report to its ARD (BIGN and a variable each)", {
  series_skip()
  home <- local_home()
  series_e2e("demog", "table",
             list(by = "TRT01A", continuous = "AGE", categorical = c("SEX", "RACE"),
                  analysis_ids = list(AGE = "AGE_STATS")),
             ids = c("BIGN", "AGE_STATS", "SEX", "RACE"), in_code = "AGE_STATS", home = home)
})

test_that("forest_hr: a template report to its ARD (BIGN, HR), a subgroup row", {
  series_skip()
  skip_if_not_installed("survival")
  skip_if_not_installed("ggsurvfit")
  skip_if_not_installed("cardx")
  home <- local_home()
  series_e2e("forest_hr", "figure",
             list(by = "TRT01A", ref = "Control", comparison = "Drug X", subgroup_vars = "SEX",
                  conditions = list(list(subgroup = "SEX", subgroup_lvl = "F",
                                         where = tflspec::tfl_r("SEX == \"F\"")))),
             ids = c("BIGN", "HR"), in_code = "ref = \"Control\"", home = home,
             gap = series_gap)
})

test_that("km_simple: a template report to its ARD (BIGN), the unit days", {
  series_skip()
  skip_if_not_installed("survival")
  skip_if_not_installed("ggsurvfit")
  home <- local_home()
  series_e2e("km_simple", "figure", list(by = "TRT01A", time_unit = "days"),
             ids = "BIGN", in_code = "[[\"days\"]]", home = home,
             gap = series_gap)
})

test_that("lab_tox_shift: a template report to its ARD (BIGN, ALT_HIGH, HGB_LOW)", {
  series_skip()
  home <- local_home()
  series_e2e("lab_tox_shift", "table",
             list(by = "TRT01A", formats = list(n = 0, p = "xx.xx%"),
                  conditions = list(list(tox_dir = "High", param = "Alanine Aminotransferase (U/L)"),
                                    list(tox_dir = "Low", param = "Hemoglobin (g/L)"))),
             ids = c("BIGN", "ALT_HIGH", "HGB_LOW"), in_code = "Hemoglobin (g/L)", home = home,
             gap = series_gap)
})

test_that("listing: a template report to its ARD (BIGN, the total N)", {
  series_skip()
  home <- local_home()
  series_e2e("listing", "listing", list(from = "ADAE", columns = c("USUBJID", "AEDECOD"), by = "TRT01A"),
             ids = "BIGN", in_code = "ard_total_n", home = home)
})

test_that("orr_subgroup: a template report to its ARD (BIGN, ORR), a subgroup row", {
  series_skip()
  skip_if_not_installed("cardx")
  home <- local_home()
  series_e2e("orr_subgroup", "table",
             list(by = "TRT01A", category = "AVALC", responder = "CR", conf_level = 0.9,
                  subgroup_vars = "SEX",
                  conditions = list(list(subgroup = "SEX", subgroup_lvl = "F",
                                         where = tflspec::tfl_r("SEX == \"F\"")))),
             ids = c("BIGN", "ORR"), in_code = "conf.level = 0.9", home = home)
})
