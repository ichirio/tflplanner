# [Data] -> ADaM levels (#742): the file whose levels study$adam gets,
# chosen and applied; nothing applied is none.

example_define <- function() {
  f <- system.file("extdata", "define", "define.xml", package = "tflspec")
  testthat::skip_if_not(nzchar(f), "tflspec's example define.xml is not installed")
  testthat::skip_if_not_installed("xml2")
  f
}

# a study folder with an ADaM folder (no programs written: the functions only)
adam_folder <- function(env = parent.frame()) {
  d <- withr_tempdir(env)
  dir.create(file.path(d, study_layout()[["adam"]]), recursive = TRUE)
  d
}

# a planner whose spec holds only what the screen reads: adam_meta's rows
# and the kept files' text
fake_levels <- function(source = NA_character_, file = NA_character_, applied = NA_character_,
                        rows = 0L, kept = list()) {
  p <- new_planner()
  p$study[c("adam_levels", "adam_levels_file", "adam_levels_applied")] <- c(source, file, applied)
  p$spec <- list(study = list(adam_meta = list(
    variables = data.frame(dataset = rep("ADSL", rows), variable = rep("SEX", rows)),
    codelists = data.frame(codelist = rep("CL.SEX", rows)))),
    text = kept)
  p
}

test_that("the source comes from the file's extension; another is refused; ~ is the home folder", {
  expect_identical(.adam_levels_kind("data/adam/define.xml"), "define_xml")
  expect_identical(.adam_levels_kind("C:/x/Define.XML"), "define_xml")
  expect_identical(.adam_levels_kind("spec.xlsx"), "define_spec")
  expect_identical(.adam_levels_kind("spec.xls"), "define_spec")
  expect_identical(.adam_levels_kind("levels.csv"), NA_character_)
  expect_identical(.adam_levels_kind(""), NA_character_)
  d <- adam_folder()
  writeLines("a,b", file.path(d, "levels.csv"))
  p <- new_planner()
  expect_error(.adam_levels_apply(p, "levels.csv", d), "define.xml (.xml) or a Define Spec", fixed = TRUE)
  expect_error(.adam_levels_apply(p, "", d), "first")
  expect_error(.adam_levels_apply(p, "gone.xml", d), "There is no file")
  expect_identical(.adam_levels_path("~/define.xml", d),
                   normalizePath(path.expand("~/define.xml"), "/", mustWork = FALSE))
  expect_false(startsWith(.adam_levels_rel("~/define.xml", d), "~"))
})

test_that("the status: what is applied, then the file, when and how much; none one line", {
  p <- new_planner()
  expect_identical(.study_adam_levels(p), "none")
  expect_identical(.adam_levels_status_text(.adam_levels_state(p)), "No levels are applied")
  p$study[["adam_levels"]] <- "codelists"
  expect_identical(.study_adam_levels(p), "none")
  p <- fake_levels("define_xml", "data/adam/define.xml", "2026-11-22T10:15:30", rows = 3L)
  expect_identical(.adam_levels_counts(p), c(variables = 3L, codelists = 1L))
  expect_identical(.adam_levels_status_text(.adam_levels_state(p), counts = .adam_levels_counts(p)),
                   c("The levels of a define.xml are applied",
                     "data/adam/define.xml, applied 2026-11-22 10:15, 3 variables, 1 code lists"))
  ja <- .adam_levels_status_text(.adam_levels_state(p), function(x) tr(x, "ja"), .adam_levels_counts(p))
  expect_identical(ja[2L], "data/adam/define.xml\u30fb2026-11-22 10:15 \u9069\u7528\u30fb3 \u5909\u6570\u30fb1 \u30b3\u30fc\u30c9\u30ea\u30b9\u30c8")
  p$study[["adam_levels"]] <- "define_spec"
  expect_match(.adam_levels_status_text(.adam_levels_state(p))[1L], "^The levels of a Define Spec")
  expect_identical(tr("No levels are applied", "ja"), "\u6c34\u6e96\u3092\u5f53\u3066\u3066\u3044\u307e\u305b\u3093")
})

test_that("an old study: codelists and none dropped to not set; a notice when its levels may have been applied", {
  # codelists: dropped, said
  lv <- .adam_levels_drop_old(fake_levels("codelists"))
  expect_identical(.study_setting(lv$p, "adam_levels"), NA_character_)
  expect_true(lv$notice)
  # not set (the old default) with adam_meta: rows: said, nothing to drop
  p <- fake_levels(rows = 2L)
  lv <- .adam_levels_drop_old(p)
  expect_identical(lv$p, p)
  expect_true(lv$notice)
  # not set, no rows: nothing
  p <- fake_levels()
  expect_identical(.adam_levels_drop_old(p), list(p = p, notice = FALSE))
  # none: dropped to not set, said only with rows
  lv <- .adam_levels_drop_old(fake_levels("none"))
  expect_identical(.study_setting(lv$p, "adam_levels"), NA_character_)
  expect_false(lv$notice)
  expect_true(.adam_levels_drop_old(fake_levels("none", rows = 1L))$notice)
  # define_xml: as it is
  p <- fake_levels("define_xml", "data/adam/define.xml", "2026-11-22T10:15:30", rows = 1L)
  expect_identical(.adam_levels_drop_old(p), list(p = p, notice = FALSE))
})

test_that("Clear the choice has something to do: levels applied, a file chosen, or adam_meta: rows", {
  expect_false(.adam_levels_clearable(fake_levels(), ""))
  expect_true(.adam_levels_clearable(fake_levels(), "data/adam/define.xml"))
  expect_true(.adam_levels_clearable(fake_levels(rows = 1L), ""))
  expect_true(.adam_levels_clearable(fake_levels("define_xml", "x.xml", "2026-11-22T10:15:30", 1L), ""))
})

test_that("the ADaM folder's one define.xml is the candidate; two, or none, no candidate", {
  d <- adam_folder()
  expect_identical(.adam_levels_candidate(d), NA_character_)
  writeLines("<x/>", file.path(d, study_layout()[["adam"]], "define.xml"))
  expect_identical(.adam_levels_candidate(d), "data/adam/define.xml")
  writeLines("<x/>", file.path(d, study_layout()[["adam"]], "define2.xml"))
  expect_identical(.adam_levels_candidate(d), NA_character_)
  expect_identical(.adam_levels_candidate(NULL), NA_character_)
})

test_that("pending: another file chosen, or the applied file's content changed (not its time)", {
  d <- adam_folder()
  f <- file.path(d, "data", "adam", "define.xml")
  writeBin(charToRaw("<x>\n<y/>\n</x>\n"), f)
  writeLines("<x/>", file.path(d, "data", "adam", "other.xml"))
  # nothing chosen: nothing; chosen, none applied: another file
  expect_identical(.adam_levels_pending(fake_levels(), "", d), "")
  expect_identical(.adam_levels_pending(fake_levels(), "data/adam/define.xml", d), "other")
  kept <- stats::setNames(list(c(paste0("# md5: ", .adam_levels_md5(f)), "adam_meta: {}")),
                          .adam_meta_kept_rel("define_xml"))
  p <- fake_levels("define_xml", "data/adam/define.xml", "2026-11-22T10:15:30", 1L, kept)
  expect_identical(.adam_levels_pending(p, "data/adam/define.xml", d), "")
  # (the full path is the same file; a newer time with the same content too)
  expect_identical(.adam_levels_pending(p, f, d), "")
  Sys.setFileTime(f, Sys.time() + 3600)
  expect_identical(.adam_levels_pending(p, "data/adam/define.xml", d), "")
  # the same define.xml with CRLF line ends (a checkout's): the same
  writeBin(charToRaw("<x>\r\n<y/>\r\n</x>\r\n"), f)
  expect_identical(.adam_levels_pending(p, "data/adam/define.xml", d), "")
  expect_identical(.adam_levels_pending(p, "data/adam/other.xml", d), "other")
  writeBin(charToRaw("<x>\n<z/>\n</x>\n"), f)
  expect_identical(.adam_levels_pending(p, "data/adam/define.xml", d), "changed")
  expect_identical(tr(.adam_levels_pending_msg[["changed"]], "ja"),
                   "\u9069\u7528\u3057\u305f\u5f8c\u306b\u30d5\u30a1\u30a4\u30eb\u304c\u5909\u308f\u3063\u3066\u3044\u307e\u3059\u3002\u3082\u3046\u4e00\u5ea6\uff3b\u9069\u7528\uff3d\u3057\u3066\u304f\u3060\u3055\u3044\u3002")
  # the kept file read from the study folder when the spec has none (saved)
  dir.create(file.path(d, "spec", "adam_meta"), recursive = TRUE)
  writeLines(c("# md5: 0", "adam_meta: {}"), file.path(d, .adam_meta_kept_rel("define_xml")))
  p$spec$text <- list()
  expect_identical(.adam_levels_pending(p, "data/adam/define.xml", d), "changed")
})

test_that("a define's code lists become adam_meta: rows tflspec reads back as they were", {
  def <- tflspec::tfl_read_define(example_define())
  meta <- .adam_meta_from_define(def)
  block <- .adam_meta_block_lines(meta, "define_xml", "data/adam/define.xml")
  expect_match(block[1L], "define.xml data/adam/define.xml", fixed = TRUE)
  back <- .adam_meta_parse(block)
  # each variable with a code list the define has (an external one left out)
  v <- def$variables[!is.na(def$variables$codelist) & def$variables$codelist %in% def$codelists$codelist, ]
  expect_identical(paste(back$variables$dataset, back$variables$variable),
                   paste(v$dataset, v$variable))
  sex <- back$codelists[back$codelists$codelist == "CL.SEX", ]
  want <- def$codelists[def$codelists$codelist == "CL.SEX", ]
  expect_identical(sex$value, want$value)
  expect_identical(sex$decode, want$decode)
  expect_identical(as.integer(sex$order), as.integer(want$order))
  # values YAML could misread ("Y", "<65") come back as text
  expect_true(all(c("Y", "<65", ">=65") %in% back$codelists$value))
  # none: a study with none
  expect_identical(nrow(.adam_meta_parse(character())$codelists), 0L)
})

test_that("the adam_meta: block is put in place, before styles:, and taken out with its comment", {
  old <- c("codelists:", "  SEX: []", "# The ADaM metadata", "adam_meta:", "  variables: []",
           "# the page", "styles:", "  standard: {}")
  new <- .set_study_block(old, "adam_meta", c("# new", "adam_meta:", "  codelists: []"))
  expect_identical(new, c("codelists:", "  SEX: []", "# new", "adam_meta:", "  codelists: []",
                          "# the page", "styles:", "  standard: {}"))
  expect_identical(.set_study_block(old, "adam_meta"),
                   c("codelists:", "  SEX: []", "# the page", "styles:", "  standard: {}"))
  expect_identical(.set_study_block(c("a: 1"), "adam_meta", "adam_meta: {}"), c("a: 1", "adam_meta: {}"))
  expect_identical(.study_block_range(old, "adam_meta"), 3:5)
})

test_that("study.yml's own settings replace a key tflspec wrote: quoted or not, with its lines under it", {
  f <- tempfile(fileext = ".yml")
  on.exit(unlink(f))
  writeLines(c("study_id: S", "settings:", "  output_path: output/tfl", "  'adam_levels': codelists",
               "  adam_levels_file:", "    - old.xml", "    - older.xml", "  style: standard",
               "keys:", "  id: USUBJID"), f)
  .study_yml_own_settings(f, c(adam_levels = "define_xml", adam_levels_file = "data/adam/define.xml"))
  y <- yaml::read_yaml(f)
  expect_identical(y$settings$adam_levels, "define_xml")
  expect_identical(y$settings$adam_levels_file, "data/adam/define.xml")
  expect_identical(y$settings$output_path, "output/tfl")
  expect_identical(y$settings$style, "standard")
  expect_identical(y$keys$id, "USUBJID")
  expect_identical(sum(grepl("adam_levels:", readLines(f), fixed = TRUE)), 1L)
})

test_that("applying a define.xml, a Define Spec; a file with no code list, a broken file change nothing; clearing", {
  skip_on_cran()
  skip_if_no_define_levels()
  local_home()
  s <- suppressMessages(create_study("LV1", planner = add_output(new_planner(), "T1", description = "a table")))
  adam <- file.path(s$path, study_layout()[["adam"]])
  dir.create(adam, recursive = TRUE, showWarnings = FALSE)
  file.copy(example_define(), file.path(adam, "define.xml"))
  p <- s$planner
  # a broken file: its message, nothing changed
  writeLines("not xml <", file.path(adam, "broken.xml"))
  expect_error(.adam_levels_apply(p, "data/adam/broken.xml", s$path), "could not be read")
  # a Define Spec that maps no variable to a code list: its message
  none_xlsx <- file.path(adam, "nolists.xlsx")
  openxlsx::write.xlsx(list(
    Datasets = data.frame(Dataset = "ADSL", Description = "Subject-Level Analysis Dataset"),
    Variables = data.frame(Order = 1L, Dataset = "ADSL", Variable = "AGE", Label = "Age", Codelist = NA),
    Codelists = data.frame(ID = "CL.SEX", Name = "Sex", Order = 1:2, Term = c("M", "F"),
                           `Decoded Value` = c("Male", "Female"), check.names = FALSE)), none_xlsx)
  expect_error(.adam_levels_apply(p, "data/adam/nolists.xlsx", s$path), "no variable to a code list")
  # a block written by hand in defaults.yml: kept as previous.yml when replaced
  dflt <- file.path("spec", "defaults.yml")
  p <- .spec_ensure(p)
  hand <- c("adam_meta:", "  variables:", "  - {dataset: ADSL, variable: SEX, codelist: CL.X}",
            "  codelists:", "  - {codelist: CL.X, value: F}")
  p$spec$text[[dflt]] <- .set_study_block(p$spec$text[[dflt]], "adam_meta", hand)
  # the define.xml
  now <- Sys.time() + 2
  p <- .adam_levels_apply(p, file.path(adam, "define.xml"), s$path, now = now)
  expect_identical(unname(p$study[c("adam_levels", "adam_levels_file", "adam_levels_applied")]),
                   c("define_xml", "data/adam/define.xml", format(now, "%Y-%m-%dT%H:%M:%S")))
  expect_true("CL.SEX" %in% p$spec$study$adam_meta$codelists$codelist)
  expect_identical(p$spec$text[[.adam_meta_kept_rel("previous")]][-1L], hand)
  expect_identical(p$spec$text[[.adam_meta_kept_rel("define_xml")]][1L],
                   paste0("# md5: ", .adam_levels_md5(file.path(adam, "define.xml"))))
  s$planner <- p
  s <- suppressMessages(save_study(s))
  y <- yaml::read_yaml(file.path(s$path, "study.yml"))$settings
  expect_identical(y$adam_levels, "define_xml")
  expect_identical(y$adam_levels_file, "data/adam/define.xml")
  expect_true(all(file.exists(file.path(s$path, .adam_meta_kept_rel(c("define_xml", "previous"))))))
  d <- yaml::read_yaml(file.path(s$path, "spec", "defaults.yml"))
  expect_true("CL.SEX" %in% vapply(d$adam_meta$codelists, `[[`, "", "codelist"))
  s2 <- suppressMessages(open_study("LV1"))
  expect_identical(.study_adam_levels(s2$planner), "define_xml")
  expect_identical(.adam_levels_pending(s2$planner, "data/adam/define.xml", s2$path), "")
  # the applied file changed afterwards: pending, "changed"
  cat("<!-- changed -->\n", file = file.path(adam, "define.xml"), append = TRUE)
  expect_identical(.adam_levels_pending(s2$planner, "data/adam/define.xml", s2$path), "changed")
  # a Define Spec workbook
  xlsx <- file.path(adam, "spec.xlsx")
  openxlsx::write.xlsx(list(
    Datasets = data.frame(Dataset = "ADSL", Description = "Subject-Level Analysis Dataset"),
    Variables = data.frame(Order = 1:2, Dataset = "ADSL", Variable = c("SEX", "AGE"),
                           Label = c("Sex", "Age"), Codelist = c("CL.SEX", NA)),
    Codelists = data.frame(ID = "CL.SEX", Name = "Sex", Order = 1:2, Term = c("M", "F"),
                           `Decoded Value` = c("Male", "Female"), check.names = FALSE)), xlsx)
  s2$planner <- .adam_levels_apply(s2$planner, "data/adam/spec.xlsx", s2$path)
  expect_identical(.study_adam_levels(s2$planner), "define_spec")
  expect_identical(s2$planner$spec$study$adam_meta$codelists$value, c("M", "F"))
  # (the block replaced was the screen's: no new previous.yml)
  expect_null(s2$planner$spec$text[[.adam_meta_kept_rel("previous")]])
  s2 <- suppressMessages(save_study(s2))
  expect_true(file.exists(file.path(s2$path, "spec", "adam_meta", "define_spec.yml")))
  # cleared: none; adam_meta: gone from defaults.yml, the kept files stay
  s2$planner <- .adam_levels_clear(s2$planner, s2$path)
  expect_false("adam_levels" %in% s2$planner$ard$study$key)
  s2 <- suppressMessages(save_study(s2))
  expect_false(any(grepl("adam_levels", readLines(file.path(s2$path, "study.yml")))))
  expect_null(yaml::read_yaml(file.path(s2$path, "spec", "defaults.yml"))$adam_meta)
  expect_true(all(file.exists(file.path(s2$path, "spec", "adam_meta",
                                        c("define_xml.yml", "define_spec.yml")))))
  expect_identical(.study_adam_levels(suppressMessages(open_study("LV1"))$planner), "none")
})

test_that("clearing a block written by hand keeps it as spec/adam_meta/previous.yml", {
  skip_on_cran()
  skip_if_no_define_levels()
  local_home()
  s <- suppressMessages(create_study("LV3", planner = add_output(new_planner(), "T1", description = "a table")))
  p <- .spec_ensure(s$planner)
  dflt <- file.path("spec", "defaults.yml")
  hand <- c("# by hand", "adam_meta:", "  variables: []")
  p$spec$text[[dflt]] <- .set_study_block(p$spec$text[[dflt]], "adam_meta", hand)
  p <- .adam_levels_clear(p, s$path)
  expect_identical(p$spec$text[[.adam_meta_kept_rel("previous")]][-1L], hand)
  expect_false(any(grepl("^adam_meta:", p$spec$text[[dflt]])))
})

test_that("[Study] has no ADaM levels field", {
  skip_on_cran()
  skip_if_quick()
  local_home()
  suppressMessages(create_study("LV4", planner = add_output(new_planner(), "T1", description = "a table")))
  suppressWarnings(shiny::testServer(server_for("LV4"), {
    html <- output$study_detail$html
    expect_match(html, "study_treatment", fixed = TRUE)
    expect_false(grepl("study_adam_levels", html, fixed = TRUE))
  }))
  expect_false(grepl("study_adam_levels", as.character(app_ui("ja")), fixed = TRUE))
})

test_that("the screen: [Data] has ADaM levels; an old codelists is dropped on open, with a notice", {
  skip_on_cran()
  skip_if_quick()
  skip_if_no_define_levels()
  html <- as.character(app_ui("ja"))
  expect_match(html, "adam_levels_path", fixed = TRUE)
  expect_match(html, "ADaM \u306e\u4e8b\u524d\u52a0\u5de5", fixed = TRUE)
  local_home()
  suppressMessages(create_study("LV2", planner = add_output(new_planner(), "T1", description = "a table")))
  # a study saved before #742: settings: adam_levels: codelists in its state
  orig <- open_study
  local_mocked_bindings(open_study = function(...) {
    s <- orig(...)
    s$planner$study[["adam_levels"]] <- "codelists"
    s
  })
  notes <- character()
  local_mocked_bindings(showNotification = function(ui, ...) {
    notes <<- c(notes, paste(as.character(ui), collapse = " "))
    invisible()
  }, .package = "shiny")
  suppressWarnings(shiny::testServer(server_for("LV2"), {
    rv <- session$userData$rv
    expect_identical(.study_setting(rv$p, "adam_levels"), NA_character_)
    expect_true(session$userData$dirty())
    expect_match(output$adam_levels_status$html, "No levels are applied", fixed = TRUE)
  }))
  expect_true(any(grepl("The ADaM levels setting changed", notes, fixed = TRUE)))
})

test_that("a file chosen and not applied stays in the field through a save, still pending", {
  skip_on_cran()
  skip_if_quick()
  skip_if_no_define_levels()
  local_home()
  suppressMessages(create_study("LV5", planner = add_output(new_planner(), "T1", description = "a table")))
  sent <- list()
  local_mocked_bindings(updateTextInput = function(session, inputId, ...) {
    sent[[length(sent) + 1L]] <<- list(id = inputId, value = list(...)$value)
    invisible()
  }, .package = "shiny")
  suppressWarnings(shiny::testServer(server_for("LV5"), {
    rv <- session$userData$rv
    session$setInputs(adam_levels_path = "data/adam/define.xml")
    expect_match(output$adam_levels_status$html, "The file chosen is not applied yet", fixed = TRUE)
    n <- length(sent)
    # another edit, then a save: the study is set again, the field is not
    session$setInputs(study_treatment = "TRT01P")
    session$setInputs(save = 1L)
    expect_false(session$userData$dirty())
    ids <- vapply(sent[-seq_len(n)], `[[`, "", "id")
    expect_false("adam_levels_path" %in% ids)
    expect_match(output$adam_levels_status$html, "The file chosen is not applied yet", fixed = TRUE)
  }))
})
