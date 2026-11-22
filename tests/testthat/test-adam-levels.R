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

test_that("the source comes from the file's extension; another is refused", {
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
})

test_that("not set, and an old value, are none; the status line says which file and when", {
  p <- new_planner()
  expect_identical(.study_adam_levels(p), "none")
  expect_identical(.adam_levels_status_text(.adam_levels_state(p)), "No levels are applied (none)")
  p$study[["adam_levels"]] <- "codelists"
  expect_identical(.study_adam_levels(p), "none")
  p$study[c("adam_levels", "adam_levels_file", "adam_levels_applied")] <-
    c("define_xml", "data/adam/define.xml", "2026-11-22T10:15:30")
  expect_identical(.adam_levels_status_text(.adam_levels_state(p)),
                   "The levels of a define.xml are applied: data/adam/define.xml (2026-11-22 10:15)")
  p$study[["adam_levels"]] <- "define_spec"
  expect_match(.adam_levels_status_text(.adam_levels_state(p)), "^The levels of a Define Spec")
  expect_match(.adam_levels_status_text(.adam_levels_state(p), function(x) tr(x, "ja")),
               "Define Spec \u306e\u6c34\u6e96\u3092\u5f53\u3066\u3066\u3044\u307e\u3059")
})

test_that("an old study's codelists is dropped (no alias); define_xml and none are kept", {
  p <- new_planner()
  p$study[["adam_levels"]] <- "codelists"
  q <- .adam_levels_drop_old(p)
  expect_identical(.study_setting(q, "adam_levels"), NA_character_)
  p$study[["adam_levels"]] <- "none"
  expect_identical(.adam_levels_drop_old(p), p)
  p$study[["adam_levels"]] <- "define_xml"
  expect_identical(.adam_levels_drop_old(p), p)
  expect_identical(.adam_levels_drop_old(new_planner()), new_planner())
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

test_that("pending: another file chosen, or the applied file changed since", {
  d <- adam_folder()
  f <- file.path(d, "data", "adam", "define.xml")
  writeLines("<x/>", f)
  writeLines("<x/>", file.path(d, "data", "adam", "other.xml"))
  p <- new_planner()
  # nothing chosen: nothing pending; chosen, none applied: pending
  expect_false(.adam_levels_pending(p, "", d))
  expect_true(.adam_levels_pending(p, "data/adam/define.xml", d))
  at <- Sys.time() - 60
  Sys.setFileTime(f, at - 60)
  p$study[c("adam_levels", "adam_levels_file", "adam_levels_applied")] <-
    c("define_xml", "data/adam/define.xml", format(at, "%Y-%m-%dT%H:%M:%S"))
  expect_false(.adam_levels_pending(p, "data/adam/define.xml", d))
  # (the full path is the same file)
  expect_false(.adam_levels_pending(p, f, d))
  expect_true(.adam_levels_pending(p, "data/adam/other.xml", d))
  Sys.setFileTime(f, at + 30)
  expect_true(.adam_levels_pending(p, "data/adam/define.xml", d))
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
})

test_that("applying a define.xml, a Define Spec; clearing; a broken file changes nothing", {
  skip_on_cran()
  skip_if_no_define_levels()
  local_home()
  s <- suppressMessages(create_study("LV1", planner = add_output(new_planner(), "T1", description = "a table")))
  adam <- file.path(s$path, study_layout()[["adam"]])
  dir.create(adam, recursive = TRUE, showWarnings = FALSE)
  file.copy(example_define(), file.path(adam, "define.xml"))
  p <- s$planner
  # a broken file: its message, the planner as it was
  writeLines("not xml <", file.path(adam, "broken.xml"))
  expect_error(.adam_levels_apply(p, "data/adam/broken.xml", s$path), "could not be read")
  # the define.xml
  now <- as.POSIXct("2026-11-22 10:15:00")
  p <- .adam_levels_apply(s$planner, file.path(adam, "define.xml"), s$path, now = now)
  expect_identical(unname(p$study[c("adam_levels", "adam_levels_file", "adam_levels_applied")]),
                   c("define_xml", "data/adam/define.xml", "2026-11-22T10:15:00"))
  expect_true("CL.SEX" %in% p$spec$study$adam_meta$codelists$codelist)
  s$planner <- p
  s <- suppressMessages(save_study(s))
  y <- yaml::read_yaml(file.path(s$path, "study.yml"))$settings
  expect_identical(y$adam_levels, "define_xml")
  expect_identical(y$adam_levels_file, "data/adam/define.xml")
  expect_true(file.exists(file.path(s$path, "spec", "adam_meta", "define_xml.yml")))
  d <- yaml::read_yaml(file.path(s$path, "spec", "defaults.yml"))
  expect_true("CL.SEX" %in% vapply(d$adam_meta$codelists, `[[`, "", "codelist"))
  s2 <- suppressMessages(open_study("LV1"))
  expect_identical(.study_adam_levels(s2$planner), "define_xml")
  expect_false(.adam_levels_pending(s2$planner, "data/adam/define.xml", s2$path))
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
  s2 <- suppressMessages(save_study(s2))
  expect_true(file.exists(file.path(s2$path, "spec", "adam_meta", "define_spec.yml")))
  # cleared: none; adam_meta: gone from defaults.yml, the kept files stay
  s2$planner <- .adam_levels_clear(s2$planner)
  s2 <- suppressMessages(save_study(s2))
  expect_false(any(grepl("adam_levels", readLines(file.path(s2$path, "study.yml")))))
  expect_null(yaml::read_yaml(file.path(s2$path, "spec", "defaults.yml"))$adam_meta)
  expect_true(all(file.exists(file.path(s2$path, "spec", "adam_meta",
                                        c("define_xml.yml", "define_spec.yml")))))
  expect_identical(.study_adam_levels(suppressMessages(open_study("LV1"))$planner), "none")
})

test_that("the screen: [Data] has ADaM levels, [Study] no longer; an old codelists is dropped on open", {
  skip_on_cran()
  skip_if_quick()
  skip_if_no_define_levels()
  html <- as.character(app_ui("ja"))
  expect_match(html, "adam_levels_path", fixed = TRUE)
  expect_match(html, "ADaM \u306e\u6c34\u6e96", fixed = TRUE)
  local_home()
  s <- suppressMessages(create_study("LV2", planner = add_output(new_planner(), "T1", description = "a table")))
  # a study saved before #742: settings: adam_levels: codelists
  f <- file.path(s$path, "study.yml")
  y <- readLines(f, encoding = "UTF-8")
  at <- which(y == "settings:")
  y <- if (length(at)) append(y, "  adam_levels: codelists", after = at[1L]) else
    c(y, "settings:", "  adam_levels: codelists")
  writeLines(y, f, useBytes = TRUE)
  s <- suppressMessages(reload_from_spec("LV2"))
  expect_identical(.study_setting(s$planner, "adam_levels"), "codelists")
  suppressWarnings(shiny::testServer(server_for("LV2"), {
    rv <- session$userData$rv
    expect_identical(.study_setting(rv$p, "adam_levels"), NA_character_)
    expect_true(session$userData$dirty())
    expect_false(grepl("study_adam_levels", output$study_detail$html, fixed = TRUE))
    expect_match(output$adam_levels_status$html, "none", fixed = TRUE)
  }))
})
