# The template cards' table fields (gui-checks-1, table screens): a field's
# column choices are its data row's dataset's, the card is not drawn again
# while a table is typed in, "(none)" on picks and tables, the report's data
# rows as report_data, and a !series default shown light as candidates.

skip_on_cran()

tff_has_series <- function() {
  "tfl_arg_defs" %in% getNamespaceExports("tflspec") &&
    tryCatch(is.data.frame(tflspec::tfl_arg_defs(template = "tflspec_v1/demog")), error = function(e) FALSE)
}

# a study with tflspec's fictional ADaM data and a series template report
tff_study <- function(home, id, args, template = "tflspec_v1/demog") {
  skip_if_not_installed("cards")
  skip_if_not("tfl_ai_study_init" %in% getNamespaceExports("tflspec"))
  skip_if_not(tff_has_series(), "this tflspec has no series templates")
  d <- file.path(studies_root(home), id)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  for (x in c("adsl", "adae", "adtte")) {
    saveRDS(getExportedValue("tflspec", x), file.path(d, "adam", paste0(x, ".rds")))
  }
  invisible(tflspec::tfl_ai_study_init(d, adam = "adam", study_id = id))
  s <- suppressMessages(open_study(d, home = home))
  o <- tflspec::tfl_output("T-TF-1", "table", template = list(name = template, args = args))
  s$planner <- suppressWarnings(.spec_add_output(s$planner, o))
  suppressMessages(save_study(s))
  id
}

# a table field's state as its input sends it
tff_state <- function(rows, kinds = c("pick", "text")) {
  as.character(jsonlite::toJSON(list(shape = "map", cols = c("key", "value"), kinds = kinds, rows = rows),
                                auto_unbox = TRUE))
}

# the HTML of one field of the card, from its id to the next field's "Add a row"
tff_field <- function(html, fid) {
  at <- regexpr(paste0("id=\"", fid, "\""), html, fixed = TRUE)
  if (at < 0) return("")
  rest <- substring(html, at)
  end <- regexpr("rp-tbl-addrow", rest, fixed = TRUE)
  if (end < 0) rest else substring(rest, 1L, end)
}

test_that("a template's data rows: their ids and datasets, a {param} filled, the report's own after", {
  skip_if_not(tff_has_series(), "this tflspec has no series templates")
  r <- .tpl_data_rows("tflspec_v1/demog")
  expect_identical(r$id, "adsl_sub")
  expect_identical(r$from, "ADSL")
  r <- .tpl_data_rows("tflspec_v1/ae_soc_pt")
  expect_identical(r$id, c("adsl_sub", "adae_sub"))
  expect_identical(r$from, c("ADSL", "ADAE"))
  # (from: "{from}" -- the report's argument, else the param's default, else NA)
  r <- .tpl_data_rows("tflspec_v1/tflspec_hierarchy", args = list(from = "ADAE"))
  expect_identical(r$from[r$id == "rec_sub"], "ADAE")
  r <- .tpl_data_rows("tflspec_v1/tflspec_hierarchy", params = list(from = list(default = "ADCM")))
  expect_identical(r$from[r$id == "rec_sub"], "ADCM")
  expect_true(is.na(.tpl_data_rows("tflspec_v1/tflspec_hierarchy", params = list())$from[2L]))
  # (the report's own rows after the template's; a row of the same id once)
  own <- list(list(id = "adsl_itt", from = "ADSL"), list(id = "adsl_sub", from = "ADSL"))
  expect_identical(.tpl_data_rows("tflspec_v1/demog", own = own)$id, c("adsl_sub", "adsl_itt"))
  # (old data steps, a map: not rows)
  expect_identical(.tpl_data_rows("tflspec_v1/demog", own = list(steps = list()))$id, "adsl_sub")
  # (a template of no series: none)
  expect_identical(nrow(.tpl_data_rows("demog")), 0L)
  expect_null(.tpl_series_file("no_such/x"))
})

test_that("a table field's columns are its data row's dataset's, the report's data its row ids", {
  data <- list(ADSL = data.frame(USUBJID = 1, TRT01A = "A", AGE = 1),
               ADAE = data.frame(USUBJID = 1, AEDECOD = "x"))
  rows <- data.frame(id = c("adsl_sub", "rec_sub"), from = c("ADSL", "ADAE"), stringsAsFactors = FALSE)
  ctx <- .tpl_table_ctx(data, NULL, rows = rows)
  expect_identical(.choice_values("vars(adsl_sub)", ctx), c(USUBJID = "USUBJID", TRT01A = "TRT01A", AGE = "AGE"))
  expect_identical(unname(.choice_values("vars(rec_sub)", ctx)), c("USUBJID", "AEDECOD"))
  # (report_data: the data rows' ids, not the datasets' names)
  expect_identical(unname(.choice_values("report_data", ctx)), c("adsl_sub", "rec_sub"))
  expect_identical(unname(.choice_values("study_data", ctx)), c("ADSL", "ADAE"))
  # (no rows: the dataset the id begins with, as #717's fix; a dataset's name itself)
  ctx0 <- .tpl_table_ctx(data, NULL)
  expect_identical(unname(.choice_values("vars(adsl_sub)", ctx0)), c("USUBJID", "TRT01A", "AGE"))
  expect_identical(unname(.choice_values("vars(ADAE)", ctx0)), c("USUBJID", "AEDECOD"))
  # (nothing it can name: every column, as before)
  expect_setequal(unname(.choice_values("vars(zz_sub)", ctx0)), c("USUBJID", "TRT01A", "AGE", "AEDECOD"))
  skip_if_not(tff_has_series(), "this tflspec has no series templates")
  prm <- .tpl_params("tflspec_v1/demog")
  ctx <- .tpl_table_ctx(data, NULL, rows = .tpl_data_rows("tflspec_v1/demog"), params = prm)
  spec <- .tpl_table_spec(prm$labels, ctx)
  expect_identical(unname(.choice_vec(spec$choices[[1L]])), c("USUBJID", "TRT01A", "AGE"))
  expect_identical(unname(.choice_values(prm$denominator$sources, ctx)), "adsl_sub")
})

test_that("a !series default's value is the study's copy's, else the source's", {
  skip_if_not(tff_has_series(), "this tflspec has no series templates")
  skip_if_not("tfl_series_values_set" %in% getNamespaceExports("tflspec"))
  prm <- .tpl_params("tflspec_v1/demog")
  expect_identical(prm$formats$series, "formats")
  expect_null(prm$labels$series)
  sv <- .tpl_series_value(list(study = list()), "tflspec_v1/demog", prm$formats)
  expect_identical(sv, tflspec::tfl_series_read("tflspec_v1")$values$formats)
  # a study with no copy of the series: the source's
  dir <- withr::local_tempdir()
  expect_identical(.tpl_series_value(list(study = list(), dir = dir), "tflspec_v1/demog", prm$formats), sv)
  # its copy, changed: the copy's
  invisible(tflspec::tfl_copy_templates(dir, "tflspec_v1/demog"))
  tflspec::tfl_series_values_set(dir, "tflspec_v1", list(formats = list(mean = 3L)))
  own <- list(study = list(), dir = dir)
  expect_identical(.tpl_series_value(own, "tflspec_v1/demog", prm$formats), list(mean = 3L))
  # (no series default, no series template: none)
  expect_null(.tpl_series_value(list(), "tflspec_v1/demog", prm$labels))
  expect_null(.tpl_series_value(list(), "demog", prm$formats))
})

test_that("a map's candidates are light rows: only the values typed over are read back", {
  spec <- .tbl_spec("map<string,number|string>", def = list(free = TRUE))
  cand <- list(N = 0, mean = 2, sd = 3)
  html <- as.character(.field_table("f", spec, NULL, candidates = cand))
  expect_match(html, "rp-tbl-cand", fixed = TRUE)
  expect_match(html, "placeholder=\"2\"", fixed = TRUE)
  expect_match(html, "value=\"mean\"", fixed = TRUE)
  # (the value's entry in its candidate's place, the candidate behind it; its own row after)
  b <- .tbl_cand_rows(spec, .tbl_rows(spec, list(xx = 4, mean = 1)), cand)
  expect_identical(lapply(b, `[[`, "cells"), list(c("N", ""), c("mean", "1"), c("sd", ""), c("xx", "4")))
  expect_identical(vapply(b, `[[`, NA, "cand"), c(TRUE, FALSE, TRUE, FALSE))
  expect_identical(b[[2L]]$ph, c("", "2"))
  expect_null(b[[4L]]$ph)
  # (what the field sends: the blank candidates are no entries)
  st <- list(shape = "map", cols = c("key", "value"), kinds = spec$kinds,
             rows = lapply(b, `[[`, "cells"))
  expect_identical(.tbl_read(st), list(mean = 1, xx = 4))
  st$rows <- list(c("N", ""), c("mean", ""), c("sd", ""))
  expect_null(.tbl_read(st))
  # (no candidates, or a table that is no map: the value's rows alone)
  expect_identical(length(.tbl_cand_rows(spec, list(c("a", "1")))), 1L)
  rs <- .tbl_spec("rows<a, b>")
  expect_identical(length(.tbl_cand_rows(rs, list(), cand)), 0L)
})

test_that("pick, picks and table fields offer \"(none)\"; a table drawn for it is no edit", {
  pick <- shiny::selectizeInput("p", "P", c("", "adsl_sub"))
  html <- as.character(.tpl_none_wrap(pick, "p", "pick", list(name = "p"), FALSE))
  expect_match(html, "value=\"(none)\"", fixed = TRUE)
  picks <- shiny::selectizeInput("q", "Q", c("a", "b"), multiple = TRUE)
  expect_match(as.character(.tpl_none_wrap(picks, "q", "picks", list(name = "q"), TRUE)),
               "value=\"(none)\" selected", fixed = TRUE)
  tbl <- shiny::div(class = "form-group shiny-input-container", shiny::div(class = "rp-tbl", id = "tb"))
  html <- as.character(.tpl_none_wrap(tbl, "tb", "table", list(name = "tb"), TRUE))
  expect_match(html, "tplNone(&#39;tb&#39;, this)", fixed = TRUE)
  expect_match(html, "tpl-tbl-field", fixed = TRUE)
  expect_match(html, "none-note none-on", fixed = TRUE)
  # (not on a required one)
  expect_identical(as.character(.tpl_none_wrap(tbl, "tb", "table", list(required = TRUE), FALSE)),
                   as.character(tbl))
  expect_true(.tpl_table_none_drawn("table", NULL, tflspec::tfl_unset()))
  expect_false(.tpl_table_none_drawn("table", list(a = 1), tflspec::tfl_unset()))
  expect_false(.tpl_table_none_drawn("table", NULL, list(a = 1)))
  expect_true(.is_unset(.tpl_value("(none)", "table", list(name = "x"))))
  expect_true(.is_unset(.tpl_value("(none)", "pick", list(name = "x"))))
})

test_that("a demog card: its columns ADSL's, its denominator a data row, formats light, (none), typing not cut", {
  home <- local_home()
  id <- tff_study(home, "GTF1", list(by = "TRT01A", continuous = "AGE"))
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    session$setInputs(target = "T-TF-1", nav = "make", step = "content", content_nav = "content")
    card <- function() paste(as.character(output$tpl_card$html), collapse = "")
    html <- card()
    # (1) the labels' columns: ADSL's only
    lab <- tff_field(html, "tpl_tflspec_v1_demog__labels")
    expect_match(lab, "value=\"AGE\"", fixed = TRUE)
    expect_false(grepl("AEDECOD", lab, fixed = TRUE))
    expect_false(grepl("PARAMCD", lab, fixed = TRUE))
    # (4) the denominator: the data row adsl_sub, not a dataset's name
    expect_match(html, "<option value=\"adsl_sub\"", fixed = TRUE)
    # (8) formats: the series' values light
    fm <- tff_field(html, "tpl_tflspec_v1_demog__formats")
    expect_match(fm, "rp-tbl-cand", fixed = TRUE)
    expect_match(fm, "placeholder=\"2\"", fixed = TRUE)
    # (3) "(none)" on the table and on the pick
    expect_match(html, "tplNone(&#39;tpl_tflspec_v1_demog__labels&#39;, this)", fixed = TRUE)

    # (2) the first value typed: written, the card not drawn again until the table is left
    session$setInputs(`tpl_tflspec_v1_demog__labels` = tff_state(list(c("AGE", "Age (y"))))
    expect_identical(.tpl_of(rv$p, "T-TF-1")$args$labels, list(AGE = "Age (y"))
    expect_false(grepl("Age (y", card(), fixed = TRUE))
    session$setInputs(`tpl_tflspec_v1_demog__labels` = tff_state(list(c("AGE", "Age (years)"))))
    expect_identical(.tpl_of(rv$p, "T-TF-1")$args$labels, list(AGE = "Age (years)"))
    expect_false(grepl("Age (years)", card(), fixed = TRUE))
    session$setInputs(tpl_tbl_left = "tpl_tflspec_v1_demog__labels")
    expect_match(card(), "value=\"Age (years)\"", fixed = TRUE)

    # (8) an entry typed over is the report's; the others stay the series'; cleared, it goes back
    fstate <- function(mean) tff_state(list(c("N", ""), c("mean", mean), c("sd", "")), c("text", "number_text"))
    session$setInputs(`tpl_tflspec_v1_demog__formats` = fstate("1"))
    expect_identical(.tpl_of(rv$p, "T-TF-1")$args$formats, list(mean = 1))
    session$setInputs(`tpl_tflspec_v1_demog__formats` = fstate(""))
    expect_false("formats" %in% names(.tpl_of(rv$p, "T-TF-1")$args))

    # (3) "(none)" on a table: !unset; the empty table it is drawn as is no edit
    session$setInputs(`tpl_tflspec_v1_demog__labels__none` = TRUE)
    expect_true(.is_unset(.tpl_of(rv$p, "T-TF-1")$args$labels))
    session$setInputs(`tpl_tflspec_v1_demog__labels` = tff_state(list()))
    expect_true(.is_unset(.tpl_of(rv$p, "T-TF-1")$args$labels))
    # (and on the denominator's pick)
    session$setInputs(`tpl_tflspec_v1_demog__denominator` = "(none)")
    expect_true(.is_unset(.tpl_of(rv$p, "T-TF-1")$args$denominator))
  })
})
