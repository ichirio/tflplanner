# A template report's card draws an argument the report does not give with
# the study standard's value (or the template's default).  That value coming
# back from the page is no edit: it is never written into the report's
# args, where it would stop following the standard (#751 item 2).  The card
# and the check of an edit read the same value (.tpl_drawn_value()).

test_that(".tpl_drawn_value(): the report's value, else the study's as tflspec filled it, else the default", {
  prm <- list(name = "count_cell", default = NULL, study = NULL)
  p <- list(spec = list(study = list(), outputs = list(R1 = list(id = "R1"))))
  testthat::local_mocked_bindings(.tpl_filled = function(p, output_id) {
    list(study = "count_cell", args = list(count_cell = "{n} ({p}%)"))
  })
  expect_identical(.tpl_drawn_value(p, "R1", list(), prm), "{n} ({p}%)")
  expect_identical(.tpl_drawn_value(p, "R1", list(count_cell = "{n}"), prm), "{n}")
  testthat::local_mocked_bindings(.tpl_filled = function(p, output_id) NULL)
  expect_null(.tpl_drawn_value(p, "R1", list(), prm))
  expect_identical(.tpl_drawn_value(p, "R1", list(), utils::modifyList(prm, list(default = "{n}"))), "{n}")
})

test_that("a new demog report: the study standard's count_cell and col_header the card drew come back, and are not written into args", {
  skip_if_quick()
  home <- local_home()
  suppressMessages(create_sample_study(run = FALSE, home = home, study_id = "TSTD"))
  shiny::testServer(server_for("TSTD"), {
    rv <- session$userData$rv
    nm <- "demog"
    session$setInputs(add = 1, modal_type = "table", modal_how = "template", modal_id = "SD-1",
                      modal_desc = "", modal_tpl_table = nm)
    ins <- list("TRT01A", "SAF", "AGE")
    names(ins) <- paste0("mtpl_", .tpl_id_part(nm), "__", c("by", "population", "continuous"))
    do.call(session$setInputs, ins)
    session$setInputs(add_ok = 1)
    id <- "T-SD-1"
    args0 <- .tpl_of(rv$p, id)$args
    expect_setequal(names(args0), c("by", "continuous"))
    # (the study standard fills them: tflspec says so)
    expect_true(all(c("count_cell", "col_header") %in% .tpl_filled(rv$p, id)$study))
    session$setInputs(target = id, nav = "make", step = "content")
    session$flushReact()
    h <- paste(as.character(output$tpl_card$html), collapse = "")
    # each field's value as drawn comes back from the page: no edit
    for (a in c("count_cell", "col_header")) {
      iid <- paste0("tpl_", .tpl_id_part(nm), "__", a)
      m <- regmatches(h, regexpr(sprintf('id="%s"[^>]*value="[^"]*"', iid), h))
      expect_length(m, 1L)
      v <- xml2::xml_text(xml2::read_html(paste0("<p>", sub('.*value="([^"]*)"$', "\\1", m), "</p>")))
      expect_true(nzchar(v))
      do.call(session$setInputs, stats::setNames(list(v), iid))
    }
    expect_identical(.tpl_of(rv$p, id)$args, args0)
    # an edit: written
    iid <- paste0("tpl_", .tpl_id_part(nm), "__count_cell")
    do.call(session$setInputs, stats::setNames(list("{n}"), iid))
    expect_identical(.tpl_of(rv$p, id)$args$count_cell, "{n}")
  })
})
