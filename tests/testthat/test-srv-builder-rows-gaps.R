# The branches of R/srv_builder_rows.R the other tests leave out: a row of
# one's own, a variable's own decimals, back to the ARD's order, plan_cells()
# by variable edited on its card, the Total column's switch, and the
# variables' grid when its edit does not fit.

rr_home <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "ws")))
  home
}

# a study with one table and its ARD read (as the builder sees it)
rr_study <- function(id, ard) {
  p <- add_standard_defaults(add_output(new_planner(), id, type = "table"), "RR")
  s <- create_study("RR", planner = p)
  data <- rtfreporter::normalize_ard(ard)
  m <- ard_meta(ard, data)
  f <- .meta_file(s, id)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(m, f)
  saveRDS(data, sub("[.]rds$", "_data.rds", f))
  s
}

rr_ard <- function() {
  cards::ard_stack(cards::ADSL, .by = TRT01A,
                   cards::ard_continuous(variables = c(AGE, BMIBL)),
                   cards::ard_categorical(variables = SEX), .overall = TRUE)
}

# what the browser sends for the form drawn now (the form drawn again has new ids)
rr_inputs <- function(session) {
  # (the form is drawn in the flush: its ids are known after it)
  session$elapse(1000)
  session$flushReact()
  bf <- session$userData$bform
  b <- function(x) paste0("b", bf$n, "_", x)
  v <- list()
  v[[b("key")]] <- "TRT01A"
  v[[b("vars")]] <- bf$st$variables$variable
  v[[b("value")]] <- "stat"
  v[[b("cat")]] <- bf$st$cat_format
  do.call(session$setInputs, v)
  session$elapse(1000)
  session$flushReact()
}

# the table's builder opened on its first form
rr_open <- function(session, id) {
  session$setInputs(target = id, nav = "make", step = "content", content_nav = "content",
                    table_nav = "builder")
  rr_inputs(session)
}

# an input of the form drawn now
rr_set <- function(session, name, value) {
  v <- list(value)
  names(v) <- paste0("b", session$userData$bform$n, "_", name)
  do.call(session$setInputs, v)
  session$elapse(1000)
}

rr_notes <- function(env = parent.frame()) {
  got <- new.env()
  got$n <- list()
  testthat::local_mocked_bindings(
    showNotification = function(ui, ..., type = "message") {
      got$n[[length(got$n) + 1L]] <- list(text = paste(as.character(ui), collapse = ""), type = type)
      invisible("id")
    }, .package = "shiny", .env = env)
  function() got$n
}

# ---- the Rows card, the Total column ---------------------------------------------

test_that("the Rows card: a row of one's own, a variable's own decimals; the Total column's switch", {
  skip_on_cran()
  skip_if_not_installed("cards")
  rr_home()
  rr_study("T-RR", rr_ard())
  shiny::testServer(server_for("RR"), {
    notes <- rr_notes()
    rv <- session$userData$rv
    rr_open(session, "T-RR")
    # ---- a row of one's own
    n0 <- length(notes())
    rr_set(session, "own_label", "Mean +/- SD")
    rr_set(session, "own_tpl", "no statistic here")
    rr_set(session, "own_add", 1)
    expect_identical(notes()[[length(notes())]]$text,
                     "A row to add needs a label and a template with a {statistic}.")
    expect_gt(length(notes()), n0)
    rr_set(session, "own_label", "Mean +/- SD")
    rr_set(session, "own_tpl", "{mean} +/- {sd}")
    rr_set(session, "own_add", 2)
    st <- session$userData$bform$st
    expect_true("Mean +/- SD" %in% st$rows)
    expect_identical(st$templates[["Mean +/- SD"]], "{mean:.1f} +/- {sd:.2f}")
    pc <- .builder_args(builder_items(rv$p, "T-RR"), "plan_cells")
    expect_identical(pc$continuous[["Mean +/- SD"]], "{mean:.1f} +/- {sd:.2f}")
    # ---- a variable's own decimals (the form is drawn again after a write)
    rr_inputs(session)
    bf <- function() session$userData$bform
    exc_html <- function() output[[paste0("b", bf()$n, "_exc_ui")]]$html
    expect_match(exc_html(), "A variable of its own (0)", fixed = TRUE)
    rr_set(session, "exc_var", "AGE")
    rr_set(session, "exc_stat", "mean")
    rr_set(session, "exc_dg", 3)
    rr_set(session, "exc_add", 1)
    expect_identical(bf()$exc$digits, 3L)
    expect_match(exc_html(), "AGE: mean 3", fixed = TRUE)
    # the same variable and statistic again: one line, the new number
    rr_set(session, "exc_dg", 4)
    rr_set(session, "exc_add", 2)
    expect_identical(nrow(bf()$exc), 1L)
    expect_identical(bf()$exc$digits, 4L)
    rr_set(session, "exc_var", "BMIBL")
    rr_set(session, "exc_stat", "sd")
    rr_set(session, "exc_dg", 1)
    rr_set(session, "exc_add", 3)
    expect_identical(bf()$exc$variable, c("AGE", "BMIBL"))
    # a number the decimals cannot be: nothing added
    rr_set(session, "exc_dg", -1)
    rr_set(session, "exc_add", 4)
    expect_identical(nrow(bf()$exc), 2L)
    # taken away: the line asked for; a line that is not there is left
    rr_set(session, "exc_rm", 9)
    expect_identical(nrow(bf()$exc), 2L)
    rr_set(session, "exc_rm", 1)
    expect_identical(bf()$exc$variable, "BMIBL")
    expect_match(exc_html(), "BMIBL: sd 1", fixed = TRUE)
    expect_no_match(exc_html(), "AGE: mean", fixed = TRUE)
    # ---- the Total column
    tb <- function() sheet_rows(rv$p, "tables", "T-RR")
    expect_identical(tb()$total, "Total")
    html <- function() output[[paste0("b", session$userData$bform$n, "_total_ui")]]$html
    # (no analysis of the ARD in the planner is split by the column key)
    expect_match(html(), "No analysis of this report is grouped by TRT01A", fixed = TRUE)
    rr_set(session, "card_plan_total__on", TRUE)
    rr_set(session, "total_label", "All subjects")
    rr_set(session, "total_pos", "first")
    expect_identical(tb()$total, "All subjects")
    expect_identical(tb()$total_position, "first")
    # a blank heading: the one it had
    rr_set(session, "total_label", "  ")
    expect_identical(tb()$total, "All subjects")
    on <- "card_plan_total__on"
    rr_set(session, on, FALSE)
    expect_true(is.na(tb()$total))
    rr_set(session, on, TRUE)
    expect_false(is.na(tb()$total))
  })
})

# ---- the order, the grid ----------------------------------------------------------

test_that("back to the ARD's order, and the variables' grid when its edit does not fit", {
  skip_on_cran()
  skip_if_not_installed("cards")
  rr_home()
  rr_study("T-RR", rr_ard())
  shiny::testServer(server_for("RR"), {
    notes <- rr_notes()
    rv <- session$userData$rv
    rr_open(session, "T-RR")
    bf <- function() session$userData$bform
    ard <- bf()$st$variable_ard
    expect_identical(ard, c("AGE", "BMIBL", "SEX"))
    # the table's own order: the other way round
    st <- bf()$st
    st$variables <- st$variables[rev(seq_len(nrow(st$variables))), , drop = FALSE]
    rownames(st$variables) <- NULL
    rv$p <- builder_write(rv$p, "T-RR", st, was = bf()$last)
    rv$bver <- rv$bver + 1L
    rr_inputs(session)
    expect_identical(bf()$st$variables$variable, rev(ard))
    session$setInputs(b_lv_reset = list(variable = "variable", n = 1))
    rr_inputs(session)
    expect_identical(bf()$st$variables$variable, ard)
    # TRT01A keeps no order in this ARD: nothing to go back to
    before <- rv$p
    session$setInputs(b_lv_reset = list(variable = "TRT01A", n = 2))
    session$elapse(1000)
    expect_identical(rv$p, before)
    # ---- the variables as a grid (the form is drawn again after a write)
    rr_inputs(session)
    expect_error(output$b_vcl_hot, class = "shiny.silent.error")
    session$setInputs(b_vcl_toggle = 1)
    session$elapse(1000)
    expect_match(output$b_vcl$html, "b_vcl_hot", fixed = TRUE)
    w <- jsonlite::fromJSON(output$b_vcl_hot, simplifyVector = FALSE)$x
    hdr <- unlist(w$colHeaders)
    grid <- function(data, key = w$planner_key, changes = list(list(0L, "label", "", "x"))) {
      list(data = data, changes = list(event = "afterChange", changes = changes),
           params = list(planner_key = key, rClass = "data.frame", rColHeaders = as.list(hdr),
                         rColClasses = w$rColClasses, rDataDim = list(length(data), length(hdr))))
    }
    before <- rv$p
    # nothing changed in it
    session$setInputs(b_vcl_hot = grid(w$data, changes = NULL))
    expect_identical(rv$p, before)
    # drawn for another form (a grid of before)
    data <- w$data
    data[[1L]][[2L]] <- "Age"
    session$setInputs(b_vcl_hot = grid(data, key = "vcl|0"))
    expect_identical(rv$p, before)
    # variables the table does not have
    data[[1L]][[1L]] <- "NOT_A_VARIABLE"
    session$setInputs(b_vcl_hot = grid(data))
    expect_identical(rv$p, before)
    # and the same grid with the right variables: written
    data[[1L]][[1L]] <- w$data[[1L]][[1L]]
    session$setInputs(b_vcl_hot = grid(data))
    session$elapse(1000)
    expect_identical(session$userData$bform$st$variables$label[1L], "Age")
  })
})

# ---- plan_cells() by variable -------------------------------------------------------------

test_that("plan_cells() of a variable edited on its card: own template, conditions added, ordered, taken away", {
  skip_on_cran()
  skip_if_not_installed("cards")
  rr_home()
  rr_study("T-RR", rr_ard())
  shiny::testServer(server_for("RR"), {
    rr_notes()
    rv <- session$userData$rv
    rr_open(session, "T-RR")
    n <- 0L
    ev <- function(...) {
      n <<- n + 1L
      session$setInputs(b_cv_ev = list(..., nonce = n))
      session$elapse(500)
    }
    cur <- function() .builder_args(builder_items(rv$p, "T-RR"), "plan_cells")$AGE
    part <- function() .cells_entry_parse(cur())$rows[["Mean (SD)"]]
    vals <- function() vapply(part()$conds, function(x) x$value, "")
    expect_null(cur())
    # the table's own template of the variable: a copy of the common one
    ev(v = "AGE", act = "mode", value = "own:continuous")
    expect_identical(cur()[["Mean (SD)"]], "{mean:.1f} ({sd:.2f})")
    # a condition added is not written until it has its value
    ev(v = "AGE", act = "cond_add", k = "r:Mean (SD)")
    expect_identical(cur()[["Mean (SD)"]], "{mean:.1f} ({sd:.2f})")
    expect_length(session$userData$bform$cv_edit[["T-RR"]]$AGE$rows[["Mean (SD)"]]$conds, 1L)
    ev(v = "AGE", act = "cond", k = "r:Mean (SD)", i = 1, field = "col", value = "N")
    ev(v = "AGE", act = "cond", k = "r:Mean (SD)", i = 1, field = "op", value = "<")
    ev(v = "AGE", act = "cond", k = "r:Mean (SD)", i = 1, field = "tpl", value = "NE")
    ev(v = "AGE", act = "cond", k = "r:Mean (SD)", i = 1, field = "value", value = "3")
    expect_s3_class(cur(), "tfl_call")
    expect_length(part()$conds, 1L)
    expect_identical(part()$conds[[1L]][c("col", "op", "value", "tpl")],
                     list(col = "N", op = "<", value = "3", tpl = "NE"))
    expect_identical(part()$fallback, "{mean:.1f} ({sd:.2f})")
    # a second condition, moved up; one that is not there, left
    ev(v = "AGE", act = "cond_add", k = "r:Mean (SD)")
    ev(v = "AGE", act = "cond", k = "r:Mean (SD)", i = 2, field = "value", value = "10")
    ev(v = "AGE", act = "cond", k = "r:Mean (SD)", i = 2, field = "tpl", value = "-")
    expect_identical(vals(), c("3", "10"))
    ev(v = "AGE", act = "cond_up", k = "r:Mean (SD)", i = 2)
    expect_identical(vals(), c("10", "3"))
    ev(v = "AGE", act = "cond_down", k = "r:Mean (SD)", i = 2)
    expect_identical(vals(), c("10", "3"))
    ev(v = "AGE", act = "cond_up", k = "r:Mean (SD)", i = 1)
    ev(v = "AGE", act = "cond_rm", k = "r:Mean (SD)", i = 7)
    ev(v = "AGE", act = "no-such-act", k = "r:Mean (SD)")
    expect_identical(vals(), c("10", "3"))
    ev(v = "AGE", act = "cond_rm", k = "r:Mean (SD)", i = 1)
    expect_identical(vals(), "3")
    # the common template again: the variable's own goes
    ev(v = "AGE", act = "mode", value = "common")
    expect_null(cur())
  })
})
