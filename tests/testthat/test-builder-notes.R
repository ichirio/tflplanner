# What the table builder says should fit the table (#324): no "check this
# order" when the report's code list orders the column variable, and no
# continuous rows -- nor a warning about their statistics -- in a table
# with no continuous variable.

builder_study <- function(id, ard, codelists = NULL, env = parent.frame()) {
  p <- add_standard_defaults(add_output(new_planner(), id, type = "table"), "BN")
  if (!is.null(codelists)) p <- set_sheet_rows(p, "codelists", id, codelists)
  s <- create_study("BN", planner = p)
  data <- rtfreporter::normalize_ard(ard)
  m <- ard_meta(ard, data)
  f <- .meta_file(s, id)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(m, f)
  saveRDS(data, sub("[.]rds$", "_data.rds", f))
  s
}

open_builder <- function(session, id) {
  session$setInputs(target = id, nav = "make", step = "content", content_nav = "content",
                    table_nav = "builder")
  # the form's fields as the browser sends them once drawn
  bf <- session$userData$bform
  b <- function(x) paste0("b", bf$n, "_", x)
  v <- list()
  v[[b("key")]] <- "TRT01A"
  v[[b("vars")]] <- bf$st$variables$variable
  v[[b("value")]] <- "stat"
  v[[b("cat")]] <- bf$st$cat_format
  do.call(session$setInputs, v)
  session$elapse(1000)
}

test_that("a categorical-only table: no continuous rows, no warning about them", {
  skip_on_cran()
  skip_if_not_installed("cards")
  local_home()
  ard <- cards::ard_stack(cards::ADSL, .by = TRT01A,
                          cards::ard_categorical(variables = c(SEX, RACE)))
  builder_study("T-CAT", ard)
  shiny::testServer(server_for("BN"), {
    open_builder(session, "T-CAT")
    session$flushReact()
    h <- output$builder_form$html
    expect_match(h, "Categorical variables", fixed = TRUE)
    expect_false(grepl("Continuous variables: the rows", h, fixed = TRUE))
    expect_false(grepl("A row of your own", h, fixed = TRUE))
    b <- function(x) paste0("b", session$userData$bform$n, "_", x)
    w <- output[[b("warn")]]$html %||% ""
    expect_false(grepl("mean", w, fixed = TRUE))
    # what the definition says of the rows is kept as it was
    st0 <- session$userData$bform$st
    expect_identical(builder_read(session$userData$rv$p, "T-CAT")$rows, st0$rows)
  })
})

test_that("a table with a continuous variable still has its rows", {
  skip_on_cran()
  skip_if_not_installed("cards")
  local_home()
  ard <- cards::ard_stack(cards::ADSL, .by = TRT01A,
                          cards::ard_continuous(variables = AGE),
                          cards::ard_categorical(variables = SEX))
  builder_study("T-MIX", ard)
  shiny::testServer(server_for("BN"), {
    open_builder(session, "T-MIX")
    session$flushReact()
    h <- output$builder_form$html
    expect_match(h, "Continuous variables: the rows", fixed = TRUE)
    expect_match(h, "A row of your own", fixed = TRUE)
  })
})

test_that("the column order: the code list's when the report has one, else 'check'", {
  skip_on_cran()
  skip_if_not_installed("cards")
  local_home()
  ard <- cards::ard_stack(cards::ADSL, .by = TRT01A,
                          cards::ard_categorical(variables = SEX))
  lv <- sort(unique(as.character(cards::ADSL$TRT01A)))
  cl <- data.frame(variable = "TRT01A", value = lv, order = seq_along(lv),
                   stringsAsFactors = FALSE)
  builder_study("T-CL", ard, codelists = cl)
  shiny::testServer(server_for("BN"), {
    open_builder(session, "T-CL")
    session$flushReact()
    b <- function(x) paste0("b", session$userData$bform$n, "_", x)
    a <- output[[b("arms_ui")]]$html
    expect_false(grepl("Check this order", a, fixed = TRUE))
    expect_match(a, "code list", fixed = TRUE)
  })
  # without one: the order is the ARD's, and the form says to check it
  local_home()
  builder_study("T-NOCL", ard)
  shiny::testServer(server_for("BN"), {
    open_builder(session, "T-NOCL")
    session$flushReact()
    b <- function(x) paste0("b", session$userData$bform$n, "_", x)
    expect_match(output[[b("arms_ui")]]$html, "Check this order", fixed = TRUE)
  })
})

test_that("the builder's form: the Layers bar, the groups, the pages as fields, an example as placeholder", {
  skip_on_cran()
  skip_if_not_installed("cards")
  local_home()
  ard <- cards::ard_stack(cards::ADSL, .by = TRT01A,
                          cards::ard_continuous(variables = AGE),
                          cards::ard_categorical(variables = SEX))
  builder_study("T-PG", ard)
  shiny::testServer(server_for("BN"), {
    open_builder(session, "T-PG")
    session$flushReact()
    h <- output$builder_form$html
    expect_match(h, "rp-cards-bar", fixed = TRUE)
    expect_match(h, ">Cards (plan)<", fixed = TRUE)
    expect_match(h, "rp-card-sep", fixed = TRUE)
    expect_match(h, 'class="rp-card-req">Required<', fixed = TRUE)
    # the own row's fields start empty: the example is the placeholder, its
    # explanation behind the (i) of the template
    expect_match(h, 'placeholder="e.g. Mean +/- SD"', fixed = TRUE)
    expect_false(grepl('<p class="text-muted mb-0">{statistic}', h, fixed = TRUE))
    bf <- session$userData$bform
    fns <- vapply(bf$items, function(it) it$fn, "")
    skip_if_not("plan_paginate_rows" %in% fns)
    # a study card the table has not: off; its switch puts it on (its
    # usual values written), its fields shown
    if (!isTRUE(bf$items[[match("plan_paginate_rows", fns)]]$present)) {
      on <- list(TRUE)
      names(on) <- paste0("b", bf$n, "_card_plan_paginate_rows__on")
      do.call(session$setInputs, on)
      session$elapse(1000)
      session$flushReact()
      h <- output$builder_form$html
      bf <- session$userData$bform
      fns <- vapply(bf$items, function(it) it$fn, "")
      expect_true(bf$items[[match("plan_paginate_rows", fns)]]$present)
    }
    cid <- paste0("b", bf$n, "_card_plan_paginate_rows")
    # the pages' arguments as fields: a number, a choice of the schema's
    expect_match(h, sprintf('id="%s__max_rows"', cid), fixed = TRUE)
    expect_match(h, 'type="number"', fixed = TRUE)
    expect_match(h, sprintf('id="%s__split"', cid), fixed = TRUE)
    expect_match(h, '<option value="group_safe"', fixed = TRUE)
    expect_match(h, '<option value="group_force"', fixed = TRUE)
    # an edit lands in the layout sheet
    it <- bf$items[[match("plan_paginate_rows", fns)]]
    v <- list(25L, it$args$split %||% "group_safe")
    names(v) <- paste0(cid, c("__max_rows", "__split"))
    do.call(session$setInputs, v)
    session$elapse(1500)
    expect_identical(sheet_rows(session$userData$rv$p, "layout", "T-PG")$pages_max_rows, "25")
  })
})
