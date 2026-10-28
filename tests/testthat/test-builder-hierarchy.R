# The builder with a hierarchy table (SOC / PT): read and written back, it
# changes nothing; its order (tables$sort) is the one thing of its own it
# writes; a summary table's rows are never written into it

hier_meta <- function(arms = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")) {
  list(keys = list(TRT01A = arms, AEBODSYS = "CARDIAC DISORDERS", AEDECOD = "SINUS BRADYCARDIA"),
       by = "TRT01A", hierarchy = c("AEBODSYS", "AEDECOD"),
       variables = data.frame(variable = character(), kind = character(), levels = character(),
                              n_levels = integer(), stats = character(), label = character()),
       stats = c("n", "N", "p"))
}
hier_planner <- function() {
  adae <- data.frame(USUBJID = c("1", "2"), TRT01A = c("Placebo", "Placebo"),
                     AEBODSYS = "CARDIAC DISORDERS", AEDECOD = "SINUS BRADYCARDIA",
                     TRTEMFL = "Y")
  adsl <- data.frame(USUBJID = c("1", "2", "3"), SAFFL = "Y",
                     TRT01A = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
                     TRT01AN = c(0, 54, 81))
  first_hierarchy_table(new_planner(), "T-AE", "data/adam/adae.rds", adae,
                        "data/adam/adsl.rds", adsl, population = "SAFFL",
                        group = "TRT01A", where = "TRTEMFL == \"Y\"")
}

test_that("a hierarchy table reads its terms and its order", {
  p <- hier_planner()
  st <- builder_read(p, "T-AE", hier_meta())
  expect_identical(st$hierarchy, c("AEBODSYS", "AEDECOD"))
  expect_identical(as.vector(st$sort), c("alpha", "freq"))
  expect_identical(attr(st$sort, "col"), NA_character_)
  # its any-event row's name: normalize_ard()'s card, not the form's (#417)
  expect_null(st$overall_label)
  expect_identical(Filter(function(it) identical(it$fn, "normalize_ard"),
                          builder_items(p, "T-AE", hier_meta()))[[1L]]$args$overall, "Any TEAE")
  expect_identical(nrow(st$variables), 0L)
  expect_identical(st$key, "TRT01A")
  # an order of the sheets' own is "own", and kept
  q <- set_sheet_rows(p, "tables", "T-AE", transform(
    sheet_rows(p, "tables", "T-AE")[, -1L], sort = "group1 | -n"))
  expect_identical(builder_read(q, "T-AE", hier_meta())$sort, "own")
  # a summary table has neither
  expect_null(builder_read(new_planner(), "X", NULL)$sort)
})

test_that("written back unchanged, nothing changes; the order is all it writes of its own", {
  skip_on_cran()
  p <- hier_planner()
  st <- builder_read(p, "T-AE", hier_meta())
  expect_identical(builder_write(p, "T-AE", st, was = st), p)
  alpha <- st
  alpha$sort <- structure(c("alpha", "alpha"), col = NA_character_)
  q <- builder_write(p, "T-AE", alpha, was = st)
  expect_identical(sheet_rows(q, "tables", "T-AE")$sort,
                   ".overall | group1 | .depth | label")
  # nothing of a summary table: its rows, its variables, its cells
  for (sh in c("variables", "cells", "col_header", "layout")) {
    expect_identical(sheet_rows(q, sh, "T-AE"), sheet_rows(p, sh, "T-AE"), info = sh)
  }
  tb <- sheet_rows(q, "tables", "T-AE")
  expect_identical(tb$rows, "group1 = AEBODSYS")
  expect_identical(tb$label, "label = AEDECOD")
  # and back
  r <- builder_write(q, "T-AE", st, was = alpha)
  expect_identical(sheet_rows(r, "tables", "T-AE")$sort, sheet_rows(p, "tables", "T-AE")$sort)
  # one order a level: written and read back, three terms too
  expect_identical(.hier_sort(c("alpha", "alpha", "freq")),
                   ".overall | group1 | .depth | group2 | .depth | -n | label")
  for (lv in list(c("alpha", "freq"), c("alpha", "alpha"), c("freq", "freq"),
                  c("alpha", "alpha", "freq"), c("freq", "alpha", "freq"))) {
    for (col in c(NA, "Total", "Placebo")) {
      back <- .hier_sort_read(.hier_sort(lv, col), length(lv))
      expect_identical(as.vector(back), lv)
      if ("freq" %in% lv) expect_identical(attr(back, "col"), col)
    }
  }
  # an outer level by its own count (-n@group1), counted in one column
  expect_identical(.hier_sort(c("freq", "freq"), "Total"),
                   ".overall | -n@group1[Total] | group1 | .depth | -n[Total] | label")
  # two levels counted in two columns: not the card's
  expect_identical(.hier_sort_read(".overall | -n@group1[Total] | group1 | .depth | -n | label", 2L), "own")
  # what the card does not write is the sheets' own
  for (own in c("group1 | -n", ".overall | group1 | label | x", NA)) {
    expect_identical(.hier_sort_read(own, 2L), "own", info = own)
  }
})

test_that("an order by any statistic of the ARD, or a key of the user's", {
  # a row's largest / smallest in any column, another statistic
  s <- .hier_sort(c("-max(n)", "min(p)"), "Total")
  expect_identical(s, ".overall | -max(n)@group1[Total] | group1 | .depth | min(p)[Total] | label")
  back <- .hier_sort_read(s, 2L)
  expect_identical(as.vector(back), c("-max(n)", "min(p)"))
  expect_identical(attr(back, "col"), "Total")
  # a comparison's statistic: its own row, no column of the arms
  s <- .hier_sort(c("-estimate", "-max(n)"), "Total", nocol = "estimate")
  expect_identical(s, ".overall | -estimate@group1 | group1 | .depth | -max(n)[Total] | label")
  back <- .hier_sort_read(s, 2L, nocol = "estimate")
  expect_identical(as.vector(back), c("-estimate", "-max(n)"))
  expect_identical(attr(back, "col"), "Total")
  # a key of the user's: written as it is, and read back as it is
  s <- .hier_sort(c("=-n[Drug X]", "freq"))
  expect_identical(s, ".overall | -n[Drug X] | group1 | .depth | -n | label")
  expect_identical(as.vector(.hier_sort_read(s, 2L)), c("=-n[Drug X]", "freq"))
  expect_identical(as.vector(.hier_sort_read(".overall | -n | group1 | label", 2L)),
                   c("=-n", "alpha"))
  for (s in c(".overall | -max(n)@group1[Placebo] | group1 | .depth | -p[Placebo] | label",
              ".overall | -n@group1 | group1 | .depth | -estimate[A - B] | label",
              ".overall | sum(n)@group1 | group1 | .depth | -n@group1 | label")) {
    expect_identical(.hier_sort(.hier_sort_read(s, 2L, "estimate"),
                                attr(.hier_sort_read(s, 2L, "estimate"), "col"),
                                nocol = "estimate"), s, info = s)
  }
  # the card's choices: by name, then each statistic (n, p first; N not)
  ch <- .sort_key_choices(c("N", "estimate", "p", "n"))
  expect_identical(unname(ch)[1:6], c("alpha", "freq", "n", "-max(n)", "min(n)", "-p"))
  expect_false(any(grepl("(N)", ch, fixed = TRUE)))
  expect_true(all(c("-estimate", "min(estimate)") %in% ch))
  expect_identical(unname(.sort_key_choices("n", hier = FALSE)), "alpha")
})

test_that("a hierarchy counted of a denominator has the subjects per group", {
  p <- hier_planner()
  expect_true(.has_group_n(p, "T-AE", "TRT01A"))
  a <- ard_rows(p, "analyses", "T-AE")
  a$denominator <- NA
  a$output_id <- NULL
  expect_false(.has_group_n(set_ard_rows(p, "analyses", "T-AE", a), "T-AE", "TRT01A"))
})

test_that("the builder opens on a hierarchy table, and writes its order", {
  skip_on_cran()
  local_home()
  s <- create_study("BH", planner = hier_planner())
  f <- .meta_file(s, "T-AE")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(hier_meta(), f)
  shiny::testServer(server_for("BH"), {
    rv <- session$userData$rv
    session$setInputs(target = "T-AE", nav = "make", step = "content", content_nav = "content",
                      table_nav = "builder")
    expect_no_match(output$builder_note$html %||% "", "summary tables for now", fixed = TRUE)
    h <- output$builder_form$html
    expect_match(h, "Row layers", fixed = TRUE)
    expect_match(h, "(rows)", fixed = TRUE)
    # one order a term; an outer term's count waits for rtfreporter
    expect_match(h, "Most frequent first (ties by name)", fixed = TRUE)
    expect_match(h, "Counted in", fixed = TRUE)
    expect_no_match(h, "Rows: variables in order", fixed = TRUE)
    # no warning about the subjects per group: the denominator counts them
    expect_no_match(h, "a header's (N=n) prints NA", fixed = TRUE)
    bf <- session$userData$bform
    b <- function(x) paste0("b", bf$n, "_", x)
    v <- list()
    v[[b("key")]] <- "TRT01A"
    v[[b("value")]] <- "stat"
    v[[b("cat")]] <- bf$st$cat_format
    v[[b("sort1")]] <- "alpha"
    v[[b("sort2")]] <- "freq"
    do.call(session$setInputs, v)
    session$elapse(1000)
    before <- rv$p
    v[[b("sort2")]] <- "alpha"
    do.call(session$setInputs, v)
    session$elapse(1000)
    expect_identical(sheet_rows(rv$p, "tables", "T-AE")$sort, ".overall | group1 | .depth | label")
    expect_identical(sheet_rows(rv$p, "variables", "T-AE"), sheet_rows(before, "variables", "T-AE"))
    # SOC by its own count too, counted in Placebo's column
    v[[b("sort1")]] <- "freq"
    v[[b("sort2")]] <- "freq"
    v[[b("sort_col")]] <- "Placebo"
    do.call(session$setInputs, v)
    session$elapse(1000)
    expect_identical(sheet_rows(rv$p, "tables", "T-AE")$sort,
                     ".overall | -n@group1[Placebo] | group1 | .depth | -n[Placebo] | label")
    # the values plan_levels() names first (#447)
    expect_match(output$builder_form$html, "(levels(group1))", fixed = TRUE)
    v[[b("sort_first1")]] <- TRUE
    do.call(session$setInputs, v)
    session$elapse(1000)
    expect_identical(sheet_rows(rv$p, "tables", "T-AE")$sort,
                     paste(".overall | levels(group1) | -n@group1[Placebo] | group1 | .depth |",
                           "-n[Placebo] | label"))
  })
})

test_that("levels(<key>): plan_levels' values first, written and read back (#447)", {
  # before the level's own key, after .overall (the any-event row first)
  s <- .hier_sort(structure(c("freq", "freq"), first = c(TRUE, FALSE)))
  expect_identical(s, ".overall | levels(group1) | -n@group1 | group1 | .depth | -n | label")
  back <- .hier_sort_read(s, 2L)
  expect_identical(as.vector(back), c("freq", "freq"))
  expect_identical(attr(back, "first"), c(TRUE, FALSE))
  expect_identical(.hier_sort(back), s)
  # by name, the innermost level, a column that counts
  s2 <- .hier_sort(c("alpha", "freq"), "Total", first = c(TRUE, TRUE))
  expect_identical(s2, ".overall | levels(group1) | group1 | .depth | levels(label) | -n[Total] | label")
  b2 <- .hier_sort_read(s2, 2L)
  expect_identical(as.vector(b2), c("alpha", "freq"))
  expect_identical(attr(b2, "first"), c(TRUE, TRUE))
  expect_identical(attr(b2, "col"), "Total")
  # none checked: no attribute, the key as before
  expect_null(attr(.hier_sort_read(.hier_sort(c("alpha", "freq")), 2L), "first"))
  # the builder: an unchanged save writes nothing; checked, the key
  p <- hier_planner()
  st <- builder_read(p, "T-AE", hier_meta())
  expect_identical(builder_write(p, "T-AE", st, was = st), p)
  st2 <- st
  attr(st2$sort, "first") <- c(TRUE, FALSE)
  q <- builder_write(p, "T-AE", st2, was = st)
  expect_identical(sheet_rows(q, "tables", "T-AE")$sort,
                   ".overall | levels(group1) | group1 | .depth | -n | label")
  back <- builder_read(q, "T-AE", hier_meta())$sort
  expect_identical(attr(back, "first"), c(TRUE, FALSE))
  expect_identical(builder_write(q, "T-AE", builder_read(q, "T-AE", hier_meta()),
                                 was = builder_read(q, "T-AE", hier_meta())), q)
})

test_that("an order of the sheets' own is kept, and the card's order is one click away", {
  skip_on_cran()
  local_home()
  p <- hier_planner()
  tb <- sheet_rows(p, "tables", "T-AE")
  tb$output_id <- NULL
  tb$sort <- "group1 | -n"
  s <- create_study("BO", planner = set_sheet_rows(p, "tables", "T-AE", tb))
  f <- .meta_file(s, "T-AE")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(hier_meta(), f)
  shiny::testServer(server_for("BO"), {
    rv <- session$userData$rv
    session$setInputs(target = "T-AE", nav = "make", step = "content", content_nav = "content",
                      table_nav = "builder")
    h <- output$builder_form$html
    expect_match(h, "Order: as the sheets say", fixed = TRUE)
    session$setInputs(b_sort_reset = 1)
    expect_identical(sheet_rows(rv$p, "tables", "T-AE")$sort, ".overall | group1 | .depth | -n | label")
  })
})

test_that("the rows are the table's: the ARD's keys by default, a group outermost, a page each", {
  p <- hier_planner()
  m <- hier_meta()
  m$keys$SEX <- c("F", "M")
  st <- builder_read(p, "T-AE", m)
  # the starter's rows; the keys it may take (not the column's)
  expect_identical(st$row_keys, c("AEBODSYS", "AEDECOD"))
  expect_setequal(st$row_choices, c("AEBODSYS", "AEDECOD", "SEX"))
  expect_false(st$row_pages)
  # a group (an ARD's stratum) outermost: the rows and a new order
  sx <- st
  sx$row_keys <- c("SEX", "AEBODSYS", "AEDECOD")
  q <- builder_write(p, "T-AE", sx, was = st)
  tb <- sheet_rows(q, "tables", "T-AE")
  expect_identical(tb$rows, "group1 = SEX | group2 = AEBODSYS")
  expect_identical(tb$label, "label = AEDECOD")
  # (the group before the any-event row, with no summary row of its own)
  expect_identical(tb$sort, "group1 | .overall | group2 | .depth | -n | label")
  expect_identical(builder_read(q, "T-AE", m)$row_keys, c("SEX", "AEBODSYS", "AEDECOD"))
  expect_identical(as.vector(builder_read(q, "T-AE", m)$sort), c("alpha", "alpha", "freq"))
  # a page each
  pg <- builder_read(q, "T-AE", m)
  pg2 <- pg
  pg2$row_pages <- TRUE
  r <- builder_write(q, "T-AE", pg2, was = pg)
  ly <- sheet_rows(r, "layout", "T-AE")
  expect_identical(c(ly$group_page, ly$group_col), c("TRUE", "group1"))
  expect_true(builder_read(r, "T-AE", m)$row_pages)
  # a hierarchy level left out: nothing written (the card says why)
  drop <- st
  drop$row_keys <- "AEDECOD"
  expect_identical(sheet_rows(builder_write(p, "T-AE", drop, was = st), "tables", "T-AE"),
                   sheet_rows(p, "tables", "T-AE"))
  # the rows read back: constants and .label are not keys
  expect_identical(.row_keys_read('PARAM = PARAM | group1 = "Worst"', "label = .label"), "PARAM")
  expect_identical(.row_keys_read(NA, "label = AEDECOD"), "AEDECOD")
})

test_that("an ARD read without its hierarchy takes the analysis's: the levels, not variables", {
  p <- hier_planner()
  expect_identical(.analysis_hierarchy(p, "T-AE"), c("AEBODSYS", "AEDECOD"))
  # as an ARD normalized without its terms reads: the outer term a column
  # key, the inner one an analysis variable
  stale <- hier_meta()
  stale$keys$AEDECOD <- NULL
  stale$by <- c("TRT01A", "AEBODSYS")
  stale$hierarchy <- character()
  stale$variables <- data.frame(variable = "AEDECOD", kind = "categorical",
                                levels = "SINUS BRADYCARDIA | NAUSEA", n_levels = 2L,
                                stats = "n | N | p", label = NA_character_)
  m <- .meta_with_hierarchy(stale, p, "T-AE")
  expect_identical(m$hierarchy, c("AEBODSYS", "AEDECOD"))
  expect_identical(m$by, "TRT01A")
  expect_identical(m$keys$AEDECOD, c("SINUS BRADYCARDIA", "NAUSEA"))
  expect_identical(nrow(m$variables), 0L)
  # one that has it, or a report with no hierarchical analysis: as it is
  expect_identical(.meta_with_hierarchy(hier_meta(), p, "T-AE"), hier_meta())
  st <- builder_read(p, "T-AE", m)
  expect_identical(st$row_keys, c("AEBODSYS", "AEDECOD"))
  expect_identical(nrow(st$variables), 0L)
})

test_that("the hierarchy's levels are fixed chips, the last the label; normalize_ard() has the overall row", {
  skip_on_cran()
  local_home()
  s <- create_study("BF", planner = hier_planner())
  f <- .meta_file(s, "T-AE")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(hier_meta(), f)
  shiny::testServer(server_for("BF"), {
    rv <- session$userData$rv
    session$setInputs(target = "T-AE", nav = "make", step = "content", content_nav = "content",
                      table_nav = "builder")
    h <- output$builder_form$html
    bf <- session$userData$bform
    lay <- paste0("b", bf$n, "_layers")
    # no x on a level, and the page refuses to remove one
    expect_match(h, sprintf('#%s ~ .selectize-control .item[data-value="AEBODSYS"] .remove', lay),
                 fixed = TRUE)
    expect_match(h, "onDelete", fixed = TRUE)
    expect_match(h, '[\\"AEBODSYS\\",\\"AEDECOD\\"]', fixed = TRUE)
    # AEDECOD is the hierarchy's last level, the rows' text; not a variable
    expect_match(h, "AEDECOD (label)", fixed = TRUE)
    expect_no_match(h, "Analysis variables (variable)", fixed = TRUE)
    expect_match(h, "are the ARD analysis", fixed = TRUE)
    # normalize_ard(): first, its hierarchy read only, its overall row
    expect_match(h, 'data-fn="normalize_ard"', fixed = TRUE)
    expect_lt(regexpr('data-fn="normalize_ard"', h), regexpr('data-fn="table_plan"', h))
    expect_match(h, "(overall)", fixed = TRUE)
    expect_match(h, paste0('id="b', bf$n, '_card_normalize_ard__overall"'), fixed = TRUE)
    # its overall row is written into the report's normalize_ard() (the
    # sheets' tables$overall_label made from it)
    v <- list()
    v[[paste0("b", bf$n, "_key")]] <- "TRT01A"
    v[[paste0("b", bf$n, "_card_normalize_ard__overall")]] <- "Any adverse event"
    do.call(session$setInputs, v)
    session$elapse(1000)
    expect_identical(sheet_rows(rv$p, "tables", "T-AE")$overall_label, "Any adverse event")
  })
})

test_that("opening the builder on an ARD just read writes nothing: the study stays saved", {
  skip_on_cran()
  local_home()
  s <- create_study("BD", planner = hier_planner())
  f <- .meta_file(s, "T-AE")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  # as the review's preview read it: no hierarchy seen (the old reading)
  stale <- hier_meta()
  stale$keys$AEDECOD <- NULL
  stale$by <- c("TRT01A", "AEBODSYS")
  stale$hierarchy <- character()
  stale$variables <- data.frame(variable = "AEDECOD", kind = "categorical",
                                levels = "SINUS BRADYCARDIA", n_levels = 1L,
                                stats = "n | N | p", label = NA_character_)
  saveRDS(stale, f)
  shiny::testServer(server_for("BD"), {
    rv <- session$userData$rv
    session$setInputs(target = "T-AE", nav = "make", step = "content", content_nav = "content",
                      table_nav = "builder")
    bf <- session$userData$bform
    b <- function(x) paste0("b", bf$n, "_", x)
    # the form's inputs as the page sends them when drawn
    st <- bf$st
    v <- list()
    v[[b("key")]] <- st$key
    v[[b("layers")]] <- st$row_keys
    v[[b("value")]] <- st$value
    v[[b("cat")]] <- st$cat_format
    v[[b("pct")]] <- st$pct_decimals
    v[[b("overall")]] <- st$overall_label %||% ""
    for (i in seq_along(st$row_keys)) v[[b(paste0("sort", i))]] <- st$sort[i]
    v[[b("sort_col")]] <- ""
    do.call(session$setInputs, v)
    session$elapse(1000)
    expect_identical(bf$st$hierarchy, c("AEBODSYS", "AEDECOD"))
    expect_false(session$userData$dirty())
  })
})
