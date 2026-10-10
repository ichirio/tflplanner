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
