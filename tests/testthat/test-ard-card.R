# The table builder's card of what the report's ARD holds (#335): one line,
# a small table when opened, the whole normalized ARD on a button.

card_meta <- function() {
  list(by = "TRT01A", hierarchy = character(),
       keys = list(TRT01A = c("Placebo", "Low", "High")),
       variables = data.frame(
         variable = c("AGE", "SEX", "RACE"), kind = c("continuous", "categorical", "categorical"),
         levels = c(NA, "F | M", "A | B | C | D | E"), n_levels = c(0L, 2L, 5L),
         stats = c("N | mean | sd | median | min | max | p25", "n | p", "n | p"),
         label = c("Age (years)", "SEX", NA), stringsAsFactors = FALSE),
       stats = c("n", "N", "p", "mean", "sd", "median", "min", "max", "p25"),
       fetched = as.POSIXct("2026-10-11 09:05:00"))
}

test_that("the one line: columns, rows, statistics, when read", {
  x <- ard_card_summary(card_meta())
  expect_identical(x$line, paste0(
    "ARD: columns TRT01A (3 groups) | 3 variables (2 categorical, 1 continuous) | ",
    "statistics n, N, p, mean, sd, median \u2026 | read 10/11 09:05"))
  r <- x$rows
  expect_identical(r$role, c("column", "row: variable", "row: variable", "row: variable"))
  expect_identical(r$variable, c("TRT01A", "AGE", "SEX", "RACE"))
  expect_identical(r$kind, c("group", "continuous", "categorical", "categorical"))
  expect_identical(r$detail[1L], "3 levels: Placebo, Low, High")
  expect_identical(r$detail[2L], "N, mean, sd, median, min, max, p25")
  expect_identical(r$detail[4L], "5 levels: A, B, C, D, \u2026")
  # a label only where it says more than the name
  expect_identical(r$label, c(NA, "Age (years)", NA, NA))
})

test_that("pages, a hierarchy, two column keys; no meta", {
  m <- card_meta()
  m$by <- c("TRT01A", "SEX")
  m$keys$SEX <- c("F", "M")
  m$hierarchy <- c("AEBODSYS", "AEDECOD")
  m$keys$AEBODSYS <- c("SOC1", "SOC2")
  m$keys$AEDECOD <- c("PT1", "PT2", "PT3")
  x <- ard_card_summary(m, page_by = "AEBODSYS")
  expect_match(x$line, "columns TRT01A \u00d7 SEX (3 \u00d7 2 groups)", fixed = TRUE)
  expect_match(x$line, "pages AEBODSYS | rows AEDECOD", fixed = TRUE)
  expect_identical(x$rows$role[1:4], c("column", "column", "page split", "row: hierarchy"))
  # the column keys are not counted among the variables
  expect_match(x$line, "2 variables (1 categorical, 1 continuous)", fixed = TRUE)
  # a hierarchy table with no analysis variable: nothing said of variables
  m2 <- m
  m2$variables <- m2$variables[0, ]
  expect_false(grepl("variables", ard_card_summary(m2)$line, fixed = TRUE))
  none <- ard_card_summary(NULL)
  expect_null(none$rows)
  expect_match(none$line, "not read yet", fixed = TRUE)
  # in Japanese
  ja <- ard_card_summary(card_meta(), tr = function(x) tr(x, lang = "ja"))
  expect_match(ja$line, "\u5217 TRT01A\uff083 \u7fa4\uff09", fixed = TRUE)
  expect_identical(ja$rows$role[1L], "\u5217")
})

test_that("the builder shows the card, and the whole ARD on its button", {
  skip_on_cran()
  skip_if_not_installed("cards")
  local_home()
  p <- add_standard_defaults(add_output(new_planner(), "T-AC", type = "table"), "AC")
  s <- create_study("AC", planner = p)
  ard <- cards::ard_stack(cards::ADSL, .by = TRT01A,
                          cards::ard_continuous(variables = AGE),
                          cards::ard_categorical(variables = SEX))
  data <- rtfreporter::normalize_ard(ard)
  f <- .meta_file(s, "T-AC")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(ard_meta(ard, data), f)
  saveRDS(data, sub("[.]rds$", "_data.rds", f))
  shiny::testServer(server_for("AC"), {
    session$setInputs(target = "T-AC", nav = "make", step = "content",
                      content_nav = "content", table_nav = "builder")
    h <- output$builder_ard_card$html
    expect_match(h, "ARD: columns TRT01A (3 groups)", fixed = TRUE)
    expect_match(h, "2 variables (1 categorical, 1 continuous)", fixed = TRUE)
    expect_match(h, "ard_card_all", fixed = TRUE)
    session$setInputs(ard_card_all = 1)
    # the default columns, then all of them
    dt <- output$ard_card_dt
    expect_false(is.null(dt))
    session$setInputs(ard_card_allcols = TRUE)
    expect_false(is.null(output$ard_card_dt))
  })
})
