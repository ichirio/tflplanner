# The condition built from rows (R/condition_builder.R)

test_that("a condition is read into rows and written back the same", {
  same <- c('SAFFL == "Y"',
            'SAFFL == "Y" & PARAMCD %in% c("ALT", "AST")',
            '!(SAFFL %in% "Y")',
            'AGE >= 65 & !is.na(TRTSDT)',
            '(SEX == "F" & AGE < 50) | RACE == "ASIAN"',
            'ADT > as.Date("2024-01-01")',
            'AGE > -1',
            '`MY VAR` == "a"',
            'A %in% c(1, 2)',
            'is.na(DTHDT)')
  for (e in same) expect_identical(.cond_write(.cond_parse(e)), e, info = e)
  # written another way: the same rows
  expect_identical(.cond_tidy("SAFFL!='Y'"), '!(SAFFL %in% "Y")')
  expect_identical(.cond_tidy('SAFFL  ==  "Y"&AGE>=65'), 'SAFFL == "Y" & AGE >= 65')
  expect_identical(.cond_tidy('PARAMCD %in% "ALT"'), 'PARAMCD == "ALT"')
  # not equal keeps the blanks: !(x %in% ...)
  r <- .cond_parse('SAFFL != "Y"')
  expect_identical(r$op, "!=")
  expect_identical(.cond_write(r), '!(SAFFL %in% "Y")')
  # what the rows cannot hold
  for (e in c('!(A == 1 & B == 2)', 'grepl("x", AETERM)', 'A == B', 'A & (B | C)',
              'A == 1 & (B == 2 | C == 3)', 'A %in% c("a", 1)', 'x <- 1')) {
    expect_null(.cond_parse(e), info = e)
  }
  # none
  expect_identical(nrow(.cond_parse(NA)), 0L)
  expect_identical(nrow(.cond_parse("  ")), 0L)
  expect_true(is.na(.cond_write(.cond_empty())))
  # a row without values is left out
  r <- rbind(.cond_row(1, "SEX", "==", "F"), .cond_row(1, "RACE", "==", character()))
  expect_identical(.cond_write(r), 'SEX == "F"')
})

test_that("a column's values to choose from, with their counts", {
  ch <- .cond_choices(c("F", "M", "F", NA, ""))
  expect_identical(unname(ch), c("F", "M"))
  expect_identical(names(ch), c("F (2)", "M (1)"))
  expect_null(.cond_choices(c(1, 2)))
  expect_null(.cond_choices(as.Date("2024-01-01")))
  expect_length(.cond_choices(as.character(1:500), max = 200L), 200L)
  expect_identical(.cond_type(1), "num")
  expect_identical(.cond_type(Sys.Date()), "date")
  expect_identical(.cond_type("a"), "chr")
})

test_that("the rows build the condition; the definition's is read back", {
  d <- data.frame(SAFFL = c("Y", "Y", "N"), PARAMCD = c("ALT", "AST", "ALT"),
                  AGE = c(30, 70, 50), stringsAsFactors = FALSE)
  attr(d$SAFFL, "label") <- "Safety Population Flag"
  val <- shiny::reactiveVal('SAFFL == "Y"')
  shiny::testServer(condition_builder_server, args = list(
    data = shiny::reactive(d), value = val,
    shortcuts = shiny::reactive(c("Safety set" = 'SAFFL == "Y"'))), {
    session$flushReact()
    h <- output$body$html
    expect_match(h, "Safety Population Flag", fixed = TRUE)
    expect_match(h, "Y (2)", fixed = TRUE)
    expect_match(h, "Safety set", fixed = TRUE)
    # read back: nothing changed, the same condition
    expect_identical(session$returned()$expr, 'SAFFL == "Y"')
    # a value chosen
    session$setInputs(var_1 = "SAFFL", op_1 = "==", val_1 = c("Y", "N"))
    expect_identical(session$returned()$expr, 'SAFFL %in% c("Y", "N")')
    # another row
    session$setInputs(add = 1)
    session$setInputs(var_2 = "AGE", op_2 = ">=", val_2 = "65")
    expect_identical(session$returned()$expr, 'SAFFL %in% c("Y", "N") & AGE >= 65')
    # an "or" group
    session$setInputs(add_or = 1)
    session$setInputs(var_3 = "PARAMCD", op_3 = "!=", val_3 = "ALT")
    expect_identical(session$returned()$expr,
                     '(SAFFL %in% c("Y", "N") & AGE >= 65) | !(PARAMCD %in% "ALT")')
    # a row removed
    session$setInputs(del = 3)
    expect_identical(session$returned()$expr, 'SAFFL %in% c("Y", "N") & AGE >= 65')
    # a shortcut adds its rows
    session$setInputs(short = 1)
    expect_match(session$returned()$expr, 'SAFFL == "Y"$')
    # written as R, and back
    session$setInputs(to_raw = 1)
    expect_match(output$body$html, "textarea", fixed = TRUE)
    session$setInputs(raw = 'grepl("A", PARAMCD)')
    expect_identical(session$returned()$expr, 'grepl("A", PARAMCD)')
    expect_true(session$returned()$ok)
    session$setInputs(raw = 'PARAMCD ==')
    expect_false(session$returned()$ok)
  })
})

test_that("a condition the rows cannot hold stays as R; none is none", {
  val <- shiny::reactiveVal('grepl("x", AETERM)')
  shiny::testServer(condition_builder_server, args = list(
    data = shiny::reactive(NULL), value = val), {
    session$flushReact()
    expect_match(output$body$html, "textarea", fixed = TRUE)
    expect_match(output$body$html, "grepl", fixed = TRUE)
    # another definition's condition: read again
    val(NA_character_)
    session$flushReact()
    expect_match(output$body$html, "No condition", fixed = TRUE)
    expect_true(is.na(session$returned()$expr))
    # no data: the variable and values typed in
    session$setInputs(add = 1)
    session$setInputs(var_1 = "SEX", op_1 = "==", val_1 = "F, M")
    expect_identical(session$returned()$expr, 'SEX %in% c("F", "M")')
  })
})

test_that("the condition changes only when it changes", {
  d <- data.frame(SEX = c("F", "M"))
  val <- shiny::reactiveVal('SEX == "F"')
  n <- 0L
  shiny::testServer(function(input, output, session) {
    r <- condition_builder_server("c", data = shiny::reactive(d), value = val)
    shiny::observe({
      r()
      n <<- n + 1L
    })
  }, {
    session$flushReact()
    first <- n
    # the same value set again: nothing
    session$setInputs(`c-var_1` = "SEX", `c-op_1` = "==", `c-val_1` = "F")
    expect_identical(n, first)
    session$setInputs(`c-val_1` = c("F", "M"))
    expect_identical(n, first + 1L)
  })
})
