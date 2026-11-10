# The branches of R/derive_builder.R the other tests leave out: what the
# form cannot hold is kept as R, the errors in words, and the editor's
# fields (a text written in the field, the days and R kinds, the branches of
# a split)

drv_kind <- function(x) .drv_read(x)$kind

# ---- reading: what the form cannot hold stays R, as written -----------------

test_that("a piece with no NAME = in front is an R expression with no name", {
  r <- .drv_read("  just_text  ")
  expect_identical(r[c("name", "kind", "expr", "text")],
                   list(name = "", kind = "r", expr = "just_text", text = "just_text"))
  # a comparison is not a name being given
  expect_identical(.drv_read("A == 1")$name, "")
})

test_that("a missing value in a split: kept as NA; one that is not a value, as R", {
  r <- .drv_read("X = ifelse(A > 1, NA, \"b\")")
  expect_identical(r$kind, "cond")
  expect_identical(r$branches[[1L]]$value, "NA")
  # (an NA of another type is not one value the field can show)
  expect_identical(drv_kind("X = ifelse(A > 1, NA_integer_, \"b\")"), "r")
  expect_identical(drv_kind("X = ifelse(A > 1, \"a\", f(B))"), "r")
})

test_that("a case_when() with .default reads back, and what it cannot hold stays R", {
  r <- .drv_read("X = dplyr::case_when(A > 1 ~ \"a\", .default = \"z\")")
  expect_identical(r$kind, "cond")
  expect_identical(r$branches, list(list(cond = "A > 1", value = "a")))
  expect_identical(r$otherwise, "z")
  # TRUE last is the otherwise; TRUE not last is a condition of its own
  r <- .drv_read("X = case_when(A > 1 ~ \"a\", TRUE ~ \"b\")")
  expect_identical(r$otherwise, "b")
  expect_length(r$branches, 1L)
  r <- .drv_read("X = case_when(TRUE ~ \"b\", A > 1 ~ \"a\")")
  expect_identical(vapply(r$branches, function(b) b$cond, ""), c("TRUE", "A > 1"))
  expect_identical(r$otherwise, "")
  # a .default that is not a value, an argument that is no formula, a named
  # one, a value that is not one value, and no condition at all
  expect_identical(drv_kind("X = case_when(A > 1 ~ \"a\", .default = f(B))"), "r")
  expect_identical(drv_kind("X = case_when(A > 1 ~ \"a\", other)"), "r")
  expect_identical(drv_kind("X = case_when(first = A > 1 ~ \"a\")"), "r")
  expect_identical(drv_kind("X = case_when(A > 1 ~ f(B))"), "r")
  expect_identical(drv_kind("X = case_when(TRUE ~ \"a\")"), "r")
})

test_that("a cut() the form cannot hold stays R, as written", {
  same <- c(
    "X = cut(AGE, 5, right = FALSE)",                             # breaks not c()
    "X = cut(AGE, c(-Inf, Inf), right = FALSE)",                  # no cut point
    "X = cut(AGE, c(0, 65, Inf), right = FALSE)",                 # does not start at -Inf
    "X = cut(AGE, c(-Inf, 65, 100), right = FALSE)",              # does not end at Inf
    "X = cut(AGE, c(-Inf, CUT, Inf), right = FALSE)",             # a cut point that is no number
    "X = cut(AGE, c(-Inf, 65, Inf), lab, right = FALSE)",         # labels not c()
    "X = cut(AGE, c(-Inf, 65, Inf), c(\"a\"), right = FALSE)",    # one label for two groups
    "X = cut(AGE, c(-Inf, 65, Inf), c(\"a\", 2), right = FALSE)", # a label that is no text
    "X = cut(f(AGE), c(-Inf, 65, Inf), right = FALSE)")           # not a column
  for (x in same) {
    r <- .drv_read(x)
    expect_identical(r$kind, "r", info = x)
    expect_identical(r$text, x, info = x)
  }
})

test_that("a days piece the form cannot hold stays R", {
  expect_identical(drv_kind("X = as.numeric(f(A) - B)"), "r")
  expect_identical(drv_kind("X = as.numeric(A - 1)"), "r")
  expect_identical(drv_kind("X = as.numeric(A + B)"), "r")
  expect_identical(drv_kind("X = as.numeric(A, B)"), "r")
  r <- .drv_read("X = as.numeric(AENDT - ASTDT)")
  expect_identical(r[c("kind", "end", "start", "plus1")],
                   list(kind = "days", end = "AENDT", start = "ASTDT", plus1 = FALSE))
})

# ---- writing: the errors in words --------------------------------------------

test_that("a cut with no variable, and an R with nothing written, are said", {
  expect_error(.drv_write(list(name = "X", kind = "cut", var = "", breaks = "65")),
               "Choose the variable to cut")
  expect_error(.drv_write(list(name = "X", kind = "cut", breaks = "65")),
               "Choose the variable to cut")
  expect_error(.drv_write(list(name = "X", kind = "r", expr = "  ")),
               "Write the R that makes the column")
  expect_error(.drv_write(list(name = "X", kind = "r")), "Write the R that makes the column")
})

# ---- the editor ---------------------------------------------------------------

# the editor on its own, over a few columns: what it tells the user is kept
drv_app <- function(told) {
  function(input, output, session) {
    session$userData$ed <- .derive_editor_server(
      input, output, session, t = identity,
      notify = function(msg, type = "message") told$msgs <- c(told$msgs, paste(type, msg)),
      cols = shiny::reactive(c("AGE", "AVAL", "ASTDT", "AENDT")),
      data = shiny::reactive(data.frame(AGE = 50, AVAL = 1)))
  }
}

drv_told <- function() {
  e <- new.env()
  e$msgs <- character()
  e
}

# a field of the editor being made, by the short name the fields have
drv_set <- function(session, ...) {
  a <- list(...)
  f <- session$userData$drv$fid
  do.call(session$setInputs, stats::setNames(a[c(FALSE, TRUE)], vapply(a[c(TRUE, FALSE)], f, "")))
}

test_that("the sheet's text written in the field is read again, and closes the editor", {
  skip_on_cran()
  shiny::testServer(drv_app(drv_told()), {
    ed <- session$userData$ed
    session$setInputs(drv_new = 1)
    expect_match(output$drv_editor$html, "Add the column", fixed = TRUE)
    session$setInputs(adata_derive = "G = ifelse(AGE < 65, \"a\", \"b\") | D = as.numeric(AENDT - ASTDT)")
    expect_identical(ed$text(),
                     "G = ifelse(AGE < 65, \"a\", \"b\") | D = as.numeric(AENDT - ASTDT)")
    expect_match(output$drv_list$html, "Split by conditions", fixed = TRUE)
    expect_match(output$drv_list$html, "Days between two dates", fixed = TRUE)
    expect_null(output$drv_editor$html)
    # the same text again: nothing read; blank: no columns made
    session$setInputs(adata_derive = "G = ifelse(AGE < 65, \"a\", \"b\") | D = as.numeric(AENDT - ASTDT)")
    expect_length(.drv_read_all(ed$text()), 2L)
    session$setInputs(adata_derive = "  ")
    expect_identical(ed$text(), NA_character_)
    expect_match(output$drv_list$html, "None: the data's columns as they are.", fixed = TRUE)
  })
})

test_that("a column that is not there is not taken out", {
  skip_on_cran()
  shiny::testServer(drv_app(drv_told()), {
    ed <- session$userData$ed
    ed$load("A = 1 | B = 2")
    session$setInputs(drv_del = 9)
    session$setInputs(drv_del = 0)
    expect_identical(ed$text(), "A = 1 | B = 2")
    session$setInputs(drv_del = 2)
    expect_identical(ed$text(), "A = 1")
  })
})

test_that("days between two dates, made in the form and then changed", {
  skip_on_cran()
  shiny::testServer(drv_app(drv_told()), {
    ed <- session$userData$ed
    session$setInputs(drv_new = 1)
    drv_set(session, "name", "DUR", "kind", "days")
    expect_match(output$drv_kind_ui$html, session$userData$drv$fid("end"), fixed = TRUE)
    expect_match(output$drv_kind_ui$html, "+ 1 (the first day is day 1)", fixed = TRUE)
    drv_set(session, "end", "AENDT", "start", "ASTDT", "plus1", TRUE)
    session$setInputs(drv_ok = 1)
    expect_identical(ed$text(), "DUR = as.numeric(AENDT - ASTDT) + 1")
    # changed: the same column written again, not another one
    session$setInputs(drv_edit = 1)
    drv_set(session, "name", "DUR", "kind", "days", "end", "AENDT", "start", "ASTDT", "plus1", FALSE)
    session$setInputs(drv_ok = 2)
    expect_identical(ed$text(), "DUR = as.numeric(AENDT - ASTDT)")
  })
})

test_that("an R expression made in the form, with a variable put in it", {
  skip_on_cran()
  put <- new.env()
  put$text <- character()
  testthat::local_mocked_bindings(
    updateTextInput = function(session, ...) {
      a <- list(...)
      put$text <- c(put$text, paste(a[[1L]], a$value))
    },
    updateSelectizeInput = function(...) invisible(),
    .package = "shiny")
  shiny::testServer(drv_app(drv_told()), {
    ed <- session$userData$ed
    f <- session$userData$drv$fid
    session$setInputs(drv_new = 1)
    drv_set(session, "name", "PCHG", "kind", "r")
    expect_match(output$drv_kind_ui$html, f("expr"), fixed = TRUE)
    expect_match(output$drv_kind_ui$html, "Put in a variable...", fixed = TRUE)
    # a variable chosen is put at the end of the R, a space before it
    drv_set(session, "expr", "round(", "insert", "AVAL")
    expect_true(paste(f("expr"), "round(AVAL") %in% put$text)
    drv_set(session, "expr", "AVAL /", "insert", "AGE")
    expect_true(paste(f("expr"), "AVAL / AGE") %in% put$text)
    drv_set(session, "expr", "AVAL", "insert", "AGE")
    expect_true(paste(f("expr"), "AVAL AGE") %in% put$text)
    drv_set(session, "expr", "round(AVAL / AGE * 100, 1)")
    session$setInputs(drv_ok = 1)
    expect_identical(ed$text(), "PCHG = round(AVAL / AGE * 100, 1)")
  })
})

test_that("a column the form cannot write: the user is told, nothing is made", {
  skip_on_cran()
  told <- drv_told()
  shiny::testServer(drv_app(told), {
    ed <- session$userData$ed
    # nothing is being made: the button does nothing
    session$setInputs(drv_ok = 1)
    expect_identical(told$msgs, character())
    session$setInputs(drv_new = 1)
    drv_set(session, "name", "", "kind", "r", "expr", "A")
    session$setInputs(drv_ok = 2)
    expect_match(told$msgs[1L], "warning A column made needs a name", fixed = TRUE)
    expect_identical(ed$text(), NA_character_)
    # the editor stays open to be put right
    expect_match(output$drv_editor$html, "Add the column", fixed = TRUE)
    drv_set(session, "name", "X", "kind", "r", "expr", "1")
    session$setInputs(drv_ok = 3)
    expect_identical(ed$text(), "X = 1")
    # the same name twice
    session$setInputs(drv_new = 2)
    drv_set(session, "name", "X", "kind", "r", "expr", "2")
    session$setInputs(drv_ok = 4)
    expect_match(told$msgs[2L], "X is made already", fixed = TRUE)
    expect_identical(ed$text(), "X = 1")
  })
})

test_that("a split's conditions: another one added, one chosen, one taken out", {
  skip_on_cran()
  shiny::testServer(drv_app(drv_told()), {
    ed <- session$userData$ed
    b <- session$userData$drv
    session$setInputs(drv_new = 1)
    drv_set(session, "name", "AGEGR", "kind", "cond")
    b$cond_value("AGE < 65")
    b$cond_key(shiny::isolate(b$cond_key()) + 1L)
    session$flushReact()
    drv_set(session, "val1", "young")
    # another: a second line, chosen, its condition blank
    session$setInputs(drv_br_add = 1)
    expect_match(output$drv_branches$html, "(the condition, below)", fixed = TRUE)
    expect_match(output$drv_branches$html, "AGE &lt; 65", fixed = TRUE)
    b$cond_value("AGE < 75")
    b$cond_key(shiny::isolate(b$cond_key()) + 1L)
    session$flushReact()
    drv_set(session, "val2", "middle", "else", "old")
    # the first chosen again: its value is kept
    session$setInputs(drv_br_pick = 1)
    expect_match(output$drv_branches$html, "AGE &lt; 65", fixed = TRUE)
    expect_match(output$drv_branches$html, "AGE &lt; 75", fixed = TRUE)
    session$setInputs(drv_ok = 1)
    expect_identical(ed$text(), paste0(
      "AGEGR = dplyr::case_when(AGE < 65 ~ \"young\", AGE < 75 ~ \"middle\", TRUE ~ \"old\")"))
    # opened again, the second taken out: the first stays with its value
    session$setInputs(drv_edit = 1)
    drv_set(session, "name", "AGEGR", "kind", "cond", "else", "old")
    session$setInputs(drv_br_del = 2)
    session$setInputs(drv_ok = 2)
    expect_identical(ed$text(), "AGEGR = ifelse(AGE < 65, \"young\", \"old\")")
  })
})

test_that("a split's last condition cannot be taken out, nor one that is not there", {
  skip_on_cran()
  shiny::testServer(drv_app(drv_told()), {
    ed <- session$userData$ed
    b <- session$userData$drv
    session$setInputs(drv_new = 1)
    drv_set(session, "name", "X", "kind", "cond")
    b$cond_value("AGE > 1")
    b$cond_key(shiny::isolate(b$cond_key()) + 1L)
    session$flushReact()
    drv_set(session, "val1", "y")
    session$setInputs(drv_br_del = 1)
    expect_match(output$drv_branches$html, "AGE &gt; 1", fixed = TRUE)
    session$setInputs(drv_br_add = 1)
    session$setInputs(drv_br_del = 7)
    expect_match(output$drv_branches$html, "AGE &gt; 1", fixed = TRUE)
    # a condition chosen that is not there: the condition built goes to no line
    session$setInputs(drv_br_pick = 9)
    b$cond_value("AGE > 99")
    b$cond_key(shiny::isolate(b$cond_key()) + 1L)
    session$flushReact()
    session$setInputs(drv_br_pick = 1)
    session$setInputs(drv_br_del = 2)
    session$setInputs(drv_ok = 1)
    expect_identical(ed$text(), "X = ifelse(AGE > 1, \"y\", NA)")
  })
})
