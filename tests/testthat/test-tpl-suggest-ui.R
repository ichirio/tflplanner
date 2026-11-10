# The add-report dialog's suggestion (R/srv_add_report.R, tpl_widget()): a
# placeholder and the top of the choices, never a filled-in value.
# Fictional data: Example Pharma's study.

# demog's parameters with the defaults of `args` taken away, as tflspec's batch
# will leave them
mock_no_default <- function(args, tpl = "demog", env = parent.frame()) {
  orig <- .tpl_params
  testthat::local_mocked_bindings(.tpl_params = function(name) {
    p <- orig(name)
    if (identical(name, tpl)) for (a in args) p[[a]]$default <- NULL
    p
  }, .env = env)
}

ui_study <- function(id) {
  s <- create_study(id)
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  d <- data.frame(USUBJID = paste0("EX-", 1:4), AGE = c(40, 52, 61, 33), SEX = c("F", "M", "F", "M"),
                  RACE = c("WHITE", "ASIAN", "WHITE", "BLACK"), WEIGHTBL = c(60, 80, 72, 90),
                  stringsAsFactors = FALSE)
  saveRDS(d, file.path(s$path, "data/adam/adsl.rds"))
  s
}

open_demog <- function(session) {
  session$setInputs(add = 1, modal_type = "table", modal_how = "template", modal_tpl_table = "demog",
                    modal_id = "T-D", modal_desc = "x")
}

# the <select> and its options (the script after it) of an argument in the dialog's "More arguments"
field_html <- function(output, arg) {
  h <- as.character(output$tpl_modal_more$html)
  m <- regexpr(sprintf("<select [^>]*id=\"mtpl_demog__%s\".*?</script>", arg), h)
  regmatches(h, m)
}

test_that("no default: an empty field, the suggestion as placeholder, 'Add all' first (multi-pick)", {
  skip_on_cran()
  local_home()
  ui_study("U1")
  mock_no_default(c("continuous", "categorical"))
  shiny::testServer(server_for("U1"), {
    open_demog(session)
    f <- field_html(output, "continuous")
    expect_length(f, 1L)
    expect_match(f, "multiple", fixed = TRUE)
    expect_false(grepl("selected", f, fixed = TRUE))
    # (the top group: Add all, before the data's columns)
    expect_match(f, "Add all suggested (AGE, WEIGHTBL)", fixed = TRUE)
    expect_lt(regexpr("Add all suggested", f, fixed = TRUE), regexpr(">USUBJID<", f, fixed = TRUE))
    expect_match(f, "<optgroup label=\"Suggested\"", fixed = TRUE)
    h <- as.character(output$tpl_modal_more$html)
    expect_match(h, "Suggested: AGE, WEIGHTBL", fixed = TRUE)
    expect_match(h, "Suggested: SEX, RACE", fixed = TRUE)
    expect_match(h, "onItemAdd", fixed = TRUE)
  })
})

test_that("'Add all' picked: the field's values are the suggested ones, in order; others kept", {
  skip_on_cran()
  local_home()
  ui_study("U2")
  mock_no_default(c("continuous", "categorical"))
  shiny::testServer(server_for("U2"), {
    open_demog(session)
    session$setInputs(mtpl_demog__population = "SAF", mtpl_demog__continuous = .tpl_suggest_all, mtpl_demog__categorical = "SEX",
                      mtpl_demog__by = "TRT01A")
    session$setInputs(add_ok = 1)
    tp <- .tpl_of(session$userData$rv$p, "T-D")
    expect_identical(tp$args$continuous, c("AGE", "WEIGHTBL"))
    expect_identical(tp$args$categorical, "SEX")
  })
  expect_identical(.tpl_suggest_expand(c("RACE", .tpl_suggest_all, "SEX"), c("SEX", "AGE")),
                   c("RACE", "SEX", "AGE"))
  expect_identical(.tpl_suggest_expand(c("A", "B"), "C"), c("A", "B"))
})

test_that("nothing picked: nothing written (the placeholder is not a value)", {
  skip_on_cran()
  local_home()
  ui_study("U3")
  mock_no_default(c("continuous"))
  shiny::testServer(server_for("U3"), {
    open_demog(session)
    session$setInputs(mtpl_demog__population = "SAF", mtpl_demog__by = "TRT01A", mtpl_demog__continuous = character(0))
    session$setInputs(add_ok = 1)
    tp <- .tpl_of(session$userData$rv$p, "T-D")
    expect_null(tp$args$continuous)
  })
})

test_that("a param with a default: the same field as before, no suggestion", {
  skip_on_cran()
  local_home()
  ui_study("U4")
  shiny::testServer(server_for("U4"), {
    open_demog(session)
    h <- as.character(output$tpl_modal_more$html)
    expect_false(grepl("Add all suggested", h, fixed = TRUE))
    expect_false(grepl("Suggested", h, fixed = TRUE))
  })
})

test_that("a template or argument not in the table: no extra item", {
  skip_on_cran()
  local_home()
  ui_study("U5")
  shiny::testServer(server_for("U5"), {
    session$setInputs(add = 1, modal_type = "listing", modal_how = "template", modal_tpl_listing = "listing",
                      modal_id = "L-1", modal_desc = "x")
    expect_false(grepl("Suggested", as.character(output$tpl_modal_more$html), fixed = TRUE))
  })
})

test_that("a single pick (var): the suggested value is the first item of the choices, no Add all", {
  skip_on_cran()
  local_home()
  s <- ui_study("U6")
  saveRDS(data.frame(USUBJID = "EX-1", AEBODSYS = "Cardiac disorders", AEDECOD = "Palpitations",
                     stringsAsFactors = FALSE), file.path(s$path, "data/adam/adae.rds"))
  mock_no_default("term", tpl = "ae_pt_freq")
  shiny::testServer(server_for("U6"), {
    session$setInputs(add = 1, modal_type = "table", modal_how = "template", modal_tpl_table = "ae_pt_freq",
                      modal_id = "T-A", modal_desc = "x")
    h <- as.character(output$tpl_modal_more$html)
    m <- regexpr("<select [^>]*id=\"mtpl_ae_pt_freq__term\".*?</script>", h)
    f <- regmatches(h, m)
    expect_length(f, 1L)
    expect_false(grepl("multiple", f, fixed = TRUE))
    expect_false(grepl("Add all", f, fixed = TRUE))
    expect_match(f, "Suggested: AEDECOD", fixed = TRUE)
    expect_match(f, "<optgroup label=\"Suggested\">[[:space:]]*<option value=\"AEDECOD\"")
    expect_false(grepl("value=\"AEDECOD\"[^>]*selected", f))
  })
})

test_that(".tpl_suggest_select: a single pick lists the suggested values as the first group, no Add all", {
  tt <- function(x) x
  h <- as.character(.tpl_suggest_select("f", "F", c("", "AEBODSYS", "AEDECOD", "X"), "AEDECOD", FALSE,
                                        list(), tt))
  expect_match(h, "<optgroup label=\"Suggested\">", fixed = TRUE)
  expect_match(h, "placeholder\":\"Suggested: AEDECOD", fixed = TRUE)
  expect_false(grepl("Add all", h, fixed = TRUE))
  expect_false(grepl("onItemAdd", h, fixed = TRUE))
  # (the blank first, the suggestion not again below it)
  expect_lt(regexpr("value=\"\"", h, fixed = TRUE), regexpr("optgroup", h, fixed = TRUE))
  expect_length(gregexpr("value=\"AEDECOD\"", h, fixed = TRUE)[[1L]], 1L)
  # (Esc closes the list only; the list is no wider than the field)
  expect_match(h, "stopPropagation", fixed = TRUE)
  expect_match(h, "max-width", fixed = TRUE)
})

test_that(".tpl_suggest_select: a multi-pick lists each suggested value, once, with Add all above them", {
  tt <- function(x) x
  s <- paste0("V", 1:8)
  h <- as.character(.tpl_suggest_select("g", "G", c("OTHER", s), s, TRUE, list(), tt))
  expect_match(h, "Add all suggested (V1, V2, V3, …)", fixed = TRUE)
  expect_lt(regexpr("Add all", h, fixed = TRUE), regexpr("value=\"V1\"", h, fixed = TRUE))
  expect_length(gregexpr("value=\"V1\"", h, fixed = TRUE)[[1L]], 1L)
  expect_lt(regexpr("value=\"V8\"", h, fixed = TRUE), regexpr("value=\"OTHER\"", h, fixed = TRUE))
  # (the swap and the upkeep of the item: selectize's options, not page script)
  for (k in c("onItemAdd", "onItemRemove", "addItem", "removeOption", "stopPropagation", "max-width")) {
    expect_match(h, k, fixed = TRUE)
  }
  # (a search never finds it; taking it away is not closing the list)
  expect_match(h, "getScoreFunction", fixed = TRUE)
  expect_match(h, "if (value === '__tpl_suggest_all__') return;", fixed = TRUE)
  # (one suggested value: no Add all)
  h1 <- as.character(.tpl_suggest_select("g", "G", c("OTHER", s), "V2", TRUE, list(), tt))
  expect_false(grepl("<option value=\"__tpl_suggest_all__\"", h1, fixed = TRUE))
  expect_match(h1, "value=\"V2\"", fixed = TRUE)
})

test_that("a template with a data argument: its required fields follow the dataset chosen", {
  skip_on_cran()
  local_home()
  s <- ui_study("U7")
  saveRDS(data.frame(USUBJID = "EX-1", AEBODSYS = "Cardiac disorders", AEDECOD = "Palpitations",
                     stringsAsFactors = FALSE), file.path(s$path, "data/adam/adae.rds"))
  saveRDS(data.frame(USUBJID = "EX-1", CMCLAS = "Analgesics", CMDECOD = "Paracetamol",
                     stringsAsFactors = FALSE), file.path(s$path, "data/adam/adcm.rds"))
  shiny::testServer(server_for("U7"), {
    session$setInputs(add = 1, modal_type = "table", modal_how = "template", modal_tpl_table = "hierarchical",
                      modal_id = "T-H", modal_desc = "x")
    box <- function() as.character(output$mtpl_hierarchical___req_box$html)
    expect_match(box(), "Suggested: AEBODSYS, AEDECOD", fixed = TRUE)
    session$setInputs(mtpl_hierarchical__data = "ADCM")
    expect_match(box(), "Suggested: CMCLAS, CMDECOD", fixed = TRUE)
    expect_false(grepl("AEBODSYS", box(), fixed = TRUE))
    # ("Add all" picked: the new dataset's values)
    session$setInputs(mtpl_hierarchical__population = "SAF", mtpl_hierarchical__hierarchy = .tpl_suggest_all)
    session$setInputs(add_ok = 1)
    tp <- .tpl_of(session$userData$rv$p, "T-H")
    expect_identical(tp$args$hierarchy, c("CMCLAS", "CMDECOD"))
  })
})
