test_that("args go into the fields and back, and nothing written by hand is lost", {
  skip_if_not_installed("cardx")
  f <- .ard_form_fields("cardx::ard_categorical_ci")
  expect_true(all(c("method", "conf.level", "value") %in% f$arg))
  expect_false(any(c("data", "variables", "by", "fmt_fun") %in% f$arg))
  x <- 'method = "wilson", conf.level = 0.9, value = everything() ~ "Y", weights = W, foo(1)'
  p <- .ard_args_parse(x, f)
  expect_identical(p$values$method, "wilson")
  expect_identical(p$values$conf.level, "0.9")
  expect_identical(p$values$value, "Y")
  expect_identical(p$other, "foo(1)")
  b <- .ard_args_build(p$values, f, p$other)
  expect_true(.ard_args_same(b, x))
  # what a field cannot show stays as R
  p2 <- .ard_args_parse("value = list(SEX = \"F\")", f)
  expect_identical(p2$other, "value = list(SEX = \"F\")")
  expect_identical(.ard_args_build(p2$values, f, p2$other), "value = list(SEX = \"F\")")
  # R that does not parse is kept whole
  expect_identical(.ard_args_parse("conf.level = ", f)$other, "conf.level = ")
  # nothing: NA
  expect_true(is.na(.ard_args_build(list(method = ""), f, "")))
  expect_identical(.ard_args_build(list(value = "1"), f), "value = everything() ~ 1")
})

test_that("the functions are the company's keywords and the catalog, by category", {
  skip_if_not_installed("cardx")
  e <- .ard_fn_entries(.std_ard_methods(), tflspec::tfl_ard_functions())
  expect_identical(e$category[1], "Company standard")
  expect_true("cardx::ard_categorical_ci" %in% e$value)
  # an old name only for the row that names it
  expect_false("cards::ard_continuous" %in% e$value)
  expect_true("cards::ard_continuous" %in%
                .ard_fn_entries(.std_ard_methods(), tflspec::tfl_ard_functions(),
                                current = "cards::ard_continuous")$value)
  # ard_stack() is offered (analyses inside it, #107); the other functions
  # that run others are not yet
  expect_identical(unique(e$state[e$value == "cards::ard_stack"]), "ok")
  expect_identical(unique(e$state[e$value == "cards::ard_strata"]), "later")
  # a study's own function is kept
  own <- .ard_fn_entries(.std_ard_methods(), tflspec::tfl_ard_functions(),
                         current = "mypkg::ard_mine")
  expect_identical(own$category[own$value == "mypkg::ard_mine"], "Own and code")
  # out of the builder's scope (Q9): not listed, but kept for a row naming one
  expect_false(any(c("cardx::ard_survey_svyttest", "cards::ard_formals") %in% e$value))
  expect_false("Survey designs" %in% e$category)
  sv <- .ard_fn_entries(.std_ard_methods(), tflspec::tfl_ard_functions(),
                        current = "cardx::ard_survey_svyttest")
  expect_identical(sv$state[sv$value == "cardx::ard_survey_svyttest"], "out")
  # the English heading is kept for the search
  expect_identical(e$label_en[e$value == "cardx::ard_stats_t_test"], "t test")
  expect_identical(.ard_method_call("hierarchical", .std_ard_methods()),
                   "cards::ard_stack_hierarchical")
  expect_true(is.na(.ard_method_call("custom", .std_ard_methods())))
})

test_that("a field's empty choice names the default when it is one value", {
  expect_identical(.ard_default_label("choice", "c(\"waldcc\", \"wald\")",
                                      c("waldcc", "wald"), "(d)", "(d: %s)"), "(d: waldcc)")
  expect_identical(.ard_default_label("number", "0.95", character(), "(d)", "(d: %s)"), "(d: 0.95)")
  expect_identical(.ard_default_label("levels", "list(where(is_binary) ~ 1L)",
                                      character(), "(d)", "(d: %s)"), "(d)")
  expect_identical(.ard_default_label("code", "NULL", character(), "(d)", "(d: %s)"), "(d)")
})

test_that("a level field offers the code lists first, then the data's values", {
  cl <- data.frame(variable = "SEX", value = c("M", "F"), label = NA, order = c("2", "1"))
  d <- data.frame(SEX = c("F", "M", "U"))
  expect_identical(.level_choices("SEX", cl, d), c("F", "M", "U"))
  expect_identical(.level_choices("SEX", NULL, d), c("F", "M", "U"))
})

test_that("the form picks a function, fills its arguments and writes args", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  local_home()
  s <- create_study("AG")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADSL, file.path(s$path, "data/adam/adsl.rds"))
  s$planner <- first_table(s$planner, "T1", "data/adam/adsl.rds", cards::ADSL,
                           "SAFFL", "TRT01A", c("AGE", "SEX"))
  a <- ard_rows(s$planner, "analyses", "T1")
  a$method[a$analysis_id == "CAT"] <- "cardx::ard_categorical_ci"
  a$args[a$analysis_id == "CAT"] <- "conf.level = 0.9, weights = W"
  s$planner <- set_ard_rows(s$planner, "analyses", "T1", a)
  save_study(s)
  shiny::testServer(server_for("AG"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "ard", target = "T1")
    session$setInputs(hot_ard_analyses_select = list(select = list(r = 3L)))
    st_env <- session$userData$st_env
    id <- function(x) paste0("st", st_env$n, "_", x)
    h <- output$ard_stat_ui$html
    expect_match(h, "Analysis CAT")
    # its category is open, the function chosen
    expect_match(output$ard_fn_list$html, "ard_categorical_ci", fixed = TRUE)
    g <- output$ard_an_args$html
    expect_match(g, "conf.level", fixed = TRUE)
    expect_match(g, 'value="0.9"', fixed = TRUE)
    # weights is a field of its own (R code), not the other arguments
    expect_match(g, 'value="W"', fixed = TRUE)
    # another category on screen does not change the chosen function
    inp <- list()
    inp[[id("fn_cat")]] <- "Summaries"
    do.call(session$setInputs, inp)
    expect_false(grepl("ard_categorical_ci", output$ard_fn_list$html, fixed = TRUE))
    session$setInputs(ard_stat_apply = 1)
    r <- ard_rows(rv$p, "analyses", "T1")
    r <- r[r$analysis_id == "CAT", ]
    expect_identical(r$method, "cardx::ard_categorical_ci")
    # nothing changed: the args as written
    expect_identical(r$args, "conf.level = 0.9, weights = W")
    # a field changed: args written from the fields, the rest kept
    inp <- list()
    inp[[id("a_method")]] <- "wilson"
    inp[[id("a_conf.level")]] <- "0.9"
    inp[[id("args_other")]] <- "max.iterations = 20"
    do.call(session$setInputs, inp)
    session$setInputs(ard_stat_apply = 2)
    r <- ard_rows(rv$p, "analyses", "T1")
    expect_identical(r$args[r$analysis_id == "CAT"],
                     'method = "wilson", conf.level = 0.9, weights = W, max.iterations = 20')
    # the chosen function is named above the list
    expect_match(output$ard_fn_now$html, "ard_categorical_ci", fixed = TRUE)
    # a level field starts at the default, never at a level: a function
    # chosen and applied untouched writes no args
    g <- output$ard_an_args$html
    expect_match(g, '<option value="" selected>', fixed = TRUE)
    a2 <- ard_rows(rv$p, "analyses", "T1")
    a2$args[a2$analysis_id == "CONT"] <- NA
    rv$p <- set_ard_rows(rv$p, "analyses", "T1", a2)
    session$setInputs(hot_ard_analyses_select = list(select = list(r = 2L)))
    inp <- list(); inp[[id("fn_pick")]] <- "cardx::ard_continuous_ci"
    do.call(session$setInputs, inp)
    session$setInputs(ard_stat_apply = 3)
    r <- ard_rows(rv$p, "analyses", "T1")
    expect_identical(r$method[r$analysis_id == "CONT"], "cardx::ard_continuous_ci")
    expect_true(is.na(r$args[r$analysis_id == "CONT"]))
    # a search across the categories: a heading, or the function's name
    # written as words
    inp <- list(); inp[[id("fn_q")]] <- "t test"
    do.call(session$setInputs, inp)
    expect_match(output$ard_fn_list$html, "ard_stats_t_test", fixed = TRUE)
    inp <- list(); inp[[id("fn_q")]] <- "continuous ci"
    do.call(session$setInputs, inp)
    expect_match(output$ard_fn_list$html, "ard_continuous_ci", fixed = TRUE)
    # in any case
    inp <- list(); inp[[id("fn_q")]] <- "T TEST"
    do.call(session$setInputs, inp)
    expect_match(output$ard_fn_list$html, "ard_stats_t_test", fixed = TRUE)
  })
})

test_that("every hint of the ARD catalog has its Japanese", {
  f <- system.file("ard", "args.csv", package = "tflspec")
  skip_if(!nzchar(f))
  a <- utils::read.csv(f, stringsAsFactors = FALSE)
  h <- unique(a$hint[!is.na(a$hint) & nzchar(a$hint)])
  keys <- utils::read.csv(system.file("i18n", "strings.csv", package = "tflplanner"),
                          stringsAsFactors = FALSE, encoding = "UTF-8")[[1]]
  miss <- setdiff(paste0("arg-hint:", h), keys)
  expect_identical(miss, character(), info = paste(miss, collapse = "\n"))
})

test_that("what the catalog does not offer is not listed", {
  skip_if_not_installed("cardx")
  f <- tflspec::tfl_ard_functions()
  skip_if(!"offered" %in% names(f))
  e <- .ard_fn_entries(.std_ard_methods(), f)
  expect_false(any(f$call[!f$offered] %in% e$value))
})
