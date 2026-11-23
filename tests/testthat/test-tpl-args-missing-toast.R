# A template report with a required argument empty that tflspec finds
# (demog: `continuous` or `categorical`, #752): its error is said in the
# app's language, naming the arguments by their card headings and the step
# they are filled in; its ARD code is not run while the error stands (no
# second note, "The ARD code ... did not run"); filled in, it runs.

skip_if_quick()  # slow: not in the quick check of a PR into next (helper.R)

# the notes shown (text, type, id) and the reports whose ARD code ran
watch_toasts <- function(env = parent.frame()) {
  seen <- new.env()
  seen$notes <- list()
  seen$fetched <- character()
  local_mocked_bindings(
    showNotification = function(ui, ..., id = NULL, type = "default") {
      seen$notes[[length(seen$notes) + 1L]] <- list(
        text = gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(ui), collapse = " "))),
        type = type, id = id %||% NA_character_)
      invisible("id")
    },
    showModal = function(ui, session = NULL) invisible(),
    removeModal = function(session = NULL) invisible(),
    .package = "shiny", .env = env)
  local_mocked_bindings(
    fetch_ard = function(study, output_id, ...) {
      seen$fetched <- c(seen$fetched, output_id)
      stop("(the ARD code ran)", call. = FALSE)
    },
    .env = env)
  seen
}
toast_texts <- function(seen) trimws(vapply(seen$notes, function(n) n$text, ""))
toast_ids <- function(seen) vapply(seen$notes, function(n) n$id, "")

# a new demog report with no variables, in the sample study (not run), on
# its content step
add_demog_no_vars <- function(session) {
  session$setInputs(add = 1, modal_type = "table", modal_how = "template", modal_tpl_table = "demog",
                    modal_id = "99-1", modal_desc = "Demography", modal_section = "Sec A",
                    mtpl_demog__population = "SAF", mtpl_demog__by = "TRT01A")
  session$setInputs(add_ok = 1)
  session$setInputs(target = "T-99-1", nav = "make", step = "content", content_nav = "content")
}

toast_study <- function(lang, env = parent.frame()) {
  home <- local_home(env)
  .set_config("language", lang)
  suppressMessages(create_sample_study(run = FALSE, home = home, study_id = "TOAST"))
  "TOAST"
}

skip_if_no_args_class <- function() {
  e <- tryCatch(tflspec::tfl_template_expand("demog", list(population = "SAF", by = "TRT01A")),
                error = identity)
  skip_if_not(inherits(e, "tflspec_template_args_missing"),
              "a tflspec without tflspec_template_args_missing")
}

ja_want <- paste0("連続変数（continuous）か ",
                  "カテゴリ変数（categorical）を 2 analysis ",
                  "で入れてください。")
en_want <- "Fill in Continuous variables (continuous) or Categorical variables (categorical) in 2 analysis."

test_that("the text names the arguments by their card headings and the step; English and Japanese", {
  local_home()
  ja <- function(x) tr(x, "ja")
  expect_identical(.tpl_args_missing_text("demog", c("continuous", "categorical"), any = TRUE), en_want)
  expect_identical(.tpl_args_missing_text("demog", c("continuous", "categorical"), any = TRUE, t = ja),
                   ja_want)
  # (each needed: "and"; the report's population a template hides: step 1)
  expect_identical(.tpl_args_missing_text("demog", c("population", "by")),
                   paste("Fill in Analysis set (population) and Variable that splits the columns (by)",
                         "in 1 data, 2 analysis."))
  # (the heading is the card's)
  prm <- .tpl_params("demog")
  expect_identical(.tpl_arg_heading(prm$continuous, ja), "連続変数（continuous）")
  # tflspec's condition, read as data; another error keeps its own message
  e <- structure(class = c("tflspec_template_args_missing", "error", "condition"),
                 list(message = "template demog: give ...", call = NULL, template = "demog",
                      args = c("continuous", "categorical"), any = TRUE))
  expect_identical(.error_text(e), en_want)
  expect_identical(.error_note_id(e), "tpl_args_missing")
  expect_identical(.error_text(simpleError("No file x")), "No file x")
  expect_null(.error_note_id(simpleError("No file x")))
  # where the preview waits: the rule as the text above, the card's own by name
  rule <- list(template = "demog", args = c("continuous", "categorical"), any = TRUE)
  expect_identical(.tpl_missing_say(structure(rule$args, rule = rule)), en_want)
  expect_identical(.tpl_missing_say("population"),
                   "Required arguments are empty: population. The preview waits for them.")
})

test_that("ja: the save's error names the arguments in Japanese; the ARD code is not run, no second note", {
  skip_if_no_args_class()
  id <- toast_study("ja")
  seen <- watch_toasts()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    add_demog_no_vars(session)
    expect_true("T-99-1" %in% names(rv$p$spec$outputs))
    # the new report's first preview waits: its ARD code is not run
    expect_identical(seen$fetched, character())
    expect_false(any(grepl("ARD", toast_texts(seen), fixed = TRUE)))
    seen$notes <- list()
    # (the save's R warning, tflspec's: the report's sheets kept as they are)
    suppressWarnings(session$setInputs(save = 1))
    hit <- grepl(ja_want, toast_texts(seen), fixed = TRUE)
    expect_identical(sum(hit), 1L)
    expect_false(any(grepl("give `continuous`", toast_texts(seen), fixed = TRUE)))
    expect_identical(toast_ids(seen)[hit], "tpl_args_missing")
    # Update preview: the same note (its id), nothing else; the ARD code not run
    seen$notes <- list()
    session$setInputs(ard_preview = 1)
    expect_identical(seen$fetched, character())
    expect_length(seen$notes, 1L)
    expect_match(toast_texts(seen), ja_want, fixed = TRUE)
    expect_identical(toast_ids(seen), "tpl_args_missing")
  })
})

test_that("en: a clear English text; once a variable is filled in, the ARD code runs as before", {
  skip_if_no_args_class()
  id <- toast_study("en")
  seen <- watch_toasts()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    add_demog_no_vars(session)
    expect_identical(seen$fetched, character())
    seen$notes <- list()
    session$setInputs(ard_preview = 1)
    expect_identical(seen$fetched, character())
    expect_length(seen$notes, 1L)
    expect_match(toast_texts(seen), en_want, fixed = TRUE)
    # filled in: the gate opens, and Update preview runs the report's ARD code
    session$setInputs(tpl_demog__continuous = "AGE")
    expect_identical(.tpl_of(rv$p, "T-99-1")$args$continuous, "AGE")
    expect_length(.tpl_missing_report(rv$p, "T-99-1"), 0L)
    seen$notes <- list()
    session$setInputs(ard_preview = 2)
    expect_true("T-99-1" %in% seen$fetched)
    expect_false(any(grepl("Fill in", toast_texts(seen), fixed = TRUE)))
  })
})
