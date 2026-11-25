# The branches of R/srv_add_report.R the other tests leave out: the Add
# dialog's two ways (code and template) with what they refuse, the template
# report's cards (edit, revert, switch, data, convert to code) and its panes
# -- driven through shiny::testServer() on the whole app, as the other server
# tests do.

skip_if_quick()  # slow: not in the quick check of a PR into next (helper.R)

# what the server showed and where it went, instead of the screen: the notes,
# the dialogs (as text) and the tabs chosen
watch_add <- function(env = parent.frame()) {
  shown <- new.env()
  shown$notes <- list()
  shown$modals <- list()
  shown$removed <- 0L
  shown$navs <- list()
  local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      shown$notes[[length(shown$notes) + 1L]] <- list(text = paste(as.character(ui), collapse = " "),
                                                      type = type)
      invisible("id")
    },
    showModal = function(ui, session = NULL) {
      shown$modals[[length(shown$modals) + 1L]] <- as.character(ui)
      invisible()
    },
    removeModal = function(session = NULL) {
      shown$removed <- shown$removed + 1L
      invisible()
    },
    .package = "shiny", .env = env)
  local_mocked_bindings(
    nav_select = function(id, selected = NULL, ...) {
      shown$navs[[length(shown$navs) + 1L]] <- c(id = id, selected = selected)
      invisible()
    },
    .package = "bslib", .env = env)
  shown
}

add_notes <- function(shown) vapply(shown$notes, function(n) n$text, "")
add_modal <- function(shown) shown$modals[[length(shown$modals)]]
add_nav <- function(shown, id) {
  v <- vapply(shown$navs, function(x) if (identical(x[["id"]], id)) x[["selected"]] else NA_character_, "")
  v <- v[!is.na(v)]
  if (length(v)) v[length(v)] else NA_character_
}
# an output's HTML as one string (NULL: ""), and as its text
add_html <- function(x) paste(as.character(x), collapse = "")
add_text <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", add_html(x)))
add_row <- function(p, id) p$outputs[p$outputs$output_id == id, , drop = FALSE]
add_args <- function(p, id) p$spec$outputs[[id]]$template$args

# the sample study (not run), its ID
sample_for_add <- function(id = "ADDG") {
  home <- local_home(parent.frame())
  suppressMessages(create_sample_study(run = FALSE, home = home, study_id = id))
  id
}

# (a server takes long to start: the cases of one dialog run on one)

# ---- Add, as code ------------------------------------------------------------------------

test_that("Add as code: prefix, section and description are kept, blanks are left empty, an ID that exists is refused, and the new report's tab opens", {
  id <- sample_for_add()
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    # a report with all its fields
    rv <- session$userData$rv
    session$setInputs(add = 1)
    expect_match(add_modal(shown), "Add a report", fixed = TRUE)
    session$setInputs(modal_type = "listing", modal_how = "code", modal_id = "ZZ-1",
                      modal_desc = "  Zebra listing ", modal_section = " Zebras ")
    session$setInputs(add_ok = 1)
    row <- add_row(rv$p, "L-ZZ-1")
    expect_identical(nrow(row), 1L)
    expect_identical(row$description, "Zebra listing")
    expect_identical(row$section, "Zebras")
    expect_identical(add_nav(shown, "nav"), "make")
    expect_identical(add_nav(shown, "step"), "content")
    expect_gte(shown$removed, 1L)
    # a blank description and section
    rv <- session$userData$rv
    session$setInputs(add = 1, modal_type = "figure", modal_how = "code", modal_id = "ZZ-2",
                      modal_desc = "   ", modal_section = "")
    session$setInputs(add_ok = 2)
    row <- add_row(rv$p, "F-ZZ-2")
    expect_identical(nrow(row), 1L)
    expect_true(is.na(row$description))
    expect_true(is.na(row$section))
    expect_identical(report_info(rv$p, "F-ZZ-2")$type, "figure")
    # an ID that exists
    rv <- session$userData$rv
    before <- rv$p$outputs$output_id
    session$setInputs(add = 1, modal_type = "listing", modal_how = "code", modal_id = "L-16-2-7",
                      modal_desc = "")
    suppressMessages(session$setInputs(add_ok = 3))
    expect_identical(rv$p$outputs$output_id, before)
    expect_match(add_notes(shown)[length(shown$notes)], "L-16-2-7' is already on the list", fixed = TRUE)
    expect_identical(shown$notes[[length(shown$notes)]]$type, "error")
  })
})

# ---- Add, from a template: what it refuses ------------------------------------------------

test_that("Add from a template refuses with no template chosen, a required argument empty or an ID that exists, and adds nothing", {
  id <- sample_for_add()
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    # no template chosen
    rv <- session$userData$rv
    before <- rv$p$outputs$output_id
    session$setInputs(add = 1, modal_type = "table", modal_how = "template", modal_id = "ZZ-3",
                      modal_desc = "", modal_tpl_table = "")
    session$setInputs(add_ok = 1)
    expect_identical(add_notes(shown), "Choose a template")
    expect_identical(shown$notes[[1L]]$type, "warning")
    expect_identical(rv$p$outputs$output_id, before)
    # a required argument empty
    rv <- session$userData$rv
    before <- rv$p$outputs$output_id
    session$setInputs(add = 1, modal_type = "table", modal_how = "template", modal_id = "ZZ-4",
                      modal_desc = "", modal_tpl_table = "hierarchical")
    session$setInputs(add_ok = 2)
    expect_match(add_notes(shown)[length(shown$notes)], "Fill in the required argument ", fixed = TRUE)
    expect_identical(shown$notes[[length(shown$notes)]]$type, "warning")
    expect_identical(rv$p$outputs$output_id, before)
    # an ID that exists
    rv <- session$userData$rv
    before <- rv$p$outputs$output_id
    session$setInputs(add = 1, modal_type = "listing", modal_how = "template", modal_tpl_listing = "listing",
                      modal_id = "L-16-2-7", modal_desc = "", modal_section = "",
                      mtpl_listing__data = "ADAE", mtpl_listing__columns = "USUBJID | Subject")
    suppressMessages(session$setInputs(add_ok = 3))
    expect_identical(rv$p$outputs$output_id, before)
    expect_match(add_notes(shown)[length(shown$notes)], "L-16-2-7' is already on the list", fixed = TRUE)
    expect_identical(shown$notes[[length(shown$notes)]]$type, "error")
  })
})

# ---- the dialog ------------------------------------------------------------------------------

test_that("the dialog's Create waits for an ID and the required arguments, and the other arguments are offered below", {
  id <- sample_for_add()
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    session$setInputs(add = 1, modal_type = "table", modal_how = "template",
                      modal_tpl_table = "demog", modal_id = "")
    # (each kind has its template list; the chosen one's required argument is in the dialog)
    dlg <- add_modal(shown)
    for (k in c("table", "listing", "figure")) expect_match(dlg, sprintf('id="modal_tpl_%s"', k), fixed = TRUE)
    # (a template with a data argument: its required fields drawn apart, to follow the dataset)
    expect_match(add_html(output$mtpl_demog___req_box$html), 'id="mtpl_demog__population"', fixed = TRUE)
    expect_match(add_html(output$modal_ok_js$html), "prop('disabled', true)", fixed = TRUE)
    # an ID, but the analysis set is not chosen
    session$setInputs(modal_id = "ZZ-5")
    expect_match(add_html(output$modal_ok_js$html), "prop('disabled', true)", fixed = TRUE)
    # (the analysis set: the report's population, asked here; `by` has no default)
    session$setInputs(mtpl_demog__population = "SAF")
    expect_match(add_html(output$modal_ok_js$html), "prop('disabled', true)", fixed = TRUE)
    session$setInputs(mtpl_demog__by = "TRT01A")
    expect_match(add_html(output$modal_ok_js$html), "prop('disabled', false)", fixed = TRUE)
    # the template's other arguments: each its own input, counted in the heading
    opt0 <- Filter(function(x) !isTRUE(x$required), .tpl_shown(.tpl_params("demog")))
    opt <- Filter(function(x) !.tpl_is_key(x), opt0)
    more <- add_html(output$tpl_modal_more$html)
    expect_match(more, sprintf("More arguments (%d)", length(opt)), fixed = TRUE)
    for (nm in names(opt0)) expect_match(more, sprintf('id="mtpl_demog__%s"', nm), fixed = TRUE)
  })
})

# ---- a template table: Add, its cards, its panes ------------------------------------------

test_that("Add from a template writes the report with its template, the arguments changed and the section; the panes show it", {
  id <- sample_for_add()
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    session$setInputs(add = 1, modal_type = "table", modal_how = "template", modal_tpl_table = "demog",
                      modal_id = "ZZ-5", modal_desc = " Demography ", modal_section = "Sec A",
                      mtpl_demog__population = "SAF", mtpl_demog__by = "TRT01A",
                      # (an optional argument left at its default is not written; one changed is)
                      mtpl_demog__total = FALSE,
                      mtpl_demog__continuous = "AGE")
    session$setInputs(add_ok = 1)
    row <- add_row(rv$p, "T-ZZ-5")
    expect_identical(row$description, "Demography")
    expect_identical(row$section, "Sec A")
    expect_identical(row$population, "SAF")
    tp <- .tpl_of(rv$p, "T-ZZ-5")
    expect_identical(tp$name, "demog")
    expect_identical(tp$args, list(continuous = "AGE", by = "TRT01A"))
    expect_identical(add_nav(shown, "step"), "content")
    # the panes of a template table
    session$setInputs(target = "T-ZZ-5", nav = "make", step = "content", content_nav = "content")
    expect_match(add_html(output$tpl_card_ard$html), 'id="tpl_demog__total"', fixed = TRUE)
    # the template itself: one card above the steps
    expect_match(add_text(output$tpl_card_head$html), "Convert to code", fixed = TRUE)
    expect_false(grepl("Convert to code", add_text(output$tpl_card$html), fixed = TRUE))
    expect_match(add_text(output$tpl_card_data$html), "Template arguments: data", fixed = TRUE)
    expect_match(add_text(output$tpl_card_data$html), "Analysis set (population)", fixed = TRUE)
    expect_false(grepl("Template arguments: page", add_text(output$tpl_card_page$html), fixed = TRUE))
    expect_match(add_text(output$tpl_spec_yaml$html), "spec/outputs/T-ZZ-5.yml", fixed = TRUE)
    expect_match(add_text(output$tpl_spec_yaml$html), "name: demog", fixed = TRUE)
    expect_match(add_text(output$tpl_spec_yaml$html), "Expanded (read only)", fixed = TRUE)
    expect_match(add_text(output$tpl_code_expanded$html), "The content's code expanded", fixed = TRUE)
    expect_match(add_text(output$tpl_code_expanded$html), "plan_cells(", fixed = TRUE)
    # (the new table's first preview has made its ARD: the pane shows it, not "Update preview")
    pane <- add_text(output$tpl_ard_pane$html)
    expect_false(grepl("Update preview to see the ARD", pane, fixed = TRUE))
    expect_match(pane, "A run saves it in the study's ARD", fixed = TRUE)
    expect_match(output$tpl_ard_code, "programs/ard/T-ZZ-5.R", fixed = TRUE)
    expect_match(add_text(output$tpl_ard_code_head$html), "programs/ard/T-ZZ-5.R", fixed = TRUE)
    # the analysis set: the data step's field, written as the report's population
    other <- setdiff(stats::na.omit(rv$p$ard$populations$population_id), "SAF")[1L]
    skip_if(is.na(other), "the sample study has one population")
    session$setInputs(tpl_report_pop = other)
    expect_identical(add_row(rv$p, "T-ZZ-5")$population, other)
    expect_identical(rv$p$spec$outputs[["T-ZZ-5"]]$common$population, other)
    expect_null(.tpl_of(rv$p, "T-ZZ-5")$args$population)
  })
})

test_that("a template card: an edit is written, a value just replaced is not an edit, a report's own value can be reverted, a header is warned about, and another template asks first", {
  # On a slow machine a write and its say-back are far apart: the 15 s
  # window of .tpl_stale_say() is widened so the test does not depend on it.
  withr::local_options(tflplanner.tpl_stale_window = 3600)
  id <- sample_for_add()
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    session$setInputs(add = 1, modal_type = "table", modal_how = "template", modal_tpl_table = "shift",
                      modal_id = "ZZ-6", modal_desc = "", modal_section = "",
                      mtpl_shift__population = "SAF")
    suppressMessages(session$setInputs(add_ok = 1))
    session$setInputs(target = "T-ZZ-6", nav = "make", step = "content", content_nav = "content")
    invisible(suppressMessages(output$tpl_card$html))
    expect_identical(add_args(rv$p, "T-ZZ-6"), list(population = "SAF"))
    # a choice: written when it differs from the template's default ("none")
    session$setInputs(tpl_shift__split = "group_safe")
    expect_identical(add_args(rv$p, "T-ZZ-6")$split, "group_safe")
    # (the value the write just replaced, said again by a card drawn before it: not an edit)
    session$setInputs(tpl_shift__split = "none")
    expect_identical(add_args(rv$p, "T-ZZ-6")$split, "group_safe")
    session$setInputs(tpl_revert = "split")
    expect_null(add_args(rv$p, "T-ZZ-6")$split)
    # a text, written; Revert takes it out
    session$setInputs(tpl_shift__analysis_id = "A1")
    expect_identical(add_args(rv$p, "T-ZZ-6")$analysis_id, "A1")
    session$setInputs(tpl_revert = "analysis_id")
    expect_identical(add_args(rv$p, "T-ZZ-6"), list(population = "SAF"))
    # Revert of an argument the report does not give, an empty field for one it does not give: nothing
    session$setInputs(tpl_revert = "total", tpl_shift__post_header = "")
    expect_identical(add_args(rv$p, "T-ZZ-6"), list(population = "SAF"))
    # a header text with no {col}, or with braces that do not match: kept, and warned about
    n0 <- length(shown$notes)
    session$setInputs(tpl_shift__post_header = "no col here")
    expect_identical(add_args(rv$p, "T-ZZ-6")$post_header, "no col here")
    expect_identical(add_notes(shown)[-seq_len(n0)],
                     "The header has no {col}: each category's column gets the same text.")
    expect_identical(shown$notes[[length(shown$notes)]]$type, "warning")
    session$setInputs(tpl_shift__post_header = "{col (x")
    expect_identical(add_args(rv$p, "T-ZZ-6")$post_header, "{col (x")
    expect_identical(add_notes(shown)[length(shown$notes)],
                     "The header's braces don't match: it prints as typed.")
    # another template: asked first, with what is kept and what is dropped
    session$setInputs(tpl_shift__post_header = "{col}", tpl_shift__analysis_id = "A1")
    to <- "lab_shift"
    stopifnot(!"analysis_id" %in% names(.tpl_shown(.tpl_params(to))))
    nm <- length(shown$modals)
    # (the same template again: nothing to ask)
    session$setInputs(tpl_switch = "shift")
    expect_length(shown$modals, nm)
    session$setInputs(tpl_switch = to)
    dlg <- add_text(add_modal(shown))
    expect_match(dlg, "Change the template to lab_shift", fixed = TRUE)
    expect_match(dlg, "Arguments kept: population", fixed = TRUE)
    expect_match(dlg, "Arguments dropped: analysis_id", fixed = TRUE)
    # (nothing is written before OK)
    expect_identical(.tpl_of(rv$p, "T-ZZ-6")$name, "shift")
    session$setInputs(tpl_switch_cancel = 1)
    expect_identical(.tpl_of(rv$p, "T-ZZ-6")$name, "shift")
    suppressWarnings(session$setInputs(tpl_switch_ok = 1))
    expect_identical(.tpl_of(rv$p, "T-ZZ-6")$name, to)
    expect_identical(add_args(rv$p, "T-ZZ-6")$population, "SAF")
    expect_null(add_args(rv$p, "T-ZZ-6")$analysis_id)
  })
})

# ---- a template report writes no `extends` (tflspec: the template's own) -----------------------

test_that("a template report made in the dialog writes no extends", {
  skip_if_not("tfl_output_extends" %in% getNamespaceExports("tflspec"), "tflspec's extends per step (#778)")
  id <- sample_for_add()
  watch_add()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    session$setInputs(add = 1, modal_type = "listing", modal_how = "template", modal_tpl_listing = "listing",
                      modal_id = "ZZ-8", modal_desc = "", modal_section = "",
                      mtpl_listing__data = "ADAE", mtpl_listing__columns = "USUBJID | Subject")
    session$setInputs(add_ok = 1)
    expect_identical(.tpl_of(rv$p, "L-ZZ-8")$name, "listing")
    expect_null(rv$p$spec$outputs[["L-ZZ-8"]]$extends)
    txt <- paste(rv$p$spec$text[["spec/outputs/L-ZZ-8.yml"]], collapse = "
")
    expect_false(grepl("extends", txt, fixed = TRUE))
    # (and the page configuration chosen on it writes its `style:` alone, no extends)
    q <- .pgs_add_pattern(rv$p, "Wide")
    q <- .pgs_set_pattern(q, "L-ZZ-8", "Wide")
    expect_identical(q$spec$outputs[["L-ZZ-8"]]$style, "Wide")
    expect_null(q$spec$outputs[["L-ZZ-8"]]$extends)
  })
})

# ---- a template listing: its data, its conversion to code --------------------------------------

test_that("a template listing: its data card writes the dataset's steps, keeps a join of its own and takes the steps away; Convert to code asks first", {
  id <- sample_for_add()
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    session$setInputs(add = 1, modal_type = "listing", modal_how = "template", modal_tpl_listing = "listing",
                      modal_id = "ZZ-7", modal_desc = "", modal_section = "",
                      mtpl_listing__data = "ADAE", mtpl_listing__columns = "USUBJID | Subject")
    session$setInputs(add_ok = 1)
    expect_identical(.tpl_of(rv$p, "L-ZZ-7")$name, "listing")
    session$setInputs(target = "L-ZZ-7", nav = "make", step = "content", content_nav = "content",
                      lf_right = "code")
    steps <- function() rv$p$spec$outputs[["L-ZZ-7"]]$data$steps
    # no dataset: asked for
    session$setInputs(tpl_data_ds = "", tpl_data_apply = 1)
    expect_identical(add_notes(shown)[length(shown$notes)], "Choose the dataset.")
    expect_null(steps())
    # the dataset, the study's analysis set, and a record condition
    session$setInputs(tpl_data_ds = "ADAE", tpl_data_param = "", tpl_data_pop = "SAF",
                      tpl_data_filter = "AESER == 'Y'", tpl_data_apply = 2)
    f <- .tpl_steps_fields(steps())
    expect_identical(c(f$dataset, f$filter), c("ADAE", "AESER == 'Y'"))
    expect_true(f$join)
    expect_identical(f$join_where, "SAFFL == \"Y\"")
    expect_identical(rv$p$spec$outputs[["L-ZZ-7"]]$common$population, "SAF")
    # the report's own join, as it is: no population of the study
    session$setInputs(tpl_data_pop = ".own", tpl_data_apply = 3)
    expect_true(.tpl_steps_fields(steps())$join)
    expect_true(is.null(rv$p$spec$outputs[["L-ZZ-7"]]$common$population))
    # no analysis set and no filter: the dataset is read, and that is all
    session$setInputs(tpl_data_pop = "", tpl_data_filter = "", tpl_data_apply = 4)
    f <- .tpl_steps_fields(steps())
    expect_identical(f$dataset, "ADAE")
    expect_false(f$join)
    expect_identical(f$filter, "")
    # the preview button of the listing: on to its result
    session$setInputs(lf_preview_tpl = 1)
    expect_identical(add_nav(shown, "lf_right"), "result")
    # Go to the card, then Convert to code
    session$setInputs(nav = "make")
    session$setInputs(tpl_goto_card = 1)
    expect_identical(add_nav(shown, "step"), "content")
    session$setInputs(tpl_to_code = 1)
    expect_match(add_text(add_modal(shown)), "Convert L-ZZ-7 to code? ", fixed = TRUE)
    expect_match(add_text(add_modal(shown)), "This cannot be undone", fixed = TRUE)
    expect_false(is.null(.tpl_of(rv$p, "L-ZZ-7")))
    session$setInputs(tpl_to_code_ok = 1)
    expect_null(.tpl_of(rv$p, "L-ZZ-7"))
    expect_identical(add_notes(shown)[length(shown$notes)],
                     "L-ZZ-7 is a code report now: its items are on the usual cards (save to keep it).")
  })
})

# ---- a template figure ------------------------------------------------------------------------

test_that("a template figure's preview draws the picture, says when the definition has changed, and waits for a required argument", {
  id <- sample_for_add()
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    session$setInputs(add = 1, modal_type = "figure", modal_how = "template", modal_tpl_figure = "km",
                      modal_id = "ZZ-8", modal_desc = "", modal_section = "",
                      mtpl_km__by = "TRT01A", mtpl_km__param = "OS")
    suppressWarnings(session$setInputs(add_ok = 1))
    expect_identical(.tpl_of(rv$p, "F-ZZ-8")$name, "km")
    session$setInputs(target = "F-ZZ-8", nav = "make", step = "content", content_nav = "content",
                      fig_right = "ard")
    expect_match(add_html(output$tpl_fig_out$html), "Update preview draws the figure", fixed = TRUE)
    # (the ARD's tabs are what is looked at: the result tab is not put in front)
    suppressWarnings(session$setInputs(tpl_fig_preview = 1))
    expect_match(add_html(output$tpl_fig_out$html), "data:image/png;base64,", fixed = TRUE)
    expect_false(grepl("Changed: preview again", add_html(output$tpl_fig_out$html), fixed = TRUE))
    expect_true(is.na(add_nav(shown, "fig_right")))
    # a definition changed since: said above the picture
    session$setInputs(tpl_km__conf_level = 0.9)
    expect_match(add_html(output$tpl_fig_out$html), "Changed: preview again", fixed = TRUE)
    # a required argument emptied: the preview waits for it, on to the result tab
    suppressWarnings(session$setInputs(fig_right = "code", tpl_km__by = ""))
    expect_null(add_args(rv$p, "F-ZZ-8")$by)
    suppressWarnings(session$setInputs(tpl_fig_preview = 2))
    expect_match(add_text(output$tpl_fig_out$html),
                 "Required arguments are empty: by. The preview waits for them.", fixed = TRUE)
    expect_identical(add_nav(shown, "fig_right"), "result")
  })
})

# ---- demog: the dialog's untouched fields, the switch, the population ---------------------

# What a field of the dialog sends untouched, as the browser has it: the
# start value (the study's, else the default) as tpl_widget() draws it
add_untouched <- function(x, study) {
  start <- .tpl_study_value(study, x) %||% x$default
  switch(.tpl_type(x),
    flag = isTRUE(start),
    number = .num_or_null(start),
    text = if (is.null(start)) "" else .tpl_show(start),
    list = .tpl_list_text(start, x),
    vars = , chips = , choices = as.character(start %||% character()),
    as.character(start %||% "")[1L])
}

test_that("the dialog writes only the optional arguments changed in it: demog and stf_t02_demog untouched write none", {
  id <- sample_for_add()
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    for (nm in c("demog", "stf_t02_demog")) {
      rid <- paste0("ZD-", nm)
      opt <- Filter(function(x) !isTRUE(x$required), .tpl_shown(.tpl_params(nm)))
      sent <- lapply(opt, add_untouched, study = rv$p$spec$study)
      names(sent) <- sprintf("mtpl_%s__%s", nm, names(opt))
      sent <- Filter(Negate(is.null), sent)
      # (every field as the browser sends it, then the required ones set)
      do.call(session$setInputs, c(list(add = 1, modal_type = "table", modal_how = "template",
                                        modal_tpl_table = nm, modal_id = rid, modal_desc = "",
                                        modal_section = ""), sent))
      req <- list("SAF", "TRT01A", "AGE", "SEX")
      names(req) <- sprintf("mtpl_%s__%s", nm, c("population", "by", "continuous", "categorical"))
      do.call(session$setInputs, req)
      suppressMessages(session$setInputs(add_ok = which(nm == c("demog", "stf_t02_demog"))))
      args <- add_args(rv$p, paste0("T-", rid))
      expect_setequal(names(args), c("by", "continuous", "categorical"))
      expect_identical(add_row(rv$p, paste0("T-", rid))$population, "SAF")
    }
    # (a heading of two lines: a box that keeps the line break)
    more <- add_html(output$tpl_modal_more$html)
    expect_match(more, "<textarea[^>]*id=\"mtpl_stf_t02_demog__col_header\"")
    # (a field changed is written: FALSE blank rows as the flag, not "false")
    session$setInputs(add = 2, modal_type = "table", modal_how = "template", modal_tpl_table = "demog",
                      modal_id = "ZD-3", mtpl_demog__population = "SAF", mtpl_demog__by = "TRT01A",
                      mtpl_demog__col_header = "{col}\nN={n}", mtpl_demog__stub_before = FALSE)
    suppressMessages(session$setInputs(add_ok = 3))
    args <- add_args(rv$p, "T-ZD-3")
    expect_identical(args$col_header, "{col}\nN={n}")
  })
})

test_that("a switch to demog keeps population as the report's population; back from demog it is the argument again", {
  id <- sample_for_add()
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    session$setInputs(add = 1, modal_type = "table", modal_how = "template", modal_tpl_table = "shift",
                      modal_id = "ZZ-7", modal_desc = "", modal_section = "",
                      mtpl_shift__population = "SAF")
    suppressMessages(session$setInputs(add_ok = 1))
    session$setInputs(target = "T-ZZ-7", nav = "make", step = "content", content_nav = "content")
    invisible(suppressMessages(output$tpl_card$html))
    session$setInputs(tpl_switch = "demog")
    dlg <- add_text(add_modal(shown))
    expect_match(dlg, "To the report's population: population", fixed = TRUE)
    expect_false(grepl("Arguments dropped:[^.]*population", dlg))
    suppressWarnings(suppressMessages(session$setInputs(tpl_switch_ok = 1)))
    expect_identical(.tpl_of(rv$p, "T-ZZ-7")$name, "demog")
    expect_null(add_args(rv$p, "T-ZZ-7")$population)
    expect_identical(rv$p$spec$outputs[["T-ZZ-7"]]$common$population, "SAF")
    # (back: the report's population is the argument again)
    session$setInputs(tpl_switch = "shift")
    dlg <- add_text(add_modal(shown))
    expect_match(dlg, "From the report's population: population", fixed = TRUE)
    suppressWarnings(suppressMessages(session$setInputs(tpl_switch_ok = 2)))
    expect_identical(.tpl_of(rv$p, "T-ZZ-7")$name, "shift")
    expect_identical(add_args(rv$p, "T-ZZ-7")$population, "SAF")
  })
})

test_that("demog with no population: no ARD run and no error note, the preview waits; its field's tip is the usual one", {
  id <- sample_for_add()
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    session$setInputs(add = 1, modal_type = "table", modal_how = "template", modal_tpl_table = "demog",
                      modal_id = "ZZ-8", modal_desc = "", modal_section = "",
                      mtpl_demog__population = "SAF", mtpl_demog__by = "TRT01A",
                      mtpl_demog__continuous = "AGE")
    # (the dialog's population field: the analysis set's tip, not tflspec's note)
    dlg <- add_html(add_modal(shown))
    expect_false(grepl("not a template parameter", dlg, fixed = TRUE))
    suppressMessages(session$setInputs(add_ok = 1))
    session$setInputs(target = "T-ZZ-8", nav = "make", step = "data", content_nav = "content")
    expect_false(grepl("not a template parameter", add_html(output$tpl_card_data$html), fixed = TRUE))
    session$setInputs(tpl_report_pop = "")
    expect_identical(.tpl_missing_report(rv$p, "T-ZZ-8"), "population")
    n0 <- length(shown$notes)
    suppressWarnings(session$setInputs(tpl_preview = 1))
    new <- shown$notes[-seq_len(n0)]
    expect_length(new, 1L)
    expect_identical(new[[1L]]$type, "warning")
    expect_match(new[[1L]]$text, "Required arguments are empty: population", fixed = TRUE)
  })
})

test_that("a summary's preview (demog's arguments) reads the study standard the report resolves with: its count cells and arm headings", {
  skip_on_cran()
  home <- local_home()
  s <- suppressMessages(sample_study_copy(home))
  # (a report names <series>/<template>: the study holds its copy)
  skip_if_not("tfl_copy_templates" %in% getNamespaceExports("tflspec"), "this tflspec has no study copies (tflspec#879)")
  invisible(tflspec::tfl_copy_templates(s$path, "tflspec_v1/tflspec_summary"))
  # (the values' text from the study's code list, as demog's default said it)
  o <- tflspec::tfl_output("T-DM", "table", label = "Demog", common = list(population = "SAF"),
                           template = list(name = "tflspec_v1/tflspec_summary",
                                           args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX",
                                                       value_labels = list(
                                                         SEX = tflspec::tfl_r("study$codelists$SEX")))))
  s$planner <- .spec_add_output(s$planner, o, label = "Demog", population = "SAF")
  s <- suppressMessages(save_study(s, home = home))
  suppressMessages(fetch_ard(s, "T-DM", home = home))
  ard <- ard_raw(s, "T-DM", home = home)
  skip_if(is.null(ard), "the report's ARD was not made")
  # (a study standard as tflspec's spec-yaml study says it: plan_cells'
  # categorical, plan_col_header's lines -- the template's study references)
  st <- tflspec::tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  # (the series template leaves normalize_ard to the study standard: kept from the study's own)
  own <- Filter(function(it) identical(it$fn, "normalize_ard"), s$planner$spec$study$defaults$content)
  expect_length(own, 1L)
  s$planner$spec$study$defaults$content <- c(own, st$defaults$content)
  r <- tflspec::tfl_resolve_output(.code_spec(s$planner)$outputs[["T-DM"]], .code_spec(s$planner)$study)
  cells <- Filter(function(it) identical(it$fn, "plan_cells"), r$content)[[1L]]$args
  expect_false(is.null(cells$categorical))
  expect_true(any(vapply(r$content, function(it) identical(it$fn, "plan_col_header"), NA)))
  pages <- .preview_content_pages(s$planner, "T-DM", list(ard = ard))
  hdr <- unlist(pages[[1L]]$col_header)
  expect_true(any(grepl("^[(]N=[0-9]+[)]$", hdr)))
  expect_true("Characteristic" %in% hdr)
  # (the categorical rows' values, indented: hard spaces taken away)
  txt <- trimws(gsub(" ", " ", unlist(lapply(pages[[1L]]$data, as.character)), fixed = TRUE))
  expect_true(all(c("Female", "Male") %in% txt))
})

test_that("the dialog shows a key argument open under the required ones, marked, and counts only the rest in the fold", {
  id <- sample_for_add()
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    session$setInputs(add = 1, modal_type = "table", modal_how = "template",
                      modal_tpl_table = "ae_soc_pt", modal_id = "")
    prm <- .tpl_shown(.tpl_params("ae_soc_pt"))
    top <- names(Filter(function(x) !isTRUE(x$required) && .tpl_is_key(x), prm))
    skip_if(!length(top), "this tflspec has no key tier")
    rest <- setdiff(names(Filter(function(x) !isTRUE(x$required), prm)), top)
    more <- add_html(output$tpl_modal_more$html)
    expect_match(more, "rp-tpl-badge-key", fixed = TRUE)
    expect_match(more, sprintf("More arguments (%d)", length(rest)), fixed = TRUE)
    fold <- sub("^.*<details", "<details", more)
    for (nm in top) {
      expect_match(more, sprintf('id="mtpl_ae_soc_pt__%s"', nm), fixed = TRUE)
      expect_false(grepl(sprintf('id="mtpl_ae_soc_pt__%s"', nm), fold, fixed = TRUE))
    }
  })
})

# ---- Add from a series template: the study's copy (tflspec#879) ------------------------------

test_that("Add from a series template copies it into the study and says so", {
  skip_if_not("tfl_copy_templates" %in% getNamespaceExports("tflspec"), "this tflspec has no study copies (tflspec#879)")
  id <- sample_for_add("TCOPY")
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    dir <- rv$study$path
    nm <- "tflspec_v1/tflspec_summary"
    session$setInputs(add = 1, modal_type = "table", modal_how = "template", modal_id = "TC-1",
                      modal_desc = "", modal_tpl_table = nm)
    do.call(session$setInputs, stats::setNames(list("TRT01A"), paste0("mtpl_", .tpl_id_part(nm), "__by")))
    session$setInputs(add_ok = 1)
    expect_true("T-TC-1" %in% rv$p$outputs$output_id)
    expect_true(file.exists(.tpl_copy_file(dir, nm)))
    expect_true(any(grepl("Copied the template tflspec_v1/tflspec_summary into this study", add_notes(shown), fixed = TRUE)))
    expect_match(add_html(output$series_copies$html), "tflspec_v1/tflspec_summary", fixed = TRUE)
    # a second report from it: the copy there, nothing said
    n <- length(shown$notes)
    session$setInputs(add = 2, modal_type = "table", modal_how = "template", modal_id = "TC-2",
                      modal_desc = "", modal_tpl_table = nm)
    do.call(session$setInputs, stats::setNames(list("TRT01A"), paste0("mtpl_", .tpl_id_part(nm), "__by")))
    session$setInputs(add_ok = 2)
    expect_true("T-TC-2" %in% rv$p$outputs$output_id)
    new <- add_notes(shown)[seq_len(length(shown$notes) - n) + n]
    expect_false(any(grepl("Copied the template", new, fixed = TRUE)))
  })
})

test_that("a KM template's data card: its button says what it does, an action, not a save (#754)", {
  id <- sample_for_add("KMB")
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    o <- tflspec::tfl_output("F-KMB-1", "figure", label = "KM",
                             template = list(name = "km_survival"),
                             data = list(steps = .tpl_steps_from("ADTTE", NULL, "", "")))
    rv$p <- .spec_add_output(rv$p, o, label = "KM")
    session$setInputs(target = "F-KMB-1", nav = "make", step = "data")
    session$flushReact()
    h <- add_html(output$tpl_card_data$html)
    expect_match(h, ">Match the analysis set</span></button>", fixed = TRUE)
    expect_match(h, "Writes the report&#39;s data: steps from these fields", fixed = TRUE)
    expect_false(grepl(">Apply</span></button>", h, fixed = TRUE))
    # (what it does is as before: the data: steps written, the analysis set's join)
    session$setInputs(tpl_data_ds = "ADTTE", tpl_data_param = "", tpl_data_pop = "SAF",
                      tpl_data_filter = "", tpl_data_apply = 1)
    f <- .tpl_steps_fields(rv$p$spec$outputs[["F-KMB-1"]]$data$steps)
    expect_true(f$join)
    # (the set on the report's data, as the dialog writes it: no `common:`, #868)
    expect_identical(rv$p$spec$outputs[["F-KMB-1"]]$data$population, "SAF")
    expect_null(rv$p$spec$outputs[["F-KMB-1"]]$common$population)
  })
})

test_that("a KM template in the dialog: the report's set is the table's control, the study's sets only; Create writes the data's population, no common: and no join (#868)", {
  skip_if_not("km_survival" %in% tflspec::tfl_templates()$name)
  id <- sample_for_add("KMD")
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    session$setInputs(add = 1, modal_type = "figure", modal_how = "template",
                      modal_tpl_figure = "km_survival", modal_id = "")
    dlg <- add_html(add_modal(shown))
    # (no control of its own, no flag choices)
    expect_false(grepl("___data_pop", dlg, fixed = TRUE))
    expect_false(grepl('value="flag:', dlg, fixed = TRUE))
    # the control a table's report set has: its id, its label, the study's sets
    sel_of <- function(html, input_id) {
      m <- regmatches(html, regexpr(sprintf('(?s)<select id="%s".*?</select>', input_id), html, perl = TRUE))
      expect_length(m, 1L)
      v <- regmatches(m, gregexpr('value="[^"]*"', m))[[1L]]
      sub('^value="([^"]*)"$', "\\1", v)
    }
    label_of <- function(html, input_id) {
      at <- as.integer(regexpr(sprintf('for="%s"', input_id), html, fixed = TRUE))
      expect_gt(at, 0L)
      substr(html, at, at + 400L)
    }
    sets <- as.character(stats::na.omit(rv$p$ard$populations$population_id))
    expect_setequal(sel_of(dlg, "mtpl_km_survival__population"), c("", sets))
    expect_match(label_of(dlg, "mtpl_km_survival__population"), "This report(&#39;|')s population")
    # (the table's: the same label and choices)
    session$setInputs(modal_type = "table", modal_tpl_table = "demog")
    req <- add_html(output$mtpl_demog___req_box$html)
    expect_match(label_of(req, "mtpl_demog__population"), "This report(&#39;|')s population")
    expect_setequal(sel_of(req, "mtpl_demog__population"), c("", sets))
    # (ADSL's Y/N flags no set says: told under it, to be registered in the study)
    fl <- .tpl_flags_undefined(.tpl_data_cols(rv$study$path, rv$p)[["ADSL"]], rv$p$ard$populations)
    if (length(fl)) {
      expect_match(dlg, "is registered as a study population (spec/data.yml populations) first", fixed = TRUE)
    }
    # Create: the set on the report's data, the steps the dataset's and its parameter's
    session$setInputs(modal_type = "figure", modal_tpl_figure = "km_survival", modal_id = "KM-1",
                      modal_desc = "", modal_section = "",
                      mtpl_km_survival___data_ds = "ADTTE", mtpl_km_survival___data_param = "TTSKIN",
                      mtpl_km_survival__population = "SAF", mtpl_km_survival__by = "TRT01A")
    suppressMessages(session$setInputs(add_ok = 1))
    o <- rv$p$spec$outputs[["F-KM-1"]]
    expect_false(is.null(o))
    expect_identical(o$data$population, "SAF")
    expect_null(o$common$population)
    expect_identical(vapply(o$data$steps, function(s) s$step, ""), c("read", "param"))
    expect_null(o$template$args$population)
    txt <- paste(rv$p$spec$text[["spec/outputs/F-KM-1.yml"]], collapse = "\n")
    expect_false(grepl("common:", txt, fixed = TRUE))
    expect_false(grepl("join", txt, fixed = TRUE))
    # (the toc line's set, which the report list reads)
    toc <- Filter(function(e) identical(e$id, "F-KM-1"), rv$p$spec$study$toc$outputs)
    expect_identical(toc[[1L]]$population, "SAF")
    # no set chosen: none written (a KM type may have none)
    session$setInputs(add = 2, modal_type = "figure", modal_how = "template", modal_tpl_figure = "km_survival",
                      modal_id = "KM-2", modal_desc = "", modal_section = "",
                      mtpl_km_survival___data_ds = "ADTTE", mtpl_km_survival___data_param = "TTSKIN",
                      mtpl_km_survival__population = "", mtpl_km_survival__by = "TRT01A")
    suppressMessages(session$setInputs(add_ok = 2))
    o2 <- rv$p$spec$outputs[["F-KM-2"]]
    expect_false(is.null(o2))
    expect_null(o2$data$population)
    expect_null(o2$common$population)
  })
})

test_that("the dialog has one control for the report's set on every figure template: no population field, the flags note behind its (?) (#868, decided by coordinator 11-26 night)", {
  id <- sample_for_add("KMP")
  shown <- watch_add()
  shiny::testServer(server_for(id), {
    rv <- session$userData$rv
    figs <- .tpl_list("figure")$name
    want <- intersect(c("km_survival", "forest_hr", "tflspec_v1/forest_hr", "tflspec_v1/km_simple"), figs)
    skip_if(!length(want), "no figure template to look at")
    fl <- .tpl_flags_undefined(.tpl_data_cols(rv$study$path, rv$p)[["ADSL"]], rv$p$ard$populations)
    for (i in seq_along(want)) {
      nm <- want[i]
      session$setInputs(add = i, modal_type = "figure", modal_how = "template",
                        modal_tpl_figure = nm, modal_id = "")
      session$flushReact()
      box <- tryCatch(output[[sprintf("mtpl_%s___req_box", .tpl_id_part(nm))]]$html, error = function(e) NULL)
      h <- paste0(add_html(add_modal(shown)), add_html(box), add_html(output$tpl_modal_more$html))
      sel <- sprintf('<select id="mtpl_%s__population"', .tpl_id_part(nm))
      expect_equal(lengths(regmatches(h, gregexpr(sel, h, fixed = TRUE))), 1L, info = nm)
      # (the set control, not an argument: no field headed "(population)")
      expect_false(grepl("Analysis set (population)", h, fixed = TRUE), info = nm)
      # (the flags note is no line under it; it is in the (?) of the control.
      # The dialog's own small lines -- the report type's note, .type_notes --
      # are not it: no small text-muted line says the flags)
      lines <- regmatches(h, gregexpr('(?s)<p class="small text-muted[^"]*">.*?</p>', h, perl = TRUE))[[1L]]
      expect_false(any(grepl("Y/N flags", lines, fixed = TRUE)), info = nm)
      if (length(fl)) {
        expect_match(h, "is registered as a study population (spec/data.yml populations) first", fixed = TRUE)
      }
    }
    # (the code dialog's control has its (?) too)
    session$setInputs(add = length(want) + 1L, modal_type = "figure", modal_how = "code")
    session$flushReact()
    code <- add_html(add_modal(shown))
    at <- regexpr('for="modal_pop"', code, fixed = TRUE)
    if (at > 0L) expect_match(substr(code, at, at + 600L), "rp-tip", fixed = TRUE)
  })
})

test_that("the dialog's set control writes where the template takes the set: a shown argument too, else the report's set alone (#868)", {
  skip_if_not("forest_hr" %in% tflspec::tfl_templates()$name)
  prm <- .tpl_params("forest_hr")
  expect_true(.tpl_asks_report_pop(prm))
  expect_false("population" %in% names(.tpl_dialog_shown(prm)))
  a <- .tpl_dialog_args(prm, list(population = "SAF", param = "PFS"))
  expect_null(attr(a, "missing"))
  expect_identical(attr(a, "population"), "SAF")
  expect_identical(a$population, "SAF")
  # (required by the template: no set, no Create)
  b <- .tpl_dialog_args(prm, list(population = "", param = "PFS"))
  expect_identical(attr(b, "missing"), "population")
  # (a template with no `population`: the report's set alone, optional)
  v1 <- "tflspec_v1/forest_hr"
  skip_if_not(v1 %in% .tpl_list("figure")$name)
  p1 <- .tpl_params(v1)
  expect_null(p1$population)
  expect_identical(.tpl_pop_param(p1)$name, "population")
  expect_false(isTRUE(.tpl_pop_param(p1)$required))
  c1 <- .tpl_dialog_args(p1, list(population = "SAF", by = "TRT01A"))
  expect_null(attr(c1, "missing"))
  expect_identical(attr(c1, "population"), "SAF")
  expect_null(c1$population)
  d1 <- .tpl_dialog_args(p1, list(population = "", by = "TRT01A"))
  expect_null(attr(d1, "missing"))
  expect_null(attr(d1, "population"))
})

test_that("one set of labels for the same meaning in forest_hr and tflspec_v1/forest_hr: group, reference, compared, subgroup (#868)", {
  head_of <- function(nm, a) .tpl_arg_heading(.tpl_params(nm)[[a]], identity)
  ja <- function(en) tr(en, "ja")
  expect_identical(ja("Group"), "群")
  expect_identical(ja("Reference treatment group"), "基準群")
  expect_identical(ja("Group compared with reference"), "基準群に対して示す群")
  expect_identical(ja("Subgroup variables"), "部分集団")
  expect_identical(.tpl_arg_labels[["group"]], "Group")
  expect_identical(.tpl_arg_labels[["ref"]], "Reference treatment group")
  expect_identical(.tpl_arg_labels[["comparison"]], "Group compared with reference")
  expect_identical(.tpl_arg_labels[["subgroups"]], "Subgroup variables")
  expect_identical(.tpl_arg_labels[["subgroup_vars"]], "Subgroup variables")
  if ("forest_hr" %in% tflspec::tfl_templates()$name) {
    expect_identical(head_of("forest_hr", "group"), "Group (group)")
    expect_identical(head_of("forest_hr", "ref"), "Reference treatment group (ref)")
    expect_identical(head_of("forest_hr", "comparison"), "Group compared with reference (comparison)")
  }
  v1 <- "tflspec_v1/forest_hr"
  if (v1 %in% .tpl_list("figure")$name) {
    expect_identical(head_of(v1, "by"), "Group (by)")
    expect_identical(head_of(v1, "ref"), "Reference treatment group (ref)")
    expect_identical(head_of(v1, "comparison"), "Group compared with reference (comparison)")
    expect_identical(head_of(v1, "subgroup_vars"), "Subgroup variables (subgroup_vars)")
  }
  # (the (?) texts say the group, not the treatment arm alone)
  expect_false(any(grepl("投与群", c(ja("The reference arm; empty for the first in the code list."),
                                      ja("The arm compared against the reference")))))
})
