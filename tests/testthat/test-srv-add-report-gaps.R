# The branches of R/srv_add_report.R the other tests leave out: the Add
# dialog's two ways (code and template) with what they refuse, the template
# report's cards (edit, revert, switch, data, convert to code) and its panes
# -- driven through shiny::testServer() on the whole app, as the other server
# tests do.

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
    expect_match(add_html(output$tpl_card$html), 'id="tpl_demog__total"', fixed = TRUE)
    expect_match(add_text(output$tpl_card_data$html), "Analysis data", fixed = TRUE)
    expect_match(add_text(output$tpl_card_data$html), "Analysis set (population)", fixed = TRUE)
    expect_false(grepl("Template arguments (page)", add_text(output$tpl_card_page$html), fixed = TRUE))
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
    expect_identical(rv$p$spec$outputs[["T-ZZ-5"]]$population, other)
    expect_null(.tpl_of(rv$p, "T-ZZ-5")$args$population)
  })
})

test_that("a template card: an edit is written, a value just replaced is not an edit, a report's own value can be reverted, a header is warned about, and another template asks first", {
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
    # a flag: written when it differs from the template's default (TRUE)
    session$setInputs(tpl_shift__param_page = FALSE)
    expect_identical(add_args(rv$p, "T-ZZ-6")$param_page, FALSE)
    # (the value the write just replaced, said again by a card drawn before it: not an edit)
    session$setInputs(tpl_shift__param_page = TRUE)
    expect_identical(add_args(rv$p, "T-ZZ-6")$param_page, FALSE)
    session$setInputs(tpl_revert = "param_page")
    expect_null(add_args(rv$p, "T-ZZ-6")$param_page)
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
    expect_identical(rv$p$spec$outputs[["L-ZZ-7"]]$population, "SAF")
    # the report's own join, as it is: no population of the study
    session$setInputs(tpl_data_pop = ".own", tpl_data_apply = 3)
    expect_true(.tpl_steps_fields(steps())$join)
    expect_true(is.na(rv$p$spec$outputs[["L-ZZ-7"]]$population))
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
    expect_identical(rv$p$spec$outputs[["T-ZZ-7"]]$population, "SAF")
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

test_that("demog's preview reads the study standard the report resolves with: its count cells and arm headings", {
  skip_on_cran()
  skip_if_not_installed("cards")
  home <- local_home()
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  o <- tflspec::tfl_output("T-DM", "table", label = "Demog", population = "SAF",
                           template = list(name = "demog",
                                           args = list(by = "TRT01A", continuous = "AGE", categorical = "SEX")))
  s$planner <- .spec_add_output(s$planner, o, label = "Demog", population = "SAF")
  s <- suppressMessages(save_study(s, home = home))
  suppressMessages(fetch_ard(s, "T-DM", home = home))
  ard <- ard_raw(s, "T-DM", home = home)
  skip_if(is.null(ard), "the report's ARD was not made")
  # (a study standard as tflspec's spec-yaml study says it: plan_cells'
  # categorical, plan_col_header's lines -- demog's study references)
  st <- tflspec::tfl_read_study_spec(system.file("extdata", "spec-yaml", package = "tflspec"))
  s$planner$spec$study$defaults$content <- st$defaults$content
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
  expect_true(all(c("F", "M") %in% txt))
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
