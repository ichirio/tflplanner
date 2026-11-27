# app_server(): add report -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
# A report's analysis set as its file says it (A2-T, canon 11-15: the
# `population:` of its data -- a data map's key, a template's set argument,
# the set its subjects rows carry -- tflspec::tfl_report_set(); `study` the
# study's spec, whose copies of the templates it reads); NA none
.o_population <- function(o, study = NULL) {
  if (!is.list(o)) return(NA_character_)
  # (a report the screens draw half made: no set rather than an error)
  v <- tryCatch(as.character(tflspec::tfl_report_set(o, study))[1L], error = function(e) NA_character_)
  if (is.na(v) || !nzchar(v)) NA_character_ else v
}
# A report's analysis set set (NA: taken out) where tfl_report_set() reads
# it, never in `common:` (tflspec::tfl_set_report_set(); `study` the
# study's spec, for a template that fills its set from no argument)
.o_set_population <- function(o, population, study = NULL) {
  has <- length(population) && !is.na(population[1L]) && nzchar(population[1L])
  # (a figure with no data yet: its set on a data map made for it, canon 11-15)
  if (has && identical(o$kind, "figure") && !length(o$data)) o$data <- list(population = population[1L])
  o <- tflspec::tfl_set_report_set(o, if (has) population[1L] else NA_character_, study)
  # (a data map's set first in it, as the YAML reads it; a map left empty and
  # the `common:` left empty taken out)
  d <- o$data
  if (is.list(d) && !is.null(names(d)) && "population" %in% names(d)) {
    o$data <- d[c("population", setdiff(names(d), "population"))]
  }
  if (is.list(o$data) && !length(o$data)) o$data <- NULL
  if (is.list(o$common) && !length(o$common)) o$common <- NULL
  o
}
.srv_add_report <- function() {
  shiny::observeEvent(input$add_ok, {
    id <- .id_with_prefix(input$modal_id, input$modal_type)
    # (a new report until the study is saved: the usual cards, empty -- #440)
    bnew$ids <- unique(c(bnew$ids, id))
    d <- trimws(input$modal_desc)
    desc <- if (nzchar(d)) d else NA
    if (identical(input$modal_how, "template")) return(add_template_report(id, desc))
    sec <- trimws(input$modal_section %||% "")
    p2 <- guarded(add_output(rv$p, id, description = desc,
                             type = input$modal_type,
                             section = if (nzchar(sec)) sec else NA_character_))
    pop <- input$modal_pop %||% ""
    if (!is.null(p2) && nzchar(pop)) {
      p2 <- guarded(set_report_population(p2, id, pop))
      if (!is.null(p2)) attr(p2, "made") <- attr(p2, "left") <- NULL
    }
    after_id_change(p2, id)
    # the new report's own tab
    if (!is.null(p2)) go(switch(input$modal_type,
      table = "tables", listing = "lf", figure = "designer", "outputs"))
  })
  # -- template reports (R/template_report.R, #440) ---------------------------
  tpl_data <- function() .tpl_data_cols(rv$study$path, rv$p)
  tpl_all_cols <- function() unique(unlist(lapply(tpl_data(), names)))
  # an argument's heading: "meaning (name)", its doc in the (i)
  # (.tpl_arg_heading(): .tpl_arg_labels; the doc itself when it has none)
  tpl_label <- function(prm, key = NULL) {
    if (identical(prm$name, "population") && isTRUE(prm$hidden)) {
      prm$doc <- "the analysis set's id (SAF)"
      prm$doc_ja <- NULL
    }
    doc <- prm$doc %||% ""
    short <- if (doc %in% names(.tpl_doc_labels)) .tpl_doc_labels[[doc]] else .tpl_arg_labels[prm$name]
    lab <- .tpl_arg_heading(prm, t)
    # (the help: tflspec's Japanese when it gives one, .tpl_doc_shown())
    help <- .tpl_doc_shown(prm, t, lang)
    shiny::span(lab, if (isTRUE(prm$required)) shiny::span(class = "text-danger", " *"),
                if (!is.null(key)) help_tip(.tpl_info_text(help, key, t)) else
                  if (!is.na(short) && nzchar(help)) help_tip(help))
  }
  # the input of an argument, by its type (.tpl_type(): tflspec's
  # tfl_param(type = ), guessed only when it declares none).  `params`,
  # `args`: the template's parameters and the report's arguments (a levels
  # parameter's variable is another's value).  `say_first`: the dialog's, whose
  # columns field tells the server its rows from the start.  `tpl`: the
  # template's name, for the suggestion of a field that has no value and no
  # default (its placeholder and the top of its choices; never filled in)
  # the suggestion for a template's argument, from the dialog's data (none: character());
  # `args$data`: the dataset it reads, when not the template's default
  tpl_suggestion <- function(tpl, arg, params = list(), args = list()) {
    .tpl_suggest_for(tpl, arg, tpl_data(), args, if (length(params)) params else .tpl_params(tpl),
                     study = rv$p$spec$study)
  }
  # the dataset chosen in the dialog's `data` field (NULL: the template's default)
  tpl_cur_data <- function(nm) {
    v <- input[[tpl_arg_id(nm, "data")]]
    if (is.character(v) && length(v) == 1L && nzchar(v)) v
  }
  tpl_widget <- function(input_id, prm, value, dataset = NULL, params = list(), args = list(),
                         label = tpl_label(prm), say_first = FALSE, tpl = NULL,
                         none = FALSE, rows = NULL, name = tpl) {
    data <- tpl_data()
    type <- .tpl_type(prm, tpl_all_cols())
    cols <- .tpl_columns(data, dataset %||% .tpl_vars_dataset(prm, data))
    shown <- if (is.null(value)) prm$default else value
    # (`!unset`: the "(none)" switch on; `none`, a study reference with no
    # value, or the unset: the placeholder says "(none)")
    unset <- .is_unset(shown)
    if (unset) shown <- NULL
    none <- isTRUE(none) || unset
    lab <- label
    # (a study reference with no value: its light placeholder says "(none)")
    ph <- function(x) if (none) t("(none)") else t(x)
    # (a value taken away with x: the list closed again)
    shut <- I("function() { this.close(); this.blur(); }")
    rm_btn <- list(plugins = list("remove_button"), onItemRemove = shut)
    sugg <- if (!is.null(tpl) && type %in% c("vars", "var", "chips", "levels") && .tpl_blank(shown)) {
      tpl_suggestion(tpl, prm$name, params,
                     if (is.null(args$data) && !is.null(dataset)) c(args, list(data = dataset)) else args)
    } else character()
    w <- switch(type,
      population = shiny::selectInput(input_id, lab, pop_choices(),
                                      selected = as.character(shown %||% ""), width = "100%"),
      data = shiny::selectInput(
        input_id, lab,
        c(if (is.null(shown)) stats::setNames("", ph("(default)")),
          union(as.character(shown %||% character()), names(data))),
        selected = as.character(shown %||% "")[1L], width = "100%"),
      levels = {
        vr <- .tpl_levels_of(prm, params, args)
        lv <- .tpl_levels(data, vr, dataset)
        several <- isTRUE(prm$multiple)
        if (length(sugg)) .tpl_suggest_select(
          input_id, lab, c(if (!several) stats::setNames("", ph("(not chosen)")), lv), sugg, several,
          c(if (several) rm_btn, list(create = TRUE)), t) else
        shiny::selectizeInput(
          input_id, lab,
          c(if (!several) stats::setNames("", ph("(not chosen)")),
            union(as.character(shown %||% character()), lv)),
          selected = as.character(shown %||% if (several) character() else ""),
          multiple = several, width = "100%",
          options = c(if (several) rm_btn, list(create = TRUE)))
      },
      flag = bslib::input_switch(input_id, lab, value = isTRUE(shown), width = "100%"),
      number = shiny::numericInput(input_id, lab, value = .num_or_null(shown), width = "100%", updateOn = "blur"),
      vars = if (length(sugg)) .tpl_suggest_select(
        input_id, lab, cols, sugg, TRUE,
        list(plugins = list("remove_button", "drag_drop"), onItemRemove = shut), t) else
        shiny::selectizeInput(input_id, lab, union(as.character(shown), cols),
                                   selected = as.character(shown), multiple = TRUE,
                                   width = "100%",
                                   options = list(plugins = list("remove_button", "drag_drop"),
                                                  onItemRemove = shut)),
      var = if (length(sugg)) .tpl_suggest_select(input_id, lab, c("", cols), sugg, FALSE, list(), t) else
        shiny::selectizeInput(input_id, lab, c("", union(as.character(shown %||% character()), cols)),
                                  selected = as.character(shown %||% ""), width = "100%"),
      chips = if (length(sugg)) .tpl_suggest_select(
        input_id, lab, character(), sugg, TRUE, c(rm_btn, list(create = TRUE)), t) else
        shiny::selectizeInput(
        input_id, lab, as.character(shown), selected = as.character(shown), multiple = TRUE,
        width = "100%", options = c(rm_btn, list(create = TRUE))),
      choice = shiny::selectInput(
        input_id, lab, c(if (is.null(shown)) stats::setNames("", ph("(default)")),
                         union(as.character(shown %||% character()), as.character(prm$choices))),
        selected = as.character(shown %||% "")[1L], width = "100%"),
      choices = shiny::selectizeInput(
        input_id, lab, union(as.character(shown), as.character(prm$choices)),
        selected = as.character(shown), multiple = TRUE, width = "100%", options = rm_btn),
      # (a pick from the def's source words: report_data, study_data, ...;
      # a value it does not offer kept, and one typed where the def allows)
      pick = ,
      picks = {
        several <- identical(type, "picks")
        ctx <- .tpl_table_ctx(data, dataset, args, params, populations = pop_choices(), rows = rows)
        ch <- tryCatch(.choice_values(prm$sources, ctx, tflplanner_language(), values = prm$choices),
                       error = function(e) character())
        cur <- as.character(shown %||% character())
        miss <- setdiff(cur, unlist(ch))
        if (length(miss)) ch <- if (is.list(ch)) c(as.list(stats::setNames(miss, miss)), ch) else c(miss, ch)
        shiny::selectizeInput(
          input_id, lab,
          if (several) ch else c(stats::setNames("", ph(if (is.null(prm$default)) "(not chosen)" else "(default)")),
                                 ch),
          selected = if (several) cur else if (length(cur)) cur[1L] else "",
          multiple = several, width = "100%",
          options = c(if (several) rm_btn, list(create = !isFALSE(prm$free))))
      },
      # (a listing's columns: the data's, in order, each its heading and
      # width -- the template's `data` field beside it, by its id)
      columns = .tpl_columns_widget(input_id, lab, shown, data,
                                    data_id = sub("__columns$", "__data", input_id),
                                    dataset = args$data %||% params$data$default %||% dataset, t = t,
                                    say_first = say_first),
      text = {
        txt <- if (is.null(shown)) "" else .tpl_show(shown)
        # (a text of several lines -- a heading "{col}\nN={n}": a box that
        # keeps its line breaks; a one-line field would drop them)
        if (grepl("\n", txt, fixed = TRUE)) {
          shiny::textAreaInput(input_id, lab, value = txt, width = "100%", updateOn = "blur",
                               rows = min(6L, length(strsplit(txt, "\n", fixed = TRUE)[[1L]]) + 1L))
        } else {
          shiny::textInput(input_id, lab, value = txt, width = "100%", updateOn = "blur",
                           placeholder = if (none) t("(none)") else if (is.null(prm$default)) t("(default)"))
        }
      },
      # (an R condition: the builder's R field, the dataset's columns as it is typed)
      r = .tpl_r_field(input_id, lab, shown, cols,
                       placeholder = if (is.null(prm$default)) t("(default)")),
      # (a def's table, a map or rows: the table field -- a row a line, the
      # template's own rows read only on top; its value the report's rows)
      table = {
        ctx <- .tpl_table_ctx(data, dataset, args, params, rows = rows)
        spec <- .tpl_table_spec(prm, ctx, shown, tflplanner_language())
        # (a !series default: the series' value shown light, as candidates --
        # only an entry typed over is the report's, canon 1.39)
        cand <- if (!unset && !is.null(name)) .tpl_series_value(rv$p$spec$study, name, prm)
        shiny::div(class = "form-group shiny-input-container w-100",
                   shiny::tags$label(class = "control-label", `for` = input_id, lab),
                   .field_table(input_id, spec, shown, fixed_rows = prm$fixed_rows, lang = tflplanner_language(),
                                candidates = cand))
      },
      list = {
        txt <- .tpl_list_text(shown, prm)
        if (is.null(txt)) {
          # (a value the rows cannot show -- a map of per-row formats, `!r` guards:
          # its YAML, edited in the box)
          ytxt <- .tpl_yaml_block(shown)
          ta <- shiny::textAreaInput(
            input_id, with_tip(lab, t("YAML: a `name: value` line each; `!r code` is raw R")),
            value = ytxt, width = "100%", updateOn = "blur",
            rows = max(3L, min(15L, length(strsplit(ytxt, "\n")[[1L]]) + 1L)))
          htmltools::tagQuery(ta)$find("textarea")$addClass("font-monospace")$allTags()
        } else {
          shiny::div(
            shiny::textAreaInput(
              input_id, with_tip(lab, sprintf(t("A line a row: %s"), paste(.tpl_list_fields(prm), collapse = " | "))),
              value = txt, width = "100%", updateOn = "blur",
              rows = max(2L, min(8L, length(strsplit(txt, "\n")[[1L]]) + 1L)),
              placeholder = if (none) t("(none)") else paste(.tpl_list_fields(prm), collapse = " | ")))
        }
      },
      # (a sequence of plain values -- a heading's lines, sort keys, breaks:
      # a value a line, read by .tpl_lines_parse())
      lines = {
        txt <- .tpl_lines_text(shown)
        shiny::textAreaInput(
          input_id, lab, value = txt, width = "100%", updateOn = "blur",
          rows = max(2L, min(8L, length(strsplit(txt, "\n", fixed = TRUE)[[1L]]) + 1L)),
          placeholder = if (none) t("(none)") else t("(default)"))
      })
    .tpl_none_wrap(w, input_id, type, prm, unset, t)
  }
  # The new report's dialog: the templates of each kind, a searchable list
  # with a line on each; the required arguments of each template, below it
  # -- all drawn with the dialog (shown by the choice in the page itself:
  # nothing is waited for from the server)
  tpl_input <- function(kind) paste0("modal_tpl_", kind)
  # (a series template's name has a "/": an id of letters, digits and _)
  tpl_arg_id <- function(nm, a) paste0("mtpl_", .tpl_id_part(nm), "__", .tpl_id_part(a))
  tpl_chosen <- function() {
    kind <- input$modal_type %||% "table"
    nm <- input[[tpl_input(kind)]]
    if (is.null(nm) || !nzchar(nm)) NULL else nm
  }
  tpl_modal_ui <- function() {
    shiny::tagList(lapply(report_types(), function(kind) {
      tp <- .tpl_list(kind)
      ser <- .tpl_series_templates()
      shiny::conditionalPanel(
        sprintf("input.modal_type == '%s'", kind),
        if (!nrow(tp)) {
          shiny::div(class = "alert alert-secondary py-1 small",
                     t("No template of this kind: choose Code."))
        } else shiny::tagList(
          shiny::selectizeInput(
            tpl_input(kind), t("Template"),
            stats::setNames(tp$name, vapply(tp$name, function(n) {
              d <- .tpl_desc_of(n, ser)
              if (nzchar(d)) paste0(n, " \u2014 ", t(d)) else n
            }, "")),
            selected = tp$name[1L], width = "100%",
            options = list(searchField = list("label", "value"))),
          lapply(tp$name, function(nm) {
            prm_all <- .tpl_params(nm)
            prm <- .tpl_shown(prm_all)
            req_p <- Filter(function(x) isTRUE(x$required), .tpl_dialog_shown(prm_all))
            # (the report's analysis set: one control for every template,
            # asked first -- in the box here, in the data block of a KM type's)
            if (!.tpl_own_data(prm_all)) req_p <- c(list(.tpl_pop_param(prm_all)), req_p)
            shiny::conditionalPanel(
              sprintf("input.modal_type == '%s' && input['%s'] == '%s'", kind, tpl_input(kind), nm),
              if (.tpl_own_data(prm_all)) tpl_modal_data_ui(nm),
              if (!length(req_p)) {
                shiny::p(class = "small text-muted",
                         t("No argument is required: every one has its default."))
              } else shiny::div(
                class = "border rounded p-2 mb-2",
                shiny::h6(t("Required arguments")),
                # (a template with a `data` argument: drawn again when it changes)
                if (is.null(prm$data)) tpl_req_widgets(nm) else
                  shiny::uiOutput(tpl_arg_id(nm, ".req_box"))))
          })))
    }),
    # the chosen template's other arguments, with their defaults (drawn for
    # it alone: a template has a dozen)
    shiny::uiOutput("tpl_modal_more"))
  }
  # the dialog's required arguments of a template, for the dataset `ds` (NULL:
  # the template's default); the study's value first: its treatment for `by`
  tpl_req_widgets <- function(nm, ds = NULL) {
    prm_all <- .tpl_params(nm)
    prm <- .tpl_shown(prm_all)
    req_p <- Filter(function(x) isTRUE(x$required), .tpl_dialog_shown(prm_all))
    shiny::tagList(
      # (the report's analysis set, asked first, as in the static list above;
      # a template's `population` argument is no field of its own)
      if (!.tpl_own_data(prm_all)) tpl_pop_widget(nm),
      lapply(req_p, function(x) {
        tpl_widget(tpl_arg_id(nm, x$name), x, .tpl_study_value(rv$p$spec$study, x),
                   dataset = ds %||% prm$data$default, params = prm, say_first = TRUE, tpl = nm,
                   rows = .tpl_data_rows(nm, params = prm_all))
      }))
  }
  # The report's analysis set in the dialog: one control for every template
  # that asks it (.tpl_asks_report_pop(): every one) -- the study's
  # populations only, its start the study's value else the template's
  # default.  ADSL's Y/N flags no population says are told in its (i), not
  # as lines under it: a flag that is an analysis set is registered in the
  # study first (#868)
  tpl_pop_widget <- function(nm) {
    x <- .tpl_pop_param(.tpl_params(nm))
    # ("(not chosen)" is its none: no "(none: not passed)" item beside it)
    x$unset <- "stop"
    lab <- shiny::span(
      t("This report's population"),
      if (isTRUE(x$required)) shiny::span(class = "text-danger", " *"),
      help_tip(c(
        t("The report's analysis set: one of the study's populations (spec/data.yml populations). The report's subjects are its subjects."),
        tpl_flags_text())))
    tpl_widget(tpl_arg_id(nm, "population"), x, .tpl_study_value(rv$p$spec$study, x),
               label = lab, say_first = TRUE, tpl = nm)
  }
  # ADSL's Y/N flags no study population says, told in the (?) of a report's
  # set control (the Create dialog's, the data step's), not as lines under
  # it: not offered -- a flag that is an analysis set is registered in the
  # study first (#868, codex review 9).  NULL when there are none.
  tpl_flags_text <- function() {
    fl <- .tpl_flags_undefined(tpl_data()[["ADSL"]], rv$p$ard$populations)
    if (!length(fl)) return(NULL)
    sprintf(t("ADSL's Y/N flags %s are not study populations: a flag that is a real analysis set is registered as a study population (spec/data.yml populations) first, then chosen here."),
            paste(fl, collapse = ", "))
  }
  # (... drawn again, for a template with a `data` argument, when the dataset in
  # it changes: its columns and suggestions are the new dataset's; one output a
  # template, bound the first time it is chosen)
  tpl_req_bound <- new.env()
  shiny::observeEvent(tpl_chosen(), {
    nm <- tpl_chosen()
    if (is.null(.tpl_shown(.tpl_params(nm))$data) || exists(nm, tpl_req_bound, inherits = FALSE)) return()
    assign(nm, TRUE, envir = tpl_req_bound)
    box <- tpl_arg_id(nm, ".req_box")
    output[[box]] <- shiny::renderUI({
      ds <- tpl_cur_data(nm)
      shiny::isolate(tpl_req_widgets(nm, ds))
    })
    shiny::outputOptions(output, box, suspendWhenHidden = FALSE)
  })
  # (an optional argument's value as the dialog starts it: the study's,
  # else the template's default)
  tpl_opt_start <- function(x) .tpl_study_value(rv$p$spec$study, x) %||% x$default
  output$tpl_modal_more <- shiny::renderUI({
    nm <- tpl_chosen()
    shiny::req(nm)
    prm <- .tpl_shown(.tpl_params(nm))
    opt <- Filter(function(x) !isTRUE(x$required), .tpl_dialog_shown(.tpl_params(nm)))
    if (!length(opt)) return(NULL)
    # (the key arguments right under the required ones, open, with their marker;
    # the folded group holds the rest)
    top <- Filter(.tpl_is_key, opt)
    opt <- opt[setdiff(names(opt), names(top))]
    drows <- .tpl_data_rows(nm, params = .tpl_params(nm))
    draw <- function(x, label = tpl_label(x)) tpl_widget(
      tpl_arg_id(nm, x$name), x, tpl_opt_start(x), dataset = prm$data$default, params = prm,
      label = label, say_first = TRUE, tpl = nm, rows = drows)
    key_ui <- lapply(top, function(x) draw(x, shiny::span(tpl_label(x), " ", .tpl_key_tag(x, t))))
    if (!length(opt)) return(shiny::tagList(key_ui))
    shiny::tagList(key_ui, shiny::tags$details(
      class = "border rounded p-2 mb-2",
      shiny::tags$summary(class = "small fw-semibold",
                          with_tip(sprintf(t("More arguments (%d): filters, variables, columns ..."), length(opt)),
                                   t("Each starts at its default; only a value you change is written into the report."))),
      lapply(opt, draw)))
  })
  # (bound while the dialog fades in, it counts as hidden: drawn anyway)
  shiny::outputOptions(output, "tpl_modal_more", suspendWhenHidden = FALSE)
  # a template whose data stage is the report's (a KM type): the report's
  # data asked in the dialog -- its analysis set (the control a table's
  # report set has, tpl_pop_widget()), the dataset, its parameter --
  # written as its `data:`: the set its `population:`, the rest its steps
  tpl_modal_data_ui <- function(nm) {
    d <- tpl_data()
    ds_all <- sort(unique(c(names(d), toupper(stats::na.omit(rv$p$ard$datasets$dataset)))))
    first <- if ("ADTTE" %in% ds_all) "ADTTE" else ""
    pid <- tpl_arg_id(nm, ".data_param")
    did <- tpl_arg_id(nm, ".data_ds")
    if (is.null(tpl_obs[[did]])) {
      tpl_obs[[did]] <- TRUE
      shiny::observeEvent(input[[did]], {
        pcd <- tpl_data()[[toupper(input[[did]] %||% "")]]$PARAMCD
        pcd <- if (is.null(pcd)) character() else sort(unique(as.character(stats::na.omit(pcd))))
        shiny::updateSelectizeInput(session, pid, choices = c(stats::setNames("", t("(not chosen)")), pcd),
                                    selected = "")
      }, ignoreInit = TRUE)
    }
    pcd <- d[[first]]$PARAMCD
    pcd <- if (is.null(pcd)) character() else sort(unique(as.character(stats::na.omit(pcd))))
    shiny::div(
      class = "border rounded p-2 mb-2",
      shiny::h6(t("Data (the report's data: steps)")),
      if (.tpl_asks_report_pop(.tpl_params(nm))) tpl_pop_widget(nm),
      shiny::div(
        class = "d-flex flex-wrap gap-2",
        shiny::div(style = "flex: 1 1 140px",
                   shiny::selectizeInput(did, t("Dataset (read)"), c("", ds_all), selected = first,
                                         width = "100%", options = list(create = TRUE))),
        shiny::div(style = "flex: 1 1 140px",
                   shiny::selectizeInput(pid, t("Parameter (param)"),
                                         c(stats::setNames("", t("(not chosen)")), pcd),
                                         width = "100%", options = list(create = TRUE)))))
  }
  # the dialog's data as `data:` steps (NULL: not a template of the report's
  # data; `missing`: what is still to say).  Its analysis set is no step:
  # the report's `population:` (tpl_modal_args()), whose subjects tflspec
  # keeps itself
  tpl_modal_data <- function(nm) {
    if (!.tpl_own_data(.tpl_params(nm))) return(NULL)
    ds <- toupper(trimws(input[[tpl_arg_id(nm, ".data_ds")]] %||% ""))
    par <- trimws(input[[tpl_arg_id(nm, ".data_param")]] %||% "")
    if (!nzchar(ds)) return(structure(list(), missing = t("Dataset (read)")))
    if (ds != "ADSL" && !nzchar(par)) return(structure(list(), missing = t("Parameter (param)")))
    list(steps = .tpl_steps_from(ds, NULL, par))
  }
  # what is in an argument's field (a picked "Add all suggested" is the suggestion)
  tpl_picked <- function(nm, x) {
    v <- input[[tpl_arg_id(nm, x$name)]]
    if (.tpl_suggest_all %in% v) {
      ds <- tpl_cur_data(nm)
      v <- .tpl_suggest_expand(v, tpl_suggestion(nm, x$name, .tpl_shown(.tpl_params(nm)),
                                                 if (!is.null(ds)) list(data = ds) else list()))
    }
    v
  }
  # the required arguments the dialog has (NULL: one is empty)
  # (its fields read as the card reads them: .tpl_dialog_args(), .tpl_field_arg())
  tpl_modal_args <- function(nm) {
    prm_all <- .tpl_params(nm)
    raws <- list()
    nones <- list()
    if (.tpl_asks_report_pop(prm_all)) raws["population"] <- list(input[[tpl_arg_id(nm, "population")]])
    for (x in .tpl_dialog_shown(prm_all)) {
      raws[x$name] <- list(tpl_picked(nm, x))
      nones[x$name] <- list(input[[paste0(tpl_arg_id(nm, x$name), "__none")]])
    }
    args <- .tpl_dialog_args(prm_all, raws, rv$p$spec$study, nones)
    if (!is.null(attr(args, "missing"))) return(args)
    dat <- tpl_modal_data(nm)
    if (!is.null(attr(dat, "missing"))) return(structure(list(), missing = attr(dat, "missing")))
    args
  }
  # Create: not while a required argument is empty
  output$modal_ok_js <- shiny::renderUI({
    how <- input$modal_how
    if (is.null(how)) return(NULL)
    ok <- nzchar(trimws(input$modal_id %||% ""))
    if (ok && identical(how, "template")) {
      nm <- tpl_chosen()
      ok <- !is.null(nm) && is.null(attr(tpl_modal_args(nm), "missing"))
    }
    shiny::tags$script(shiny::HTML(sprintf(
      "setTimeout(function() { $('#add_ok').prop('disabled', %s); }, 0);",
      if (ok) "false" else "true")))
  })
  # the report head's mark of the template: "Template: demog (summary)";
  # a variant "demog_jp (a variant of demog)" when tflspec tells the base
  tpl_head_link <- function(id) {
    tp <- .tpl_of(rv$p, id)
    if (is.null(tp)) return(NULL)
    info <- tryCatch(tflspec::tfl_templates(), error = function(e) NULL)
    row <- if (!is.null(info)) info[info$name == tp$name, , drop = FALSE]
    base <- if (!is.null(row) && nrow(row) && "base" %in% names(row)) row$base[1L] else NA
    extra <- if (length(base) && !is.na(base) && nzchar(base)) sprintf(t("a variant of %s"), base) else
      if (!is.null(row) && nrow(row) && "layout" %in% names(row)) row$layout[1L] else NA
    shiny::tags$a(
      href = "#", class = "small",
      onclick = "Shiny.setInputValue('tpl_goto_card', Math.random(), {priority: 'event'}); return false;",
      paste0(t("Template"), ": ", tp$name, if (!is.na(extra)) paste0(" (", extra, ")")))
  }
  # a template report made a code one: the template's items, with the
  # study's defaults and the report's own over them, written into the
  # report (tflspec::tfl_template_to_spec()); one way, asked first
  shiny::observeEvent(input$tpl_to_code, {
    id <- current()
    shiny::req(id, !is.null(.tpl_of(rv$p, id)))
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Convert %s to code?"), id),
      shiny::p(t("The template's items are written into the report, with the study's defaults and this report's settings, and edited on the usual cards from then on.")),
      shiny::div(class = "alert alert-warning py-2 small mb-0",
                 t("This cannot be undone: later changes of the template no longer reach this report.")),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("tpl_to_code_ok", t("Convert"), class = "btn-primary")),
      easyClose = TRUE))
  })
  shiny::observeEvent(input$tpl_to_code_ok, {
    shiny::removeModal()
    id <- current()
    shiny::req(id)
    p2 <- guarded(.tpl_to_code(rv$p, id))
    if (is.null(p2)) return()
    rv$p <- p2
    bump()
    tpl_mode_send(FALSE, report_kind(), id = id)
    # (its cards drawn at once: a table's preview makes the ARD they read)
    if (identical(report_kind(), "table")) do_preview(id)
    notify(sprintf(t("%s is a code report now: its items are on the usual cards (save to keep it)."), id))
  })
  shiny::observeEvent(input$tpl_goto_card, {
    go(switch(report_kind(), listing = "lf", figure = "designer", "tables"))
    session$onFlushed(function() session$sendCustomMessage("rp-tpl-card-show", TRUE), once = TRUE)
  })
  # the page told at once that a report is being opened, and whether it is
  # a template report (.tpl_js): its steps hidden until drawn
  # (`ready`: "Loading ..." taken away after this flush; a report being
  # added waits for its own -- tpl_pending)
  tpl_pending <- new.env()
  tpl_mode_send <- function(tpl, kind = "", loading = TRUE, ready = loading, id = NULL) {
    session$sendCustomMessage("rp-tpl-mode", list(tpl = isTRUE(tpl), loading = loading, id = id))
    if (loading && !isTRUE(shiny::isolate(review_hold()))) review_hold(TRUE)
    if (ready) tpl_ready_send()
  }
  # "Loading ..." taken away once the flush now running is done; a
  # template report's when its card is drawn (tpl_ready$id)
  tpl_ready <- new.env()
  # a new template report: its preview (ARD and table) made once its card
  # is on the page -- never before it
  tpl_autoprev <- new.env()
  tpl_autoprev_go <- shiny::reactiveVal(NULL)
  shiny::observeEvent(tpl_autoprev_go(), {
    id <- tpl_autoprev_go()
    tp <- .tpl_of(rv$p, id %||% "")
    if (identical(id, current()) && !is.null(tp) && !length(.tpl_missing_report(rv$p, id))) {
      do_preview(id)
    }
  })
  tpl_ready_send <- function() {
    session$onFlushed(function() {
      if (session$isClosed()) return()
      session$sendCustomMessage("rp-report-ready", TRUE)
      review_last$held <- NULL
      review_hold(FALSE)
    }, once = TRUE)
  }
  shiny::observeEvent(current(), {
    id <- current()
    # (a report being added: the page waits for it, whatever comes first)
    if (!is.null(tpl_pending$id)) {
      if (!identical(id, tpl_pending$id)) return()
      tpl_pending$id <- NULL
      .tpl_trace("the report chosen")
      session$onFlushed(function() .tpl_trace("drawn (flush done)"), once = TRUE)
    }
    p <- shiny::isolate(rv$p)
    tpl <- !is.null(id) && !is.null(.tpl_of(p, id))
    kind <- if (is.null(id)) "" else report_info(p, id)$type
    tpl_ready$id <- if (tpl) id
    tpl_mode_send(tpl, kind, loading = !is.null(id), ready = !is.null(id) && !tpl, id = id)
  }, ignoreNULL = FALSE, priority = 99)
  # A series template copied into the open study (tflspec's
  # tfl_copy_templates(); said when it is a new copy): FALSE when it
  # could not be, tflspec's error said
  tpl_copy <- function(name) {
    msg <- tryCatch(.tpl_copy_into_study(rv$study$path, name, t), error = function(e) {
      notify(conditionMessage(e), "error")
      FALSE
    })
    if (isFALSE(msg)) return(FALSE)
    if (!is.null(msg)) {
      notify(msg, "message")
      tpl_copies_ver(tpl_copies_ver() + 1L)
    }
    TRUE
  }
  add_template_report <- function(id, desc) {
    .tpl_trace("OK", start = TRUE)
    # (not made: the page's "Loading ..." taken away again)
    fail <- function(msg, type) {
      session$sendCustomMessage("rp-report-ready", TRUE)
      if (!is.null(msg)) notify(msg, type)
    }
    nm <- tpl_chosen()
    if (is.null(nm)) return(fail(t("Choose a template"), "warning"))
    args <- tpl_modal_args(nm)
    if (!is.null(attr(args, "missing"))) {
      return(fail(sprintf(t("Fill in the required argument %s."), attr(args, "missing")), "warning"))
    }
    if (is.null(rv$p$spec)) {
      return(fail(t("A template report needs the study's YAML spec (save the study first)."), "error"))
    }
    # (the study reads only its own copy of a template: copied first)
    if (!tpl_copy(nm)) return(fail(NULL, NULL))
    kind <- input$modal_type
    sec <- trimws(input$modal_section %||% "")
    # (the page shows the new report's steps from its first draw)
    tpl_mode_send(TRUE, kind, ready = FALSE, id = id)
    tpl_pending$id <- id
    if (identical(kind, "table")) tpl_autoprev$want <- id
    # (a KM type: its data the report's own `data:` steps, from the dialog.
    # The analysis set -- the dialog's one set control, #868 -- is written by
    # the one setter, .write_report_population(), where the report's set
    # lives, never in `common:` (A2-T): a figure's on its data's
    # `population:` (no step of it, canon 11-15), a template's set argument,
    # and the template's `population` argument where it takes one, the same
    # value (H5).  A set the template's own default already is (demog's
    # SAF, canon 11-15 (1)) is not written: the template holds it)
    dat <- tpl_modal_data(nm)
    pop <- as.character(attr(args, "population") %||% NA_character_)
    attr(args, "population") <- NULL
    o <- tflspec::tfl_output(id, kind, label = if (is.na(desc)) NA_character_ else desc,
                             template = .tpl_write_args(list(name = nm), args, by_step = TRUE),
                             data = if (length(dat$steps)) list(steps = dat$steps))
    own <- tryCatch(tflspec::tfl_report_set(o, rv$p$spec$study), error = function(e) NA_character_)
    if (.is_blank(pop)) pop <- own
    write_set <- !.is_blank(pop) && !identical(pop, own)
    # (straight into the spec: its toc line and its file, the view of
    # that report alone -- .spec_add_output(); its set then written as every
    # report's is, .write_report_population())
    p2 <- guarded(.spec_add_output(rv$p, o, label = desc,
                                   section = if (nzchar(sec)) sec else NA_character_,
                                   population = if (.is_blank(pop)) NA_character_ else pop))
    if (!is.null(p2) && write_set) p2 <- guarded(.write_report_population(p2, id, pop, output = o))
    .tpl_trace("written into the spec")
    if (is.null(p2)) {
      tpl_pending$id <- NULL
      return(tpl_mode_send(!is.null(.tpl_of(rv$p, current() %||% "")), report_kind()))
    }
    after_id_change(p2, id)
    if (!is.null(p2)) go(switch(kind, table = "tables", listing = "lf", figure = "designer", "outputs"))
  }
  # The template's cards: the required arguments and those the report gives
  # above, the others (their defaults, greyed) folded below; each with
  # where its value comes from.  An argument is drawn once, in the step its
  # `step:` names (.tpl_arg_section): data (tpl_card_data), analysis
  # (tpl_card_ard), content (tpl_card, tpl_card_lf, tpl_card_fig) and page
  # (tpl_card_page).  The template itself -- its name, another one, Convert
  # to code -- is the report's, not a step's: one card above the steps
  # (tpl_head), seen from every step.
  tpl_ver <- shiny::reactiveVal(0L)
  # (a write the page shows already, tpl_write(redraw = FALSE): the card is
  # left as it is while this flush is drawn -- it stands for "drawn as it was",
  # which the page keeps, .tpl_js)
  tpl_held <- FALSE
  tpl_obs <- new.env()
  # (a table field's edit that changes what the card shows around it: the
  # report whose card is drawn again once the focus leaves the table)
  tpl_tbl_due <- NULL
  shiny::observeEvent(input$tpl_tbl_left, {
    due <- tpl_tbl_due
    tpl_tbl_due <<- NULL
    if (!is.null(due) && identical(due, current())) tpl_ver(tpl_ver() + 1L)
  })
  # (per report, template and argument: the values its writes replaced, and when)
  tpl_past <- new.env()
  tpl_iid <- function(name, a) paste0("tpl_", .tpl_id_part(name), "__", .tpl_id_part(a))
  # (`population`: the report's analysis set written too, after the steps --
  # on the data map they make, .o_set_population(); NULL leaves it.  The toc
  # line always follows the report's set as written, also when an argument
  # write moved it, so the list and the designer read the same set)
  tpl_write <- function(id, args, name = NULL, steps = NULL, redraw = TRUE, population = NULL) {
    o <- .tpl_output(rv$p, id, args, name = name)
    if (length(steps)) o$data$steps <- steps
    p2 <- guarded(.write_report_population(rv$p, id, population, output = o))
    if (is.null(p2)) return(invisible(FALSE))
    rv$p <- p2
    rv$bver <- rv$bver + 1L
    rv$status_ver <- rv$status_ver + 1L
    if (redraw) tpl_ver(tpl_ver() + 1L) else {
      tpl_held <<- TRUE
      session$onFlushed(function() tpl_held <<- FALSE, once = TRUE)
    }
    bump()
    invisible(TRUE)
  }
  tpl_edit <- function(name, a, v) {
    id <- current()
    tp <- .tpl_of(rv$p, id %||% "")
    if (is.null(tp) || !identical(tp$name, name)) return()
    prm <- .tpl_params(name)[[a]]
    type <- .tpl_type(prm, tpl_all_cols())
    # (what the card drew: an echo of it is no edit -- #751 item 2)
    shown <- .tpl_drawn_value(rv$p, id, tp$args, prm)
    # (a switch with no default -- a series template's, from the series -- is drawn
    # off: its first FALSE is what it showed, no edit)
    if (is.null(shown) && identical(type, "flag")) shown <- FALSE
    if (identical(type, "columns") && is.null(.tpl_list_text(shown, prm))) return()
    # (a list the rows cannot show: its YAML text, the text it was drawn with
    # no edit; not YAML, said and the value stays)
    yaml_box <- identical(type, "list") && is.null(.tpl_list_text(shown, prm))
    if (yaml_box && identical(.tpl_yaml_edit(v, shown)$kind, "same")) return()
    if (identical(type, "text") && is.character(v) && length(v) == 1L) {
      # (the text as it was shown: no edit; YAML that does not parse: said, and the value stays)
      sh <- .tpl_drawn_value(rv$p, id, tp$args, prm)
      if (!is.null(sh) && identical(trimws(v), .tpl_show(sh))) return()
      bad <- tryCatch({ .tpl_text_parse(trimws(v)); NULL }, error = function(e) conditionMessage(e))
      if (!is.null(bad)) {
        notify(sprintf(t("%s is not valid YAML, so it is not changed: %s"), a, bad), "error")
        tpl_ver(tpl_ver() + 1L)
        return()
      }
    }
    # (read as the dialog reads it: an empty field is the default, "(none)" !unset)
    val <- tryCatch(.tpl_field_arg(v, type, prm, shown, echo = FALSE), error = function(e) e)
    if (inherits(val, "error")) {
      notify(sprintf(t("%s is not valid YAML, so it is not changed: %s"), a, conditionMessage(val)), "error")
      tpl_ver(tpl_ver() + 1L)
      return()
    }
    # (a field drawn again says its value back: what it shows already --
    # the report's, the study's or the default -- is no edit)
    # (a switch drawn off for its "(none)" says FALSE when drawn: not an edit
    # unless the switch was moved by hand, .tpl_flag_off_drawn())
    if (.tpl_flag_off_drawn(type, val, shown, input[[paste0(tpl_iid(name, a), "__none")]])) return()
    if (.tpl_table_none_drawn(type, val, shown)) return()
    list_key <- function(z) .tpl_list_text(z, prm) %||% .tpl_yaml_block(z)
    same <- if (!is.na(su <- .same_unset(val, shown))) su else if (identical(type, "table")) {
      identical(.tpl_table_key(val, prm), .tpl_table_key(shown, prm))
    } else if (type %in% c("list", "columns")) {
      identical(list_key(val), list_key(shown))
    } else !is.null(val) && identical(as.character(unlist(val)), as.character(unlist(shown)))
    if (same) return()
    # (a field drawn from a card older than its last write says the value it
    # had then: not an edit -- drawn again, it shows the report's.  A person
    # who really types the old value again within the window sees the field
    # go back and types it once more)
    key <- paste(id, name, a)
    now <- as.numeric(Sys.time())
    as_key <- function(z) if (.is_unset(z)) .none_word else if (identical(type, "table")) .tpl_table_key(z, prm) else
      if (type %in% c("list", "columns")) list_key(z) else
      paste(as.character(unlist(z)), collapse = "\n")
    # (a table field is not drawn again while it is typed in: what it says is
    # what it shows -- a value typed and taken back again is an edit)
    if (!identical(type, "table") && .tpl_stale_say(as_key(val), tpl_past[[key]], now)) {
      tpl_ver(tpl_ver() + 1L)
      return()
    }
    args <- .tpl_set_arg(tp$args, prm, val)
    if (identical(args, tp$args)) return()
    # (the columns field shows its edit already, and a redraw takes the focus
    # from its table: the card is drawn again only when what it shows around
    # the field changes -- the field given or not, the required ones missing.
    # A table field's is put off until the focus leaves the table
    # (tpl_tbl_left): the first value typed into it is never cut off)
    around <- is.null(tp$args[[a]]) != is.null(args[[a]]) ||
      !identical(.tpl_missing(name, tp$args, rv$p$spec$study), .tpl_missing(name, args, rv$p$spec$study))
    redraw <- !type %in% c("columns", "table") || (around && !identical(type, "table"))
    # (the "(none)" button is outside the table: drawn again at once)
    if (identical(type, "table") && .is_unset(val)) redraw <- TRUE
    if (around && !redraw) tpl_tbl_due <<- id
    if (isTRUE(tpl_write(id, args, redraw = redraw))) {
      keep <- tpl_past[[key]] %||% data.frame(v = character(), t = numeric())
      keep <- keep[now - keep$t <= 60, , drop = FALSE]
      tpl_past[[key]] <- utils::tail(rbind(keep, data.frame(v = as_key(shown), t = now)), 5L)
    }
    # (a soft warning: the value is kept)
    msg <- if (!is.null(val) && a %in% .tpl_header_checked) .tpl_header_problem(val, a)
    if (!is.null(msg)) notify(t(msg), "warning")
  }
  tpl_observe <- function(name, prm) {
    for (a in names(prm)) {
      key <- paste(name, a)
      if (!is.null(tpl_obs[[key]])) next
      tpl_obs[[key]] <- TRUE
      local({
        a <- a
        iid <- tpl_iid(name, a)
        shiny::observeEvent(input[[iid]], tpl_edit(name, a, input[[iid]]),
                            ignoreInit = TRUE, ignoreNULL = FALSE)
        # (a switch's "(none)" button: its own input; the switch itself says no more than off)
        shiny::observeEvent(input[[paste0(iid, "__none")]],
                            if (isTRUE(input[[paste0(iid, "__none")]])) tpl_edit(name, a, .none_word),
                            ignoreInit = TRUE)
      })
    }
  }
  shiny::observeEvent(input$tpl_revert, {
    id <- current()
    tp <- .tpl_of(rv$p, id %||% "")
    a <- input$tpl_revert
    if (is.null(tp) || is.null(tp$args[[a]])) return()
    args <- tp$args
    args[[a]] <- NULL
    tpl_write(id, args)
  })
  shiny::observeEvent(input$tpl_preview, do_preview())
  # another template: the arguments of the same name kept, the others
  # dropped -- said by name before; the report's own items stay.  The
  # report's set is no argument kept or dropped: it stays the report's,
  # written by the one setter where the new template reads it (its data,
  # `common:`, and its `population` argument where it takes one --
  # .write_report_population(); review M12)
  shiny::observeEvent(input$tpl_switch, {
    id <- current()
    tp <- .tpl_of(rv$p, id %||% "")
    to <- input$tpl_switch
    if (is.null(tp) || !length(to) || !nzchar(to) || identical(to, tp$name)) return()
    to_prm <- .tpl_params(to)
    keep <- tpl_switch_keep(tp, to_prm)
    # (to a KM type: the data stage goes to the report's `data:` steps; an
    # argument that is not the set is dropped)
    moved <- if (.tpl_own_data(to_prm)) setdiff(intersect(names(tp$args), c("data", "param")), keep)
    set <- tpl_switch_set(tp, id)
    drop <- setdiff(names(tp$args), c(keep, moved, "population"))
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Change the template to %s"), to),
      shiny::p(t("Arguments kept:"), " ",
               if (length(keep)) shiny::code(paste(keep, collapse = ", ")) else t("(none)")),
      if (length(moved)) shiny::p(t("To the report's data: steps:"), " ",
                                  shiny::code(paste(moved, collapse = ", "))),
      if (!is.na(set)) shiny::p(sprintf(
        t("Analysis set %s: the report's, kept: the new template takes it where it reads the set."),
        set)),
      shiny::p(t("Arguments dropped:"), " ",
               if (length(drop)) shiny::code(paste(drop, collapse = ", ")) else t("(none)")),
      shiny::p(class = "small text-muted", t("The report's own cards are kept.")),
      footer = shiny::tagList(
        shiny::tags$button(type = "button", class = "btn btn-default", `data-bs-dismiss` = "modal",
                           onclick = "Shiny.setInputValue('tpl_switch_cancel', Date.now())",
                           t("Cancel")),
        .btn("tpl_switch_ok", "OK", class = "btn-primary")),
      easyClose = FALSE))
  })
  # (the arguments of the same name the new template shows, but the set:
  # the report's, not an argument to carry)
  tpl_switch_keep <- function(tp, to_prm) {
    setdiff(intersect(names(tp$args), names(.tpl_shown(to_prm))), "population")
  }
  # (the report's set, kept over the switch, whatever the two templates say
  # of it: as its file says it, .o_population() -- else, an older file's,
  # the old template's `population` argument; NA none.  The setter writes
  # it for the new template: on a figure's data (no join step, canon 11-15),
  # in a table's `common:`, and as its `population` argument where it takes
  # one)
  tpl_switch_set <- function(tp, id) {
    pid <- .o_population(rv$p$spec$outputs[[id]])
    if (is.na(pid)) pid <- as.character(tp$args$population %||% NA_character_)[1L]
    if (is.na(pid) || !nzchar(pid)) NA_character_ else pid
  }
  shiny::observeEvent(input$tpl_switch_cancel, tpl_ver(tpl_ver() + 1L))
  shiny::observeEvent(input$tpl_switch_ok, {
    shiny::removeModal()
    id <- current()
    tp <- .tpl_of(rv$p, id %||% "")
    to <- input$tpl_switch
    if (is.null(tp) || is.null(to)) return()
    if (!tpl_copy(to)) return(tpl_ver(tpl_ver() + 1L))
    to_prm <- .tpl_params(to)
    args <- tp$args[tpl_switch_keep(tp, to_prm)]
    # (to a KM type: the old template's data stage -- its dataset, its
    # parameter -- becomes the report's `data:` steps, written with the
    # template in one go; steps of the report's own that say a parameter
    # are kept.  The report's set: the setter's, tpl_write(population =))
    steps <- NULL
    set <- tpl_switch_set(tp, id)
    if (.tpl_own_data(to_prm)) {
      have <- rv$p$spec$outputs[[id]]$data$steps
      f <- .tpl_steps_fields(have)
      if (!length(have) || (f$fields && !nzchar(f$param) && !identical(toupper(f$dataset), "ADSL"))) {
        from <- .tpl_params(tp$name)
        ds <- as.character(tp$args$data %||% from$data$default %||% "")[1L]
        par <- as.character(tp$args$param %||% from$param$default %||% "")[1L]
        if (!nzchar(ds) || (!identical(toupper(ds), "ADSL") && !nzchar(par))) {
          tpl_ver(tpl_ver() + 1L)
          return(notify(sprintf(t("%s takes the report's data: fill in param (the PARAMCD) first."), to),
                        "warning"))
        }
        steps <- .tpl_steps_from(ds, NULL, par)
      }
    }
    tpl_write(id, args, name = to, steps = steps, population = if (!is.na(set)) set)
  })
  # the card drawn: "Loading ..." taken away when it is the report opened,
  # and a new template report's preview started after it
  tpl_card_drawn <- function(id) {
    .tpl_trace("argument card drawn")
    if (!identical(tpl_ready$id, id)) return(invisible())
    tpl_ready$id <- NULL
    tpl_ready_send()
    if (identical(tpl_autoprev$want, id)) {
      tpl_autoprev$want <- NULL
      session$onFlushed(function() if (!session$isClosed()) tpl_autoprev_go(id), once = TRUE)
    }
    invisible()
  }
  tpl_card_ui <- function() {
    id <- current()
    shiny::req(id)
    card <- tpl_card_draw(id, "content")
    tpl_card_drawn(id)
    card
  }
  # A template whose data stage is the report's (a KM type): the report's
  # `data:` steps -- the dataset, the parameter, a filter -- and its
  # analysis set (the data's `population:`, whose subjects the program keeps:
  # no join step of it), written by Match the analysis set.  Steps the
  # fields cannot say are kept and shown as they are.
  tpl_data_block <- function(id, steps) {
    f <- .tpl_steps_fields(steps)
    pop <- tpl_data_pop(id)
    d <- tpl_data()
    ds_all <- sort(unique(c(names(d), toupper(stats::na.omit(rv$p$ard$datasets$dataset)))))
    pcd <- d[[toupper(f$dataset)]]$PARAMCD
    pcd <- if (is.null(pcd)) character() else sort(unique(as.character(stats::na.omit(pcd))))
    head <- shiny::div(class = "fw-semibold small mb-1", t("Data (the report's data: steps)"))
    if (!f$fields) {
      return(shiny::div(
        class = "border rounded p-2 mb-2", head,
        shiny::p(class = "small text-muted mb-1",
                 t("Steps the fields cannot say: kept as they are; change them in the report's YAML.")),
        shiny::tags$pre(class = "small mb-0", yaml::as.yaml(list(data = list(steps = steps))))))
    }
    cell <- function(x) shiny::div(style = "flex: 1 1 140px; min-width: 0", x)
    shiny::div(
      class = "border rounded p-2 mb-2", head,
      shiny::div(
        class = "d-flex flex-wrap gap-2",
        cell(shiny::selectizeInput("tpl_data_ds", t("Dataset (read)"), choices = c("", ds_all),
                                   selected = f$dataset, width = "100%", options = list(create = TRUE))),
        cell(shiny::selectizeInput("tpl_data_param", t("Parameter (param)"),
                                   choices = unique(c("", f$param, pcd)), selected = f$param,
                                   width = "100%", options = list(create = TRUE)))),
      # (the analysis set, then the subset: CDISC ARS's AnalysisSet and
      # DataSubset)
      shiny::selectInput("tpl_data_pop", shiny::span(
                           t("Analysis set (population)"),
                           help_tip(c(t("The report's analysis set: one of the study's populations (spec/data.yml populations). The report's subjects are its subjects."),
                                      tpl_flags_text()))),
                         tpl_data_pop_choices(id, f),
                         selected = tpl_data_pop_sel(id, f), width = "100%"),
      shiny::textInput("tpl_data_filter", t("Record condition (filter)"), f$filter, width = "100%",
                       placeholder = t("an R condition on the records, e.g. AVAL > 0")),
      shiny::div(class = "small text-muted mb-1",
                 if (is.null(pop) && f$join) {
                   sprintf(t("Analysis set: the report's own join (%s: %s)."), f$join_dataset, f$join_where)
                 } else if (is.null(pop)) t("Analysis set: none (the report has no population).") else
                   sprintf(t("Analysis set: %s (%s: %s), the report's set: the program keeps its subjects."),
                           pop$population_id, pop$dataset, pop$where %||% "")),
      # (a report of the study's set with a join in its steps -- written
      # before the set kept its subjects itself: a copy of it, or, the set
      # changed since, another one -- said; Match takes it out, canon 11-15)
      if (!is.null(pop) && f$join) {
        if (.tpl_join_stale(f, pop)) shiny::div(
          class = "alert alert-warning py-1 small",
          sprintf(t("The steps' join (%s: %s) is not the analysis set's: Match the analysis set takes it out."),
                  f$join_dataset, f$join_where))
        else shiny::div(
          class = "small text-muted mb-1",
          t("The steps also have the analysis set's join, written before the report's set kept its subjects: Match the analysis set takes it out."))
      },
      # (an action, not a save: the data: steps written from these fields,
      # the analysis set the report's -- #754, canon 11-15)
      .btn("tpl_data_apply", t("Match the analysis set"), class = "btn-sm btn-outline-primary",
           title = t("Writes the report's data: steps from these fields, and the analysis set as the report's data: population (no join step: the program keeps the set's subjects).")),
      if (length(steps)) shiny::tags$details(
        class = "mt-1",
        shiny::tags$summary(class = "small", t("The steps written")),
        shiny::tags$pre(class = "small mb-0", yaml::as.yaml(list(data = list(steps = steps))))))
  }
  # the card's analysis-set choice: the report's population, else its own
  # join as it is (".own": a join its file has, on a flag or not), else none
  tpl_data_pop_sel <- function(id, f) {
    pid <- .o_population(rv$p$spec$outputs[[id]])
    if (!is.na(pid)) return(pid)
    if (f$join) ".own" else ""
  }
  tpl_data_pop_choices <- function(id, f) {
    # (the study's populations only, as the Create dialog offers them: a
    # flag that is an analysis set is registered in the study first --
    # tpl_flags_text(); #868, codex review 9.  The report's set kept a
    # choice when the populations sheet has it not, as step 1's)
    ch <- pop_choices(.o_population(rv$p$spec$outputs[[id]]))
    # (the report's own join, when it has no population of the study: its
    # file's, kept as it is -- no flag offered as a set)
    if (f$join && is.null(tpl_data_pop(id))) {
      ch <- c(ch, stats::setNames(".own", sprintf("%s (%s: %s)", t("The report's own join"),
                                                   f$join_dataset, f$join_where)))
    }
    ch
  }
  tpl_data_pop <- function(id) {
    pid <- .o_population(rv$p$spec$outputs[[id]])
    pops <- rv$p$ard$populations
    if (is.na(pid) || is.null(pops) || !nrow(pops)) return(NULL)
    k <- match(as.character(pid)[1L], pops$population_id)
    if (is.na(k)) return(NULL)
    as.list(pops[k, intersect(c("population_id", "dataset", "where"), names(pops)), drop = FALSE])
  }
  shiny::observeEvent(input$tpl_data_ds, {
    # (the parameter's choices: the dataset's PARAMCD values)
    pcd <- tpl_data()[[toupper(input$tpl_data_ds %||% "")]]$PARAMCD
    pcd <- if (is.null(pcd)) character() else sort(unique(as.character(stats::na.omit(pcd))))
    shiny::updateSelectizeInput(session, "tpl_data_param", choices = unique(c("", input$tpl_data_param, pcd)),
                                selected = input$tpl_data_param)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$tpl_data_apply, {
    id <- current()
    if (is.null(id) || is.null(.tpl_of(rv$p, id))) return()
    if (!nzchar(trimws(input$tpl_data_ds %||% ""))) return(notify(t("Choose the dataset."), "warning"))
    o <- rv$p$spec$outputs[[id]]
    f <- .tpl_steps_fields(o$data$steps)
    # the analysis set chosen: the study's -- the report's set, its data's
    # `population:`, whose subjects the program keeps (canon 11-15): no join
    # step of it, an old one taken out -- or the report's own join as its
    # file has it (".own"), kept as a join, or none
    sel <- input$tpl_data_pop %||% tpl_data_pop_sel(id, f)
    join <- NULL
    if (identical(sel, ".own")) {
      join <- list(dataset = f$join_dataset, where = f$join_where)
    }
    steps <- .tpl_steps_from(input$tpl_data_ds, join, input$tpl_data_param %||% "",
                             input$tpl_data_filter %||% "")
    if (is.null(steps)) o$data$steps <- NULL else o$data$steps <- steps
    # (the set written after the steps: on the data map they make --
    # .write_report_population())
    o <- .o_set_population(o, if (is.null(join) && nzchar(sel)) sel else NA_character_,
                            rv$p$spec$study)
    if (!length(o$data)) o$data <- NULL
    p2 <- guarded(.write_report_population(rv$p, id, output = o))
    if (is.null(p2)) return()
    rv$p <- p2
    rv$bver <- rv$bver + 1L
    rv$status_ver <- rv$status_ver + 1L
    tpl_ver(tpl_ver() + 1L)
    bump()
  })
  # the data step's analysis set of a template that hides it (demog): the
  # report's population, set as the report list sets it
  shiny::observeEvent(input$tpl_report_pop, {
    id <- current()
    tp <- .tpl_of(rv$p, id %||% "")
    if (is.null(tp) || !.tpl_pop_of_report(.tpl_params(tp$name))) return()
    o <- rv$p$spec$outputs[[id]]
    sel <- input$tpl_report_pop %||% ""
    now <- .o_population(o)
    if (identical(sel, if (is.na(now)) "" else now)) return()
    p2 <- guarded(.write_report_population(rv$p, id, if (nzchar(sel)) sel else NA_character_,
                                           output = o))
    if (is.null(p2)) return()
    rv$p <- p2
    rv$bver <- rv$bver + 1L
    rv$status_ver <- rv$status_ver + 1L
    tpl_ver(tpl_ver() + 1L)
    bump()
  }, ignoreInit = TRUE)
  tpl_card_draw <- function(id, part = c("content", "data", "ard", "page", "head")) {
    part <- match.arg(part)
    tpl_ver()
    rv$ver
    if (tpl_held) return(shiny::HTML("<!--rp-keep-->"))
    p <- shiny::isolate(rv$p)
    tp <- .tpl_of(p, id)
    if (is.null(tp)) return(NULL)
    prm_all <- .tpl_params(tp$name)
    own_data <- .tpl_own_data(prm_all)
    prm <- .tpl_shown(prm_all)
    tpl_observe(tp$name, prm)
    args <- tp$args
    steps <- p$spec$outputs[[id]]$data$steps
    dataset <- if (own_data) .tpl_steps_fields(steps)$dataset else args$data %||% prm$data$default
    # (the template's data rows and the report's own: their ids the report's
    # data, their datasets the columns a vars(<id>) offers)
    drows <- .tpl_data_rows(tp$name, args, prm_all, own = p$spec$outputs[[id]]$data)
    sec_of <- vapply(names(prm), function(a) .tpl_arg_section(tp$name, a), "")
    miss <- .tpl_missing(tp$name, args, p$spec$study)
    # (the required arguments empty, said in the step they are drawn in)
    miss_all <- miss
    miss <- miss[vapply(miss, function(a) .tpl_arg_section(tp$name, a), "") %in% part]
    # (a template whose analysis set is the report's population: chosen in
    # the data step, written as the report's `population:`)
    pop_of_report <- part %in% c("data", "head") && .tpl_pop_of_report(prm_all)
    if (pop_of_report) {
      pid <- .o_population(p$spec$outputs[[id]])
      if (is.na(pid)) pid <- ""
      if (!nzchar(pid)) {
        miss <- c("population", miss)
        miss_all <- c("population", miss_all)
      }
      pop_of_report <- part == "data"
    }
    kind <- report_info(p, id)$type
    study <- p$spec$study
    # where a value comes from: this report / the study standard / the
    # function's default (and the report's over the study's, said)
    filled <- .tpl_filled(p, id)
    rows <- .tpl_args_rows(tp$name, study, .o_population(p$spec$outputs[[id]]))
    from_of <- function(a) {
      b <- .tpl_badge_of(a, args, rows)
      # (a required argument with no value and no default: nothing to say the origin of)
      if (!(a %in% names(args)) && .tpl_required_empty(prm[[a]]) &&
          (is.null(b) || b$kind %in% c("default", "none"))) return(NULL)
      og <- .tpl_arg_origin(study, prm[[a]], args, filled)
      sv <- og$study
      sk <- NULL
      if (!is.null(b) && !is.null(b$study)) {
        # (tfl_tpl_args() names the study's key and value, also for keys the
        # old lookup misses: plan_cells.default ...)
        sv <- b$study
        sk <- b$key
      }
      if (!is.null(b)) {
        # (the report's value over the study's: said small, not a second badge)
        return(shiny::tagList(
          .tpl_badge_tag(b, t, kind),
          if (identical(b$kind, "own") && !is.null(sv))
            shiny::span(class = "rp-tpl-arg-from",
                        paste0(" ", if (is.null(sk)) sprintf(t("over the study standard (%s)"), .tpl_show(sv)) else
                          sprintf(t("over the study standard (%s: %s)"), sk, .tpl_show(sv))))))
      }
      # (tflspec cannot say: the text it was)
      if (identical(og$from, "own")) {
        return(shiny::span(class = "rp-tpl-arg-from",
                           t(if (identical(kind, "table")) "This table" else "This report"),
                           if (!is.null(sv)) paste0(" \u2014 ", sprintf(t("over the study standard (%s)"),
                                                                       .tpl_show(sv)))))
      }
      std <- if (!is.na(og$source)) sprintf(t("Study standard (%s)"), og$source) else t("Study standard")
      shiny::span(class = "rp-tpl-arg-from",
                  if (identical(og$from, "study")) paste0(std, ": ", .tpl_show(sv)) else
                    t("The function's default"))
    }
    # (a key argument's marker, small, beside its label)
    key_tag <- function(x) .tpl_key_tag(x, t)
    row <- function(a, upper) {
      x <- prm[[a]]
      og <- .tpl_arg_origin(study, x, args, filled)
      sv <- if (identical(og$from, "study")) og$study
      nk <- .tpl_none_key(a, args, rows)
      shiny::div(
        class = "d-flex gap-2 align-items-start",
        shiny::div(style = "flex: 1 1 auto; min-width: 0",
                   tpl_widget(tpl_iid(tp$name, a), x, args[[a]] %||% sv, dataset, params = prm,
                              args = args, rows = drows, name = tp$name,
                              none = !is.null(nk),
                              label = shiny::span(tpl_label(x, nk), " ", key_tag(x), from_of(a)))),
        if (upper && !isTRUE(x$required) && a %in% names(args)) shiny::tags$button(
          type = "button", class = "btn btn-sm btn-outline-secondary mt-4",
          title = t("Back to the default (the line is taken out of args)"),
          onclick = sprintf("Shiny.setInputValue('tpl_revert', '%s', {priority: 'event'})", a),
          t("Revert")))
    }
    # a section's arguments: those required, given or key, then the others folded
    block <- function(secs, head = NULL) {
      nm <- names(prm)[sec_of %in% secs]
      if (!length(nm)) return(NULL)
      g <- .tpl_card_groups(prm[nm], args)
      up <- g$up
      low <- g$low
      key <- paste0("tpl_open_", secs[1L])
      shiny::tagList(
        if (!is.null(head)) shiny::h6(class = "mt-2 mb-1", head),
        lapply(up, row, upper = TRUE),
        if (length(low)) shiny::tags$details(
          class = "mt-1", open = if (isTRUE(shiny::isolate(input[[key]]))) NA,
          ontoggle = sprintf("Shiny.setInputValue('%s', this.open)", key),
          shiny::tags$summary(class = "small", sprintf("%s (%d)", t("More settings"), length(low))),
          shiny::div(class = "mt-2", lapply(low, row, upper = FALSE))))
    }
    missing_alert <- if (length(miss)) shiny::div(
      class = "alert alert-danger py-1 small",
      sprintf(t("Required arguments are empty: %s. The preview waits for them."),
              paste(miss, collapse = ", ")))
    if (part != "head") {
      none <- !any(sec_of %in% part) && !(part == "data" && own_data)
      # (a template with no page arguments: no card, the step is the page's)
      if (none && part == "page") return(NULL)
      return(bslib::card(
        class = "mb-2 border-primary",
        bslib::card_header(shiny::strong(sprintf(t("Template arguments: %s"), .tpl_step_word[[part]]))),
        bslib::card_body(
          missing_alert,
          if (part == "data" && own_data) tpl_data_block(id, steps),
          # (the analysis set of a template that takes it from the report:
          # shown, #751 item 6)
          if (part == "data" && !own_data && !pop_of_report) .tpl_pop_view_ui(.tpl_pop_view(p, id), t),
          if (pop_of_report) shiny::selectInput("tpl_report_pop", tpl_label(prm_all$population),
                                                pop_choices(pid),
                                                selected = pid, width = "100%"),
          block(part),
          if (none) shiny::p(class = "small text-muted mb-0", t("This template has no arguments in this step.")))))
    }
    # (the head: the required arguments empty in any step, with the step's name)
    head_missing <- if (length(miss_all)) shiny::div(
      class = "alert alert-danger py-1 small mb-0 w-100",
      sprintf(t("Required arguments are empty: %s. The preview waits for them."),
              paste(vapply(miss_all, function(a) {
                sec <- if (identical(a, "population") && .tpl_pop_of_report(prm_all)) "data" else
                  .tpl_arg_section(tp$name, a)
                sprintf("%s (%s)", a, t(.step_labels[[sec]]))
              }, ""), collapse = ", ")))
    others <- .tpl_list(kind)$name
    # (what the report's template: holds that is no argument of the
    # template: kept as written and shown, tflspec stops on it -- #751)
    extra <- setdiff(names(args), names(prm_all))
    head_extra <- if (length(extra)) shiny::div(
      class = "alert alert-warning py-1 small mb-0 w-100",
      sprintf(t("Not arguments of this template, kept as written (the run stops on them): %s"),
              paste(extra, collapse = ", ")),
      shiny::tags$pre(class = "small mb-0", yaml::as.yaml(args[extra])))
    bslib::card(
      class = "mb-2 border-primary",
      bslib::card_header(shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-center",
        # (the name once, in the choice of template: #751 item 7)
        with_tip(shiny::strong(t("Template")),
                 sprintf(t("Saved in spec/outputs/%s.yml as template: name and its args only."), id)),
        shiny::code(.tpl_call_name(tp$name)),
        # (another template: chosen here, confirmed in a dialog)
        shiny::tags$select(
          class = "form-select form-select-sm ms-auto", style = "width: auto",
          title = t("Change the template"),
          onchange = "Shiny.setInputValue('tpl_switch', this.value, {priority: 'event'})",
          lapply(others, function(n) shiny::tags$option(
            value = n, selected = if (identical(n, tp$name)) NA, n))),
        # (one way: the template's items written into the report as code;
        # a plain button, not an input)
        shiny::tags$button(
          type = "button", class = "btn btn-sm btn-outline-primary",
          onclick = "Shiny.setInputValue('tpl_to_code', Math.random(), {priority: 'event'})",
          paste0(t("Convert to code"), " (tfl_template_to_spec)")),
        # (for later: greyed)
        shiny::div(
          class = "dropdown",
          shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary dropdown-toggle",
                             `data-bs-toggle` = "dropdown", `aria-expanded` = "false", "\u22ef"),
          shiny::tags$ul(
            class = "dropdown-menu dropdown-menu-end",
            shiny::tags$li(shiny::tags$a(class = "dropdown-item disabled", href = "#",
                                         t("Save as a template ..."))),
            shiny::tags$li(shiny::tags$a(class = "dropdown-item disabled", href = "#",
                                         t("Make a variant ..."))))),
        if (nzchar(.tpl_desc_of(tp$name))) shiny::div(class = "small text-muted w-100",
                                                      t(.tpl_desc_of(tp$name))),
        head_missing, head_extra)))
  }
  # a template figure: drawn here from the template's items
  # (tflspec::tfl_content_run(), the figure as its program makes it), the
  # PNG in the Report result tab
  tpl_fig <- shiny::reactiveVal(NULL)
  # what the figure's own ARD of the preview was made from (a table's is kept
  # by its Update preview; ard_preview_state())
  tpl_fig_ard <- shiny::reactiveVal(NULL)
  tpl_fig_draw <- function() {
    id <- current()
    p <- rv$p
    if (is.null(id) || is.null(.tpl_of(p, id))) return()
    miss <- .tpl_missing(.tpl_of(p, id)$name, .tpl_of(p, id)$args, p$spec$study)
    if (length(miss)) {
      tpl_fig(list(id = id, key = tpl_def_key(id), error = sprintf(
        t("Required arguments are empty: %s. The preview waits for them."), paste(miss, collapse = ", "))))
      return(tpl_show_result("fig_right"))
    }
    res <- shiny::withProgress(message = sprintf(t("Preview of %s"), id), tryCatch({
      r <- tflspec::tfl_resolve_output(p$spec$outputs[[id]], p$spec$study)
      d <- tpl_data()
      names(d) <- tolower(names(d))
      # a figure that reads an ARD (forest_hr): the ARD of its analyses made
      # for the preview, as a table template's preview makes its own (not
      # the study's ard.rds, which may not be made); tfl_content_run() adds
      # the helpers (ard_stats ...) when it has `ard`
      src <- .fig_ard_source(p, id)
      if (src$kind %in% c("own", "table")) {
        shiny::setProgress(0.1, detail = sprintf(t("making its ARD (%d analyses, in its own R process)"),
                                                  NROW(ard_rows(p, "analyses", src$id))))
        a <- run_ard(current_study(), src$id)
        if (is.null(a$ard)) stop(paste(t("The preview's ARD could not be made:"), a$error), call. = FALSE)
        d$ard <- a$ard
        if (identical(src$kind, "own")) {
          # (the ARD result tab shows it; what it was made from says when it is outdated)
          ard_res(list(ard = a$ard, scope = id, seconds = a$seconds))
          tpl_fig_ard(list(id = id, print = .ard_print(current_study(), id)))
          rv$ard_ver <- rv$ard_ver + 1L
        }
      }
      # (the study: the figure draws its report's set's subjects only, as
      # its program does -- canon 11-15; a set tflspec cannot make them
      # for stops with its message, said below as any error of the preview)
      g <- tflspec::tfl_content_run(r, d, study = .content_study(r, p$spec$study))
      f <- tempfile("tplfig", fileext = ".png")
      save <- getExportedValue("ggplot2", "ggsave")
      save(f, g, width = 7.3, height = 4.2, dpi = 110)
      list(id = id, png = f)
    }, error = function(e) list(id = id, error = conditionMessage(e))))
    res$key <- tpl_def_key(id)
    tpl_fig(res)
    tpl_show_result("fig_right")
  }
  # what a preview was made from: the report's definition; the pane says
  # when it has changed since
  tpl_def_key <- function(id) tryCatch(.code_spec(rv$p)$outputs[[id]], error = function(e) NULL)
  # Update preview: on to Report result, unless the ARD's tabs are shown
  # (the ARD is what is being looked at then)
  tpl_show_result <- function(nav) {
    if (!input[[nav]] %in% c("ard_code", "ard")) bslib::nav_select(nav, "result", session = session)
  }
  # Update preview above the tabs (a figure's, a listing's; a table's is the ARD card's)
  tpl_bar <- function(btn) shiny::div(class = "d-flex align-items-center gap-1 mb-1 rp-pv-bar", btn)
  output$tpl_fig_bar <- shiny::renderUI({
    id <- current()
    shiny::req(id, identical(report_kind(), "figure"), !is.null(.tpl_of(shiny::isolate(rv$p), id)))
    tpl_bar(shiny::tagList(
      .btn("tpl_fig_preview", t("Update preview"), class = "btn-sm btn-outline-primary py-0"),
      help_tip(t(.preview_tip))))
  })
  output$tpl_lf_bar <- shiny::renderUI({
    id <- current()
    shiny::req(id, identical(report_kind(), "listing"))
    tpl_bar(.btn("lf_preview_tpl", t("Update preview"), class = "btn-sm btn-outline-primary py-0"))
  })
  shiny::observeEvent(input$lf_preview_tpl, {
    id <- current()
    if (!identical(lf_type(), "listing")) return()
    lf_show_preview(id)
    tpl_show_result("lf_right")
  })
  shiny::observeEvent(input$tpl_fig_preview, tpl_fig_draw())
  output$tpl_fig_out <- shiny::renderUI({
    if (!identical(report_kind(), "figure")) return(NULL)
    id <- current()
    if (is.null(id) || is.null(.tpl_of(shiny::isolate(rv$p), id))) return(NULL)
    r <- tpl_fig()
    old <- !is.null(r$key) && !identical(r$key, tpl_def_key(id))
    body <- if (is.null(r) || !identical(r$id, id)) {
      shiny::p(class = "small text-muted", t("Update preview draws the figure."))
    } else if (!is.null(r$error)) {
      shiny::div(class = "alert alert-danger py-2 small",
                 shiny::strong(t("The figure could not be drawn: ")), r$error)
    } else {
      bytes <- readBin(r$png, "raw", file.size(r$png))
      shiny::tagList(
        if (old) shiny::p(class = "small text-warning mb-1", t("Changed: preview again")),
        shiny::tags$img(src = paste0("data:image/png;base64,", jsonlite::base64_enc(bytes)),
                        style = "max-width: min(100%, 760px); border: 1px solid #dee2e6"))
    }
    # (not a card: it sits in the Report result tab; the expanded code is in
    # Report code)
    body
  })
  output$tpl_card <- shiny::renderUI({
    if (!identical(report_kind(), "table")) return(NULL)
    tpl_card_ui()
  })
  output$tpl_card_lf <- shiny::renderUI({
    if (!identical(report_kind(), "listing")) return(NULL)
    tpl_card_ui()
  })
  output$tpl_card_fig <- shiny::renderUI({
    if (!identical(report_kind(), "figure")) return(NULL)
    tpl_card_ui()
  })
  # the other steps' arguments (any kind of template report), and the
  # template's own card above the steps
  for (part in c("data", "ard", "page", "head")) {
    local({
      part <- part
      output[[paste0("tpl_card_", part)]] <- shiny::renderUI({
        id <- current()
        shiny::req(id)
        tpl_card_draw(id, part)
      })
    })
  }
  # (drawn on any step: its draw ends "Loading ...")
  for (o in c("tpl_card", "tpl_card_lf", "tpl_card_fig", "tpl_card_data", "tpl_card_ard", "tpl_card_page",
              "tpl_card_head")) {
    shiny::outputOptions(output, o, suspendWhenHidden = FALSE)
  }
  # The right of a template table: the ARD its template makes is made in
  # steps of its own, ARD code => ARD result, before the
  # report's SPEC => Code => Result -- tabs of every template report, greyed
  # out when it makes no ARD
  tpl_now <- shiny::reactive({
    id <- current()
    if (is.null(id)) return(NULL)
    tp <- .tpl_of(rv$p, id)
    if (is.null(tp)) NULL else id
  })
  shiny::observe({
    id <- tpl_now()
    on <- !is.null(id) && .report_uses_ard(rv$p, id)
    session$sendCustomMessage("rp-ard-tabs", list(on = on, tip = t("This report uses no ARD.")))
  })
  # ARD code: the code that makes the ARD, as Report code shows its program
  tpl_ard_program <- shiny::reactive({
    id <- tpl_now()
    shiny::req(id)
    a <- structure(rv$p$ard, class = "tfl_ard_spec")
    code <- if (any(a$analyses$output_id %in% id)) tryCatch(
      ard_program_code(a, id, dir = rv$study$path, codelists = .study_codelists(rv$p)),
      error = function(e) paste(t("The code cannot be written yet:"), conditionMessage(e)))
    shiny::req(!is.null(code))
    paste(code, collapse = "\n")
  })
  # the report's ARD as the ARD result tab shows it: the preview's, else the
  # one made before (NULL: none yet)
  tpl_ard_of <- function(id) {
    r <- ard_res()
    if (!is.null(r) && identical(r$scope, id)) r$ard else
      tryCatch(ard_raw(current_study(), id, rows = TRUE), error = function(e) NULL)
  }
  # its ARD made from a definition that has changed since (a table's: the
  # badge's idea, preview_state(); a figure's own: what its preview was made from)
  tpl_ard_outdated <- function(id) {
    if (identical(report_kind(), "table")) return(identical(preview_state(id), "outdated"))
    k <- tpl_fig_ard()
    !is.null(k) && identical(k$id, id) &&
      !identical(k$print, tryCatch(.ard_print(current_study(), id), error = function(e) NULL))
  }
  # (a pane for each kind of template report: an output id is in the page once)
  for (sfx in c("", "_lf", "_fig")) local({
    sfx <- sfx
    output[[paste0("tpl_ard_pane", sfx)]] <- shiny::renderUI({
      id <- tpl_now()
      shiny::req(id)
      rv$ard_ver
      out <- .study_ard_rel(rv$p$ard)
      run <- shiny::p(class = "small text-muted mb-1",
                      sprintf(t("A run saves it in the study's ARD (%s)."), out))
      # no ARD made yet: Update preview makes it
      if (.report_uses_ard(rv$p, id) && is.null(tpl_ard_of(id))) {
        return(shiny::tagList(
          shiny::p(class = "small text-muted mb-1", t("Update preview to see the ARD this report's analyses make.")), run))
      }
      shiny::tagList(
        if (tpl_ard_outdated(id)) {
          shiny::p(class = "small text-warning mb-1", t("ARD outdated: Update preview"))
        },
        run,
        shiny::div(class = "rp-resize", DT::DTOutput(paste0("tpl_ard_table", sfx), height = "auto", fill = FALSE)))
    })
    output[[paste0("tpl_ard_table", sfx)]] <- DT::renderDT({
      id <- tpl_now()
      shiny::req(id)
      rv$ard_ver
      shiny::validate(shiny::need(.report_uses_ard(rv$p, id), t("This report uses no ARD.")))
      a <- tpl_ard_of(id)
      # (no ARD yet: the badge and the ARD line above say it)
      shiny::req(!is.null(a))
      DT::datatable(ard_view(a), rownames = FALSE, filter = "top",
                    selection = "none", class = "compact stripe nowrap", fillContainer = FALSE,
                    options = list(pageLength = 50, scrollX = TRUE, scrollY = "55vh",
                                   scrollCollapse = TRUE, dom = "tip"))
    })
    output[[paste0("tpl_ard_code", sfx)]] <- shiny::renderText(tpl_ard_program())
    output[[paste0("tpl_ard_code_head", sfx)]] <- shiny::renderUI({
      id <- tpl_now()
      shiny::req(id)
      shiny::p(class = "small text-muted mb-1",
               shiny::code(file.path(study_layout()[["programs_ard"]], .ard_prog_name(id))))
    })
    # SPEC: the report's YAML file as written, and expanded (the template's
    # items with the report's over them), read only
    output[[paste0("tpl_spec_yaml", sfx)]] <- shiny::renderUI({
      id <- tpl_now()
      shiny::req(id)
      sp <- tryCatch(.code_spec(rv$p), error = function(e) NULL)
      o <- sp$outputs[[id]]
      shiny::req(!is.null(o))
      yaml_of <- function(x) tryCatch({
        f <- tempfile("tflplanner-tpl", fileext = ".yml")
        on.exit(unlink(f), add = TRUE)
        tflspec::tfl_write_output(x, f)
        paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
      }, error = function(e) conditionMessage(e))
      expanded <- tryCatch(tflspec::tfl_template_to_spec(o, sp$study), error = function(e) NULL)
      shiny::tagList(
        shiny::p(class = "small text-muted mb-1", shiny::code(sp$where[[id]] %||% paste0(id, ".yml"))),
        shiny::div(class = "rp-code", shiny::tags$pre(class = "small rp-flow", yaml_of(o))),
        if (!is.null(expanded)) shiny::tagList(
          shiny::h6(class = "small fw-bold mt-2 mb-1", t("Expanded (read only)")),
          shiny::div(class = "rp-code", shiny::tags$pre(class = "small rp-flow", yaml_of(expanded)))))
    })
    output[[paste0("tpl_code_expanded", sfx)]] <- shiny::renderUI({
      id <- tpl_now()
      shiny::req(id)
      sp <- tryCatch(.code_spec(rv$p), error = function(e) NULL)
      o <- sp$outputs[[id]]
      shiny::req(!is.null(o))
      code <- tryCatch({
        b <- tflspec::tfl_template_to_spec(o, sp$study)
        # (with the study: a figure's code makes its set's subjects first,
        # or says in a stop() line why it cannot)
        r <- tflspec::tfl_resolve_output(b, sp$study)
        paste(tflspec::tfl_content_code(r, pipe = "|>", study = .content_study(r, sp$study)),
              collapse = "\n")
      }, error = function(e) conditionMessage(e))
      shiny::tags$details(
        class = "mt-2",
        shiny::tags$summary(class = "small", t("The content's code expanded")),
        shiny::div(class = "rp-code", shiny::tags$pre(class = "small rp-flow", code)))
    })
  })
  .srv_keep(environment())
}
