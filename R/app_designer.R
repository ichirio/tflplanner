# The Plot Designer: a figure designed piece by piece, drawn as it is
# designed.
#
# A figure's design (tflspec's figure design) is four lists of pieces: the
# data steps from ADaM, the statistics, the figure-wide settings and the
# layers.  A template fills them at once (KM with the number at risk, mean
# over time, waterfall ...).  Then:
#   left    the pieces, as a stack: add, move up or down, remove; a piece
#           with a problem is marked;
#   middle  the figure as its program saves it (the PNG, at its size),
#           redrawn on each change, and the checks;
#   right   the piece chosen, on a form: its fields, the default shown, the
#           data's datasets, variables and PARAMCDs to pick from;
#   below   the code the design makes and the design itself (YAML).
#
# What the pieces and their fields are comes from tflspec's
# tfl_fig_parts(); the design is the figure's (fig_design()), saved with
# the study.

.pd_sections <- c(data = "Data steps", stats = "Statistics",
                  plot = "Figure settings", layers = "Layers")

.designer_ui <- function(t) {
  shiny::conditionalPanel(
    "output.report_kind == 'figure'",
    shiny::tags$style(shiny::HTML("
      .pd-item { cursor: pointer; padding: .25rem .5rem; font-size: .85rem; }
      .pd-item.active { background: #e7f1ff; border-left: 3px solid #0d6efd; }
      .pd-item .pd-sum { color: #6c757d; font-size: .75rem; }
      .pd-item .pd-bad { color: #dc3545; font-weight: bold; }
      .pd-tools button { padding: 0 .3rem; font-size: .75rem; }
      .pd-sec { font-size: .8rem; font-weight: 600; margin: .5rem 0 .2rem; }
      .pd-code textarea { font-family: monospace; font-size: .8rem; }
      .pd-overlay { background: #f8f9fa; border: 1px solid #dee2e6; border-radius: .3rem;
        padding: .4rem .6rem; font-size: .8rem; margin-bottom: .5rem; max-height: 14rem; overflow: auto; }
      .pd-overlay .badge { font-size: .65rem; }
      .pd-overlay button { padding: 0 .4rem; font-size: .7rem; }
      .pd-overlay-head { display: flex; justify-content: space-between; align-items: center; }
      .pd-piece-code pre { font-size: .75rem; max-height: 16rem; margin-bottom: 0; }
      .pd-auto .form-group, .pd-auto .checkbox { margin: 0; }
      .pd-auto label { font-size: .8rem; font-weight: normal; margin: 0; }")),
    shiny::uiOutput("pd_note"),
    # one way to make a figure: the designer (its first step a template or
    # empty); the plot written by hand, user code, apart below
    shiny::uiOutput("pd_body"),
    shiny::uiOutput("lf_fig_box"))
}

# A select input with some of its options disabled (selectize keeps them
# listed but not choosable)
.disable_options <- function(tag, values) {
  html <- as.character(tag)
  for (v in values) {
    html <- sub(paste0('<option value="', v, '"'),
                paste0('<option value="', v, '" disabled'), html, fixed = TRUE)
  }
  # (the selectize script and style go with it)
  htmltools::attachDependencies(htmltools::HTML(html),
                                htmltools::findDependencies(tag))
}

# Can a study with these datasets draw a template?  Its `data` is written
# as tflspec's catalog writes it: "ADTR + ADRS" (both), "ADLB / ADVS + ADSL"
# (ADLB or ADVS, and ADSL); "any" and "(your estimates)" need nothing.
.fig_data_ok <- function(data, have) {
  if (is.null(data) || is.na(data) || !nzchar(data)) return(TRUE)
  have <- toupper(have)
  parts <- trimws(strsplit(data, "+", fixed = TRUE)[[1L]])
  all(vapply(parts, function(p) {
    alt <- toupper(trimws(strsplit(p, "/", fixed = TRUE)[[1L]]))
    !all(grepl("^[A-Z][A-Z0-9]*$", alt)) || any(alt %in% have)
  }, NA))
}

# the dataset a template reads first, of those the study has
.fig_first_data <- function(data, have) {
  if (is.null(data) || is.na(data)) return(NA_character_)
  alt <- toupper(trimws(strsplit(trimws(strsplit(data, "+", fixed = TRUE)[[1L]])[1L],
                                 "/", fixed = TRUE)[[1L]]))
  alt <- alt[alt %in% toupper(have)]
  if (length(alt)) alt[1L] else NA_character_
}

# The template choices: under their category (tflspec >= 0.0.24.9003; the
# kind before), each with the data it reads; those the study's data cannot
# draw stay listed but cannot be chosen, and say why.
.fig_template_choices <- function(templates, have, words) {
  cat <- if ("category" %in% names(templates)) templates$category else templates$kind
  dat <- if ("data" %in% names(templates)) templates$data else rep(NA_character_, nrow(templates))
  ok <- vapply(dat, .fig_data_ok, NA, have = have, USE.NAMES = FALSE)
  lab <- paste0(words$label(templates$label),
                ifelse(is.na(dat), "", paste0("  \u00b7 ", dat)),
                ifelse(ok, "", paste0("  \u2014 ", words$missing)))
  ch <- split(stats::setNames(templates$template, lab),
              factor(cat, levels = unique(cat)))
  ch <- lapply(ch, as.list)
  ch[[words$other]] <- stats::setNames(list(""), words$empty)
  # the first choice: of those it can draw, the one that uses most of the
  # study's data besides ADSL (an AE figure for a study of ADSL and ADAE)
  used <- vapply(dat, function(d) {
    if (is.na(d)) return(0L)
    ds <- toupper(trimws(unlist(strsplit(d, "[+/]"))))
    length(intersect(setdiff(ds, "ADSL"), toupper(have)))
  }, 0L, USE.NAMES = FALSE)
  best <- if (any(ok)) which(ok)[which.max(used[ok])] else NA
  list(choices = ch, off = templates$template[!ok],
       first = if (is.na(best)) "" else templates$template[best])
}

# the value an input gives, as a field: NULL = not set (the default)
.pd_value <- function(kind, v) {
  if (is.null(v) || !length(v)) return(NULL)
  if (kind == "logical") return(isTRUE(v))
  if (kind == "number") {
    v <- suppressWarnings(as.numeric(v))
    return(if (length(v) != 1L || is.na(v)) NULL else v)
  }
  v <- as.character(v)
  v <- v[!is.na(v) & nzchar(trimws(v))]
  if (!length(v)) return(NULL)
  if (kind %in% c("variables", "param")) return(paste(v, collapse = ", "))
  v[1L]
}

# a line on what a piece is set to
.pd_summary <- function(p) {
  keys <- c("dataset", "value", "variable", "expr", "vars", "time", "by",
            "x", "y", "yintercept", "xintercept", "geom", "type", "unit")
  v <- unlist(lapply(keys, function(k) {
    x <- p[[k]]
    if (is.null(x) || !length(x) || identical(x, "")) return(NULL)
    x <- paste(x, collapse = ", ")
    if (nchar(x) > 28) x <- paste0(substr(x, 1, 26), "..")
    if (k %in% c("dataset", "value", "variable", "expr", "geom", "type")) x
    else paste0(k, "=", x)
  }))
  paste(utils::head(v, 3), collapse = "  ")
}

.pd_act <- function(op, sec, i) {
  sprintf("event.stopPropagation(); Shiny.setInputValue('pd_act', {op: '%s', sec: '%s', i: %d, n: Math.random()}, {priority: 'event'})",
          op, sec, i)
}

.designer_server <- function(input, output, session, rv, current, t, notify,
                             guarded, catalog, fig_is_new = function() FALSE,
                             page = function() input$nav) {
  pd <- new.env()
  pd$n <- 0L
  pd$data <- list()
  pd_drawn <- shiny::reactiveVal(0L)
  pd_id <- function(x) paste0("pd", pd$n, "_", x)
  # the piece being edited; the inspector is drawn again only when it
  # changes (or a piece is added, moved, removed), never while one types
  sel <- shiny::reactiveVal(list(sec = "plot", i = 1L))
  form_ver <- shiny::reactiveVal(0L)
  redraw_form <- function() form_ver(form_ver() + 1L)

  parts <- tflspec::tfl_fig_parts()
  pieces_of <- function(sec) {
    p <- unique(parts[parts$section == sec, c("piece", "piece_label")])
    stats::setNames(p$piece, t(p$piece_label))
  }

  design <- shiny::reactive({
    id <- current()
    if (is.null(id) || !identical(report_info(rv$p, id)$type, "figure")) {
      return(NULL)
    }
    fig_design(rv$p, id)
  })
  mode <- shiny::reactiveVal("none")
  fig_id <- shiny::reactiveVal(NA_character_)
  shiny::observe({
    id <- current()
    d <- design()
    m <- if (is.null(id)) "none" else
      if (!identical(report_info(rv$p, id)$type, "figure")) "other" else
        if (is.null(d)) "hand" else "design"
    mode(m)
    fig_id(if (m == "design") id else NA_character_)
  })
  shiny::observeEvent(fig_id(), {
    sel(list(sec = "plot", i = 1L))
    redraw_form()
  })

  set_design <- function(d) {
    id <- current()
    if (is.null(id) || is.null(d)) return()
    rv$p <- set_fig_design(rv$p, id, d)
  }

  # the study's data, read once a file (and again when it changes)
  study_data <- function(ds) {
    if (is.null(rv$study) || !length(ds)) return(list())
    d <- shiny::isolate(catalog())
    out <- list()
    for (x in ds) {
      pth <- d$path[match(toupper(x), toupper(d$dataset))]
      f <- if (length(pth) == 1L && !is.na(pth)) file.path(rv$study$path, pth)
      k <- if (!is.null(f) && file.exists(f)) paste(f, file.mtime(f))
      if (is.null(k)) next
      if (is.null(pd$data[[k]])) {
        s <- rv$study
        s$planner <- shiny::isolate(rv$p)
        pd$data[[k]] <- .study_data(s, x)[[toupper(x)]]
      }
      if (!is.null(pd$data[[k]])) out[[toupper(x)]] <- pd$data[[k]]
    }
    out
  }
  # the variables a design's pieces can name: the datasets' it reads, what
  # its steps derive, and its statistics' columns
  design_vars <- function(d) {
    ds <- unique(toupper(unlist(lapply(d$data, function(s) s$dataset))))
    dat <- study_data(ds)
    v <- unique(unlist(lapply(dat, names)))
    made <- unlist(lapply(d$data, function(s) switch(s$step %||% "",
      derive = s$variable, rank = s$variable %||% "INDEX",
      join = sub("\\s*=.*$", "", trimws(strsplit(s$vars %||% "", ",")[[1L]])))))
    stat <- unlist(lapply(d$stats, function(s) if (identical(s$step, "summary"))
      c("n", "mean", "sd", "se", "lo", "hi")))
    sort(unique(c(v, made, stat)))
  }
  design_params <- function(d) {
    r <- Filter(function(s) identical(s$step, "read"), d$data)
    if (!length(r)) return(character())
    x <- study_data(r[[1L]]$dataset)[[toupper(r[[1L]]$dataset)]]
    if (is.null(x) || !"PARAMCD" %in% names(x)) character() else sort(unique(x$PARAMCD))
  }

  output$pd_note <- shiny::renderUI({
    m <- mode()
    msg <- switch(m,
      none = t("Choose a Figure report in the sidebar."),
      other = t("This tab designs figures: choose a Figure report in the sidebar."),
      hand = if (fig_is_new()) t("A new figure: take the first step -- from a template or empty; both become the designer's layers. To write the ggplot code yourself instead, open User code at the bottom.") else
        t("This figure is drawn by user code (below). To make it with the designer instead, take the first step: from a template or empty."),
      t("Choose a piece on the left to change it on the right; the figure is redrawn as its program will save it. Empty = the default (shown grey)."))
    shiny::div(class = "alert alert-info py-2 small", msg)
  })

  # ---- starting: a template, filled in for the study's data --------------
  templates <- tflspec::tfl_fig_templates()
  # the catalog's datasets whose file is there
  data_present <- function() {
    d <- catalog()
    d$dataset[!is.na(d$path) & file.exists(file.path(rv$study$path, d$path))]
  }
  tpl_select <- function() {
    tc <- .fig_template_choices(
      templates, data_present(),
      list(label = t, missing = t("needs data this study has not got"),
           other = t("Other"), empty = t("Empty design")))
    s <- shiny::selectInput("pd_tpl", t("Template"), tc$choices,
                            selected = tc$first, width = "100%")
    # the ones the data cannot draw: listed, but not to be chosen (the
    # options are HTML inside the tag, so they are marked in the HTML)
    .disable_options(s, tc$off)
  }
  # the template's data, when the study has it, fills the Dataset
  shiny::observeEvent(input$pd_tpl, {
    tp <- input$pd_tpl
    if (is.null(tp) || !nzchar(tp) || !"data" %in% names(templates)) return()
    ds <- .fig_first_data(templates$data[templates$template == tp], data_present())
    shiny::updateSelectizeInput(session, "pd_tpl_data",
                                selected = if (is.na(ds)) "" else ds)
  })
  start_inputs <- function() {
    ds_have <- data_present()
    pr <- fig_presets()
    shiny::tagList(
      shiny::radioButtons("pd_from", NULL, inline = TRUE,
                          c(stats::setNames("template", t("From a template")),
                            if (nrow(pr)) stats::setNames("preset", t("From a company preset")))),
      shiny::conditionalPanel(
        "input.pd_from == 'template'",
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::div(
            tpl_select(),
            shiny::selectizeInput("pd_tpl_data", t("Dataset"), c("", ds_have),
                                  options = list(placeholder = t("choose the data")))),
          shiny::uiOutput("pd_tpl_more"))),
      if (nrow(pr)) shiny::conditionalPanel(
        "input.pd_from == 'preset'",
        shiny::selectInput("pd_preset", t("Preset"),
                           stats::setNames(pr$name, ifelse(nzchar(pr$description),
                                                           paste0(pr$name, " - ", pr$description), pr$name))),
        shiny::p(class = "small text-muted",
                 t("A preset is a design kept with the company standards (Save as preset, on a designed figure). It is copied as it is; change its dataset and parameter after."))))
  }
  output$pd_body <- shiny::renderUI({
    m <- mode()
    shiny::req(m %in% c("hand", "design"))
    if (m == "hand") {
      return(bslib::card(
        bslib::card_header(t("First step (both become the designer's layers)")),
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4)),
          shiny::div(
            shiny::h6(t("Start from a template")),
            shiny::p(class = "small text-muted",
                     t("Choose a type, its data and a few settings, then apply: the designer gets its layers at once, to add to and change.")),
            start_inputs(),
            shiny::div(.btn("pd_start", t("Apply the template"), class = "btn-sm btn-primary"))),
          shiny::div(
            shiny::h6(t("Start empty")),
            shiny::p(class = "small text-muted",
                     t("Only ggplot() and the data it reads: add the layers one by one.")),
            shiny::div(.btn("pd_empty", t("Start empty"), class = "btn-sm btn-outline-primary"))))))
    }
    id <- current()
    shiny::tagList(
    shiny::div(
      class = "small mb-2",
      shiny::strong(t("This figure's Spec (YAML):")), " ",
      shiny::code(sprintf("spec/figures/%s.yml", id)), " ",
      shiny::span(class = "text-muted",
                  t("-- the designer's result; the same whether started from a template or empty."))),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(3, 5, 4, 12)),
      bslib::card(
        bslib::card_header(t("Design")),
        # the actions on their own row, so they wrap instead of crowding the title
        shiny::div(
          class = "d-flex flex-wrap gap-1 mb-2",
          .btn("pd_retpl", t("Apply a template..."), class = "btn-sm btn-outline-secondary"),
          .btn("pd_batch", t("Copy to other parameters"), class = "btn-sm btn-outline-secondary"),
          .btn("pd_preset_save", t("Save as preset"), class = "btn-sm btn-outline-secondary"),
          .btn("pd_drop", t("Remove the design"), class = "btn-sm btn-outline-danger")),
        shiny::uiOutput("pd_stack"),
        # adding a piece: outside the stack, which is drawn again on each change
        shiny::div(
          class = "d-flex gap-1 mt-2 align-items-start",
          shiny::div(style = "flex: 1", shiny::selectInput(
            "pd_add", NULL, width = "100%",
            stats::setNames(lapply(c("data", "stats", "layers"), pieces_of),
                            t(.pd_sections[c("data", "stats", "layers")])))),
          .btn("pd_addbtn", t("Add"), class = "btn-sm btn-outline-primary"))),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::div(
          class = "d-flex justify-content-between align-items-center",
          shiny::span(t("Preview")),
          shiny::div(
            class = "d-flex align-items-center gap-3 pd-auto",
            shiny::checkboxInput("pd_auto", t("Redraw on change"), value = TRUE, width = "auto"),
            .btn("pd_redraw", t("Redraw"), class = "btn-sm btn-outline-primary")))),
        shiny::uiOutput("pd_state"),
        shiny::uiOutput("pd_size"),
        shiny::uiOutput("pd_overlay"),
        shiny::imageOutput("pd_img", height = "auto"),
        shiny::uiOutput("pd_checks")),
      bslib::card(
        bslib::card_header(t("Edit")),
        shiny::uiOutput("pd_form"),
        shiny::h6(class = "mt-3", t("Code of this piece")),
        shiny::p(class = "small text-muted mb-1",
                 t("What this piece writes into the program; it follows every change.")),
        shiny::div(class = "rp-code pd-piece-code", shiny::verbatimTextOutput("pd_piece_code"))),
      bslib::navset_card_tab(
        bslib::nav_panel(t("Code"), shiny::div(
          class = "rp-code", shiny::verbatimTextOutput("pd_code"))),
        bslib::nav_panel(t("Spec (YAML)"),
          shiny::p(class = "small text-muted mb-1",
                   t("The designer's result: the same form whether started from a template or built from empty.")),
          shiny::div(class = "rp-code", shiny::verbatimTextOutput("pd_yaml"))))))
  })

  output$pd_tpl_more <- shiny::renderUI(tryCatch(pd_tpl_more_ui(), error = function(e) {
    if (inherits(e, "shiny.silent.error")) stop(e)
    message("tflplanner: template form: ", conditionMessage(e))
    shiny::div(class = "alert alert-warning py-1 small",
               sprintf(t("The template's choices could not be made from the data: %s"),
                       conditionMessage(e)))
  }))
  pd_tpl_more_ui <- function() {
    tp <- input$pd_tpl
    shiny::req(!is.null(tp), nzchar(tp))
    kind <- templates$kind[templates$template == tp]
    ds <- input$pd_tpl_data
    dat <- if (!is.null(ds) && nzchar(ds)) study_data(c(ds, "ADSL")) else list()
    x <- dat[[toupper(ds %||% "")]]
    vars <- unique(c(names(x), names(dat$ADSL)))
    prm <- if (!is.null(x) && "PARAMCD" %in% names(x)) sort(unique(x$PARAMCD))
    sz <- function(id, lab, ch) shiny::selectizeInput(id, lab, c("", ch),
      options = list(placeholder = t("the template's default"), create = TRUE))
    cols <- .data_columns(dat)
    whole <- !templates$parts[templates$template == tp]
    shiny::tagList(
      if (kind != "swimmer") sz("pd_tpl_param", t("Parameter (PARAMCD)"), prm),
      sz("pd_tpl_pop", t("Analysis set flag"),
         .labelled(grep("FL$", names(cols), value = TRUE), cols)),
      if (!tp %in% c("km_single_arm", "individual_spider") && !kind %in% c("waterfall", "swimmer"))
        sz("pd_tpl_group", t("Group (treatment)"), .group_choices(cols)),
      if (kind %in% c("km", "swimmer") || tp == "individual_spider")
        shiny::selectInput("pd_tpl_unit", t("Time shown in (the data's time is in days)"),
                           c("months", "weeks", "days", "years")),
      if (kind %in% c("mean", "box", "individual", "pk") && !whole)
        sz("pd_tpl_value", t("Value"), intersect(c("AVAL", "CHG", "PCHG"), vars)),
      if (kind == "scatter") sz("pd_tpl_x", t("X"), vars),
      if (kind == "scatter") sz("pd_tpl_y", t("Y"), vars),
      if (kind == "pk") sz("pd_tpl_time", t("Nominal time"), vars),
      if (kind == "bar") sz("pd_tpl_category", t("Category"), vars),
      if (tp == "bar_rate_ci") shiny::textInput("pd_tpl_responders", t("Counted as response"), "CR, PR"),
      if (kind == "swimmer") sz("pd_tpl_duration", t("Duration (days)"), vars),
      if (tp %in% c("box_by_group", "scatter_shift")) sz("pd_tpl_at_visit", t("At visit"),
        if (!is.null(x) && "AVISIT" %in% names(x)) unique(x$AVISIT)),
      if (whole) shiny::p(class = "small text-muted",
        t("This type is not yet in parts: the design is its whole script, with the type's arguments to edit.")))
  }

  empty_design <- function() {
    ds <- input$pd_tpl_data
    tflspec::tfl_fig_design(data = list(list(
      step = "read", dataset = if (is.null(ds) || !nzchar(ds)) "ADSL" else ds)))
  }
  applied <- function(d) {
    if (is.null(d)) return()
    set_design(d)
    notify(t("The template is applied: add to and change its layers in the designer."))
  }
  shiny::observeEvent(input$pd_start, {
    shiny::req(current())
    applied(chosen_design())
  })
  shiny::observeEvent(input$pd_empty, {
    shiny::req(current())
    set_design(empty_design())
  })
  # a template on a design already there: it replaces the design, asked first
  shiny::observeEvent(input$pd_retpl, {
    d <- design()
    shiny::req(d)
    shiny::showModal(shiny::modalDialog(
      title = t("Apply a template"), size = "l", easyClose = TRUE,
      shiny::div(class = "alert alert-warning py-2 small",
                 sprintf(t("The current design (%d layers) is replaced by the template's."),
                         length(d$layers))),
      start_inputs(),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("pd_retpl_ok", t("Replace with the template"),
                                   class = "btn-danger"))))
  })
  shiny::observeEvent(input$pd_retpl_ok, {
    shiny::req(current())
    d <- chosen_design()
    if (is.null(d)) return()
    shiny::removeModal()
    applied(d)
  })
  chosen_design <- function() {
    tp <- input$pd_tpl
    nz <- function(v) if (is.null(v) || !nzchar(v)) NULL else v
    d <- if (identical(input$pd_from, "preset")) {
      shiny::req(input$pd_preset)
      guarded(read_fig_preset(input$pd_preset))
    } else if (is.null(tp) || !nzchar(tp)) {
      empty_design()
    } else {
      ds <- nz(input$pd_tpl_data)
      grp <- nz(input$pd_tpl_group)
      pop <- nz(input$pd_tpl_pop)
      # the group and the flag from ADSL when the dataset has not got them
      join <- if (!is.null(ds)) {
        x <- study_data(ds)[[toupper(ds)]]
        !is.null(x) && !all(c(grp, pop) %in% names(x))
      }
      args <- list(tp, data = ds, param = nz(input$pd_tpl_param), pop = pop,
                   group = grp, join_adsl = if (isTRUE(join)) TRUE,
                   value = nz(input$pd_tpl_value), x = nz(input$pd_tpl_x), y = nz(input$pd_tpl_y),
                   time = nz(input$pd_tpl_time), category = nz(input$pd_tpl_category),
                   responders = nz(input$pd_tpl_responders), duration = nz(input$pd_tpl_duration),
                   at_visit = nz(input$pd_tpl_at_visit))
      if (!is.null(nz(input$pd_tpl_unit))) args$time_unit <- input$pd_tpl_unit
      guarded(do.call(tflspec::tfl_fig_template, args[!vapply(args, is.null, logical(1))]))
    }
    d
  }

  # the same figure for other parameters: one new figure report each, its
  # design this one with the parameter changed
  shiny::observeEvent(input$pd_batch, {
    d <- design()
    id <- current()
    shiny::req(d, id)
    ps <- Filter(function(x) identical(x$step, "param"), d$data)
    if (!length(ps)) {
      notify(t("The design has no 'Keep a parameter' step to change."), "warning")
      return()
    }
    params <- design_params(d)
    now <- unlist(strsplit(paste(ps[[1L]]$value, collapse = ","), "\\s*[|,]\\s*"))
    shiny::showModal(shiny::modalDialog(
      title = t("Copy to other parameters"),
      shiny::selectizeInput("pd_batch_params", t("Parameters (one figure each)"),
                            setdiff(params, now), multiple = TRUE, width = "100%",
                            options = list(create = TRUE)),
      shiny::textInput("pd_batch_id", t("New IDs: {id} = this figure's ID, {param} = the parameter"),
                       paste0(id, "-{param}"), width = "100%"),
      shiny::p(class = "small text-muted",
               t("Each new figure copies this one's layout and design; only its parameter differs.")),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("pd_batch_ok", t("Copy"), class = "btn-primary"))))
  })
  shiny::observeEvent(input$pd_batch_ok, {
    id <- current()
    prms <- input$pd_batch_params
    shiny::req(design(), id, length(prms))
    shiny::removeModal()
    p <- guarded(copy_fig_to_params(rv$p, id, prms, input$pd_batch_id))
    if (is.null(p)) return()
    rv$p <- p
    rv$ver <- rv$ver + 1L
    made <- setdiff(p$outputs$output_id, rv$saved$outputs$output_id)
    notify(sprintf(t("Made %d figure(s): %s"), length(prms), paste(utils::tail(p$outputs$output_id, length(prms)), collapse = ", ")))
  })

  shiny::observeEvent(input$pd_preset_save, {
    shiny::req(design())
    shiny::showModal(shiny::modalDialog(
      title = t("Save as preset"),
      shiny::textInput("pd_preset_name", t("Name"), width = "100%"),
      shiny::textInput("pd_preset_desc", t("Description"), width = "100%"),
      shiny::p(class = "small text-muted",
               t("Kept with the company standards (standards/figure-presets), for every study of this home.")),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("pd_preset_ok", t("Save"), class = "btn-primary"))))
  })
  shiny::observeEvent(input$pd_preset_ok, {
    d <- design()
    shiny::req(d)
    f <- guarded(save_fig_preset(d, input$pd_preset_name, input$pd_preset_desc %||% ""))
    if (is.null(f)) return()
    shiny::removeModal()
    notify(sprintf(t("Preset saved: %s"), basename(f)))
  })
  # the advice's one-step fixes
  shiny::observeEvent(input$pd_fix, {
    r <- pv()
    d <- design()
    i <- as.integer(input$pd_fix$i)
    shiny::req(r, d, !is.null(r$advice), i >= 1L, i <= nrow(r$advice))
    fix <- r$advice$fix[[i]]
    shiny::req(!is.null(fix))
    d2 <- guarded(tflspec::tfl_fig_apply_fix(d, fix))
    if (is.null(d2)) return()
    set_design(d2)
    sel(list(sec = "plot", i = 1L))
    redraw_form()
  })
  overlay_open <- shiny::reactiveVal(TRUE)
  shiny::observeEvent(input$pd_overlay_toggle, overlay_open(!overlay_open()))
  output$pd_overlay <- shiny::renderUI({
    r <- pv()
    shiny::req(r, r$png)
    a <- r$advice
    pr <- r$problems
    n_bad <- if (is.null(pr)) 0L else nrow(pr)
    n_adv <- if (is.null(a)) 0L else nrow(a)
    if (!n_bad && !n_adv) return(NULL)
    toggle <- shiny::tags$button(
      class = "btn btn-sm btn-light", type = "button",
      onclick = "Shiny.setInputValue('pd_overlay_toggle', Math.random(), {priority: 'event'})",
      if (overlay_open()) t("Hide") else t("Show"))
    head <- shiny::div(class = "pd-overlay-head",
                       shiny::strong(sprintf("%s (%d)", t("Checks and advice"), n_bad + n_adv)), toggle)
    if (!overlay_open()) return(shiny::div(class = "pd-overlay", head))
    badge <- function(level) shiny::span(
      class = paste("badge me-1", switch(level, warning = "text-bg-warning",
                                         error = "text-bg-danger", "text-bg-info")),
      t(level))
    part_label <- function(p) {
      k <- sub("\\[.*$", "", p)
      t(if (k %in% names(.pd_sections)) .pd_sections[[k]] else p)
    }
    items <- c(
      if (n_bad) lapply(seq_len(nrow(pr)), function(i) shiny::div(
        class = "mb-1", badge("error"),
        shiny::span(class = "text-muted", pr$part[i], " "), pr$field[i], ": ", pr$problem[i])),
      if (n_adv) lapply(seq_len(nrow(a)), function(i) shiny::div(
        class = "mb-1 d-flex justify-content-between align-items-start gap-2",
        shiny::div(badge(a$level[i]), shiny::span(class = "text-muted", part_label(a$part[i]), " "),
                   do.call(sprintf, c(list(t(a$template[i])), a$args[[i]]))),
        if (!is.null(a$fix[[i]])) shiny::tags$button(
          class = "btn btn-outline-primary btn-sm text-nowrap", type = "button",
          onclick = sprintf("Shiny.setInputValue('pd_fix', {i: %d, n: Math.random()}, {priority: 'event'})", i),
          t("Apply")))))
    shiny::div(class = "pd-overlay", head, items)
  })

  shiny::observeEvent(input$pd_drop, {
    shiny::showModal(shiny::modalDialog(
      title = t("Remove the design?"),
      t("The figure's plot is then its data code again (Reports tab), written by hand."),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("pd_drop_ok", t("Remove"), class = "btn-danger"))))
  })
  shiny::observeEvent(input$pd_drop_ok, {
    shiny::removeModal()
    id <- current()
    shiny::req(id)
    rv$p <- set_fig_design(rv$p, id, NULL)
  })

  # ---- the stack ------------------------------------------------------------
  output$pd_stack <- shiny::renderUI({
    d <- design()
    shiny::req(d)
    s <- sel()
    bad <- unique((pv()$problems %||% data.frame(part = character()))$part)
    item <- function(sec, i, p, label) {
      part <- if (sec == "plot") "plot" else sprintf("%s[%d] %s", sec, i,
                                                   p$step %||% p$layer %||% "")
      n <- length(d[[sec]])
      shiny::div(
        class = paste("pd-item d-flex justify-content-between align-items-start",
                      if (identical(s$sec, sec) && identical(s$i, i)) "active"),
        onclick = .pd_act("sel", sec, i),
        shiny::div(
          if (part %in% bad) shiny::span(class = "pd-bad", "! "),
          shiny::span(label),
          shiny::div(class = "pd-sum", if (sec != "plot") .pd_summary(p))),
        if (sec != "plot") shiny::div(
          class = "pd-tools text-nowrap",
          if (i > 1L) shiny::tags$button(class = "btn btn-link", title = t("Up"),
                                         onclick = .pd_act("up", sec, i), "\u25b2"),
          if (i < n) shiny::tags$button(class = "btn btn-link", title = t("Down"),
                                        onclick = .pd_act("down", sec, i), "\u25bc"),
          shiny::tags$button(class = "btn btn-link text-danger", title = t("Remove"),
                             onclick = .pd_act("del", sec, i), "\u2715")))
    }
    label_of <- function(p) {
      k <- p$step %||% p$layer %||% ""
      l <- parts$piece_label[parts$piece == k][1L]
      t(if (is.na(l)) k else l)
    }
    sec_ui <- function(sec) {
      items <- if (sec == "plot") list(item("plot", 1L, d$plot, t("Title, axes, colours, legend, size")))
        else lapply(seq_along(d[[sec]]), function(i) item(sec, i, d[[sec]][[i]], label_of(d[[sec]][[i]])))
      shiny::div(
        shiny::div(class = "pd-sec", t(.pd_sections[[sec]])),
        if (length(items)) items else shiny::div(class = "small text-muted ps-2", t("(none)")))
    }
    shiny::tagList(
      if (!is.null(d$template)) shiny::p(class = "small text-muted mb-1",
                                         paste0(t("Template"), ": ", d$template)),
      lapply(names(.pd_sections), sec_ui))
  })

  # a piece chosen, moved, removed
  shiny::observeEvent(input$pd_act, {
    a <- input$pd_act
    d <- design()
    shiny::req(d, a$sec)
    sec <- a$sec
    i <- as.integer(a$i)
    if (a$op == "sel") {
      sel(list(sec = sec, i = i))
      redraw_form()
      return()
    }
    x <- d[[sec]]
    if (a$op == "del") {
      x <- x[-i]
      d[[sec]] <- x
      sel(list(sec = "plot", i = 1L))
    } else {
      j <- if (a$op == "up") i - 1L else i + 1L
      if (j < 1L || j > length(x)) return()
      x[c(i, j)] <- x[c(j, i)]
      d[[sec]] <- x
      if (identical(sel()$sec, sec) && identical(sel()$i, i)) sel(list(sec = sec, i = j))
    }
    set_design(d)
    redraw_form()
  })
  # a piece added: its fields' defaults, then it is the one edited
  shiny::observeEvent(input$pd_addbtn, {
      d <- design()
      k <- input$pd_add
      shiny::req(d, k)
      sec <- parts$section[parts$piece == k][1L]
      f <- parts[parts$piece == k, , drop = FALSE]
      req_def <- f$required & !is.na(f$default)
      p <- c(stats::setNames(list(k), if (sec == "layers") "layer" else "step"),
             stats::setNames(as.list(f$default[req_def]), f$field[req_def]))
      # the first layer of a KM figure is its curves
      first <- identical(k, "km_curve")
      d[[sec]] <- if (first) c(list(p), d[[sec]]) else c(d[[sec]], list(p))
      set_design(d)
      sel(list(sec = sec, i = if (first) 1L else length(d[[sec]])))
      redraw_form()
  })

  # ---- the form of the chosen piece -----------------------------------------
  piece_now <- function(d, s) {
    if (s$sec == "plot") return(list(kind = "plot", p = d$plot))
    x <- d[[s$sec]]
    if (s$i > length(x)) return(NULL)
    p <- x[[s$i]]
    list(kind = p$step %||% p$layer, p = p)
  }
  output$pd_form <- shiny::renderUI({
    form_ver()
    fig_id()
    d <- shiny::isolate(design())
    shiny::req(d)
    s <- shiny::isolate(sel())
    pn <- piece_now(d, s)
    shiny::req(pn)
    f <- parts[parts$piece == pn$kind, , drop = FALSE]
    # a whole-script layer: the type's own arguments, from its schema
    fig_args <- NULL
    if (identical(pn$kind, "figure")) {
      sc <- tflspec::tfl_fig_schema(pn$p$type)
      fig_args <- data.frame(
        field = sc$arg, kind = sc$kind, label = sc$label, default = sc$default,
        choices = sc$choices, help = sc$help, required = FALSE, of = sc$of,
        stringsAsFactors = FALSE)
      f <- f[f$field == "type", , drop = FALSE]
    }
    pd$n <- pd$n + 1L
    pd_drawn(pd$n)
    pd$id <- shiny::isolate(current())
    pd$sel <- s
    pd$fields <- f
    pd$fig_args <- fig_args
    vars <- shiny::isolate(design_vars(d))
    params <- shiny::isolate(design_params(d))
    objects <- c("df", unlist(lapply(d$stats, function(x) x$name %||%
      if (identical(x$step, "survfit")) "fit" else if (identical(x$step, "summary")) "sm")))
    cat_ds <- shiny::isolate(catalog())$dataset
    one <- function(r) {
      v <- pn$p[[r$field]]
      def <- r$default
      lab <- shiny::tags$span(
        title = r$help,
        t(r$label), if (isTRUE(r$required)) shiny::span(class = "text-danger", " *"),
        if (!is.na(def)) shiny::tags$small(class = "text-muted", paste0(" (", t("default"), ": ", def, ")")))
      id <- pd_id(r$field)
      ph <- if (is.na(def)) "" else def
      sz <- function(ch, multiple = FALSE, create = TRUE) {
        sel_v <- if (multiple) trimws(strsplit(paste(v %||% "", collapse = ","), ",")[[1L]]) else v %||% ""
        sel_v <- sel_v[nzchar(sel_v)]
        shiny::selectizeInput(id, lab, unique(c(if (!multiple) "", ch, sel_v)),
                              selected = if (length(sel_v)) sel_v else if (!multiple) "",
                              multiple = multiple, width = "100%",
                              options = list(placeholder = ph, create = create))
      }
      el <- switch(r$kind,
        dataset = sz(cat_ds),
        variable = sz(vars),
        flag = sz(grep("FL$", vars, value = TRUE)),
        variables = sz(vars, multiple = TRUE),
        param = sz(params, multiple = TRUE),
        object = sz(objects),
        choice = sz(if (!is.na(r$choices)) strsplit(r$choices, " | ", fixed = TRUE)[[1L]], create = FALSE),
        shape = sz(c("circle", "square", "diamond", "triangle", "triangle_down", "x",
                     "plus", "dot", "solid_square", "solid_triangle", "star", "open_circle"),
                   create = FALSE),
        number = shiny::numericInput(id, lab, value = if (is.null(v)) NA else as.numeric(v), width = "100%"),
        logical = shiny::checkboxInput(id, lab, value = isTRUE(as.logical(v %||% def))),
        code = shiny::div(class = "pd-code",
          shiny::textAreaInput(id, lab, value = paste(v %||% "", collapse = "\n"),
                               rows = 6, width = "100%")),
        shiny::textInput(id, lab, value = paste(v %||% "", collapse = ", "),
                         placeholder = ph, width = "100%"))
      if (!is.na(r$help) && nzchar(r$help) && r$kind != "logical")
        shiny::tagList(el, shiny::div(class = "form-text small mt-n2 mb-2", t(r$help)))
      else el
    }
    head <- if (pn$kind == "plot") t("Figure settings") else t(f$piece_label[1L])
    if (!is.null(fig_args)) {
      # the arguments' values are pn$p$args; the style has its own choices
      a <- pn$p$args %||% list()
      one_arg <- function(r) {
        v <- a[[r$field]]
        if (r$field == "style") v <- pn$p$style
        id <- pd_id(paste0("arg_", r$field))
        lab <- shiny::tags$span(title = r$help, t(r$label),
          if (!is.na(r$default)) shiny::tags$small(class = "text-muted", paste0(" (", t("default"), ": ", r$default, ")")))
        ch <- if (!is.na(r$choices)) strsplit(r$choices, " | ", fixed = TRUE)[[1L]]
        switch(r$kind,
          choice = shiny::selectizeInput(id, lab, c("", ch), selected = v %||% "", width = "100%",
                                         options = list(placeholder = r$default %||% "")),
          number = shiny::numericInput(id, lab, value = if (is.null(v)) NA else as.numeric(v), width = "100%"),
          logical = shiny::checkboxInput(id, lab, value = isTRUE(as.logical(v %||% r$default))),
          shiny::selectizeInput(id, lab, unique(c("", if (r$kind %in% c("variable", "variables", "flag")) vars,
                                                   if (r$kind == "param") params, if (r$kind == "dataset") cat_ds, v)),
                                selected = v %||% "", multiple = r$kind %in% c("variables", "param"),
                                width = "100%", options = list(placeholder = r$default %||% "", create = TRUE)))
      }
      return(shiny::tagList(
        shiny::h6(paste0(t("Whole figure"), ": ", pn$p$type)),
        shiny::p(class = "small text-muted", t(f$piece_help[1L])),
        lapply(seq_len(nrow(fig_args)), function(i) one_arg(fig_args[i, ]))))
    }
    shiny::tagList(
      shiny::h6(head),
      if (nzchar(f$piece_help[1L] %||% "")) shiny::p(class = "small text-muted", t(f$piece_help[1L])),
      lapply(seq_len(nrow(f)), function(i) one(f[i, ])))
  })

  # the form, read back into the piece
  pd_values <- shiny::reactive({
    pd_drawn()
    shiny::req(identical(pd$id, current()))
    f <- pd$fields
    shiny::req(!is.null(f), nrow(f))
    out <- list()
    seen <- FALSE
    if (!is.null(pd$fig_args)) {
      fa <- pd$fig_args
      args <- list()
      style <- NULL
      for (i in seq_len(nrow(fa))) {
        v <- input[[pd_id(paste0("arg_", fa$field[i]))]]
        if (is.null(v)) next
        seen <- TRUE
        val <- .pd_value(if (fa$kind[i] %in% c("variables", "param")) "variables" else fa$kind[i], v)
        if (fa$field[i] == "style") style <- val else args[fa$field[i]] <- list(val)
      }
      shiny::req(seen)
      return(list(sel = pd$sel, fields = list(style = style, args = args[!vapply(args, is.null, logical(1))]),
                  whole = TRUE))
    }
    for (i in seq_len(nrow(f))) {
      v <- input[[pd_id(f$field[i])]]
      if (is.null(v)) next
      seen <- TRUE
      out[f$field[i]] <- list(.pd_value(f$kind[i], v))
    }
    shiny::req(seen)
    list(sel = pd$sel, fields = out)
  })
  pd_values_d <- shiny::debounce(pd_values, 500)
  shiny::observeEvent(pd_values_d(), {
    v <- pd_values_d()
    d <- design()
    if (is.null(d) || !identical(pd$id, current())) return()
    s <- v$sel
    old <- if (s$sec == "plot") d$plot else if (s$i <= length(d[[s$sec]])) d[[s$sec]][[s$i]]
    if (is.null(old)) return()
    new <- old
    for (k in names(v$fields)) new[k] <- list(v$fields[[k]])
    if (isTRUE(v$whole) && !length(new$args)) new$args <- NULL
    new <- new[!vapply(new, is.null, logical(1))]
    if (s$sec == "plot") d$plot <- new else d[[s$sec]][[s$i]] <- new
    if (identical(.fig_norm(unclass(d)), .fig_norm(unclass(design())))) return()
    set_design(d)
    # another dataset changes what the forms offer
    if (!identical(old$dataset, new$dataset)) redraw_form()
  })

  # ---- the drawing ----------------------------------------------------------
  # The figure is drawn for the screen (about 1400 px wide, the saved
  # size), only while this tab shows, and -- unless "Redraw on change" is
  # off -- 0.6 s after the last change.  The code on the right follows
  # every change at once (it takes a few ms).
  pv <- shiny::reactiveVal(NULL)
  drawn <- shiny::reactiveVal(NULL)          # the design the picture shows
  on_tab <- shiny::reactive(identical(page(), "designer"))
  draw <- function() {
    id <- shiny::isolate(current())
    d <- shiny::isolate(design())
    if (is.null(id) || is.null(d) || is.null(rv$study)) {
      pv(NULL)
      drawn(NULL)
      return()
    }
    s <- rv$study
    s$planner <- shiny::isolate(rv$p)
    r <- tryCatch(preview_figure(s, id, d, max_px = 1400), error = function(e)
      list(png = NULL, problems = NULL, warnings = character(),
           error = conditionMessage(e), code = NULL))
    r$id <- id
    pv(r)
    drawn(list(id = id, design = .fig_norm(unclass(d))))
  }
  stale <- shiny::reactive({
    d <- design()
    w <- drawn()
    !is.null(d) && (is.null(w) || !identical(w$id, current()) ||
                      !identical(w$design, .fig_norm(unclass(d))))
  })
  design_d <- shiny::debounce(design, 600)
  shiny::observe({
    design_d()
    fig_id()
    auto <- !identical(input$pd_auto, FALSE)
    if (on_tab() && shiny::isolate(stale()) &&
        (auto || !identical(shiny::isolate(drawn()$id), current()))) {
      draw()
    }
  })
  shiny::observeEvent(input$pd_redraw, draw())
  output$pd_state <- shiny::renderUI({
    if (!stale() || is.null(pv())) return(NULL)
    shiny::div(class = "alert alert-warning py-1 px-2 small mb-2",
               t("The design has changed since this drawing: press Redraw."))
  })

  output$pd_img <- shiny::renderImage({
    r <- pv()
    shiny::req(r, r$png)
    list(src = r$png, contentType = "image/png", width = "100%",
         alt = t("The figure"))
  }, deleteFile = FALSE)
  output$pd_size <- shiny::renderUI({
    r <- pv()
    shiny::req(r)
    if (!is.null(r$error)) {
      return(shiny::div(class = "alert alert-danger py-2 small",
                        shiny::strong(t("The figure could not be drawn: ")), r$error))
    }
    z <- r$size
    if (is.null(z)) return(NULL)
    shiny::p(class = "text-muted small mb-1",
             sprintf(t("Saved as %s x %s %s, %s dpi (shown at screen resolution)"),
                     z$width, z$height, z$units, z$dpi))
  })
  output$pd_checks <- shiny::renderUI({
    r <- pv()
    if (is.null(r)) return(shiny::p(class = "text-muted small", t("Not drawn yet.")))
    pr <- r$problems
    items <- c(
      if (!is.null(pr) && nrow(pr)) lapply(seq_len(nrow(pr)), function(i)
        shiny::tags$li(class = "text-danger",
                       shiny::strong(pr$part[i], " ", pr$field[i]), ": ", pr$problem[i])),
      lapply(r$warnings, function(w) shiny::tags$li(class = "text-warning", w)))
    shiny::div(
      shiny::h6(class = "mt-2", t("Checks")),
      if (!length(items)) shiny::p(class = "text-success small", t("No problems found."))
      else shiny::tags$ul(class = "small ps-3", items))
  })
  # the lines of the script that the chosen piece makes
  output$pd_piece_code <- shiny::renderText({
    id <- current()
    d <- design()
    s <- sel()
    shiny::req(id, d)
    code <- tryCatch(.fig_design_script(d, id), error = function(e) conditionMessage(e))
    if (length(code) == 1L && !grepl("\n", code)) return(code)
    .piece_code(code, d, s)
  })
  output$pd_code <- shiny::renderText({
    id <- current()
    d <- design()
    shiny::req(id, d)
    paste(tryCatch(.fig_design_plot(d, id), error = function(e) conditionMessage(e)),
          collapse = "\n")
  })
  output$pd_yaml <- shiny::renderText({
    d <- design()
    shiny::req(d)
    f <- tempfile(fileext = ".yml")
    on.exit(unlink(f))
    tflspec::tfl_write_fig_design(d, f)
    paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  })
}

# The lines of a design's script that one piece makes: for a layer, from
# its "# ---- layer i:" marker to the next marker; for the figure settings,
# their block; for a data step, the df pipeline (its steps in order) or its
# own code block; for a statistic, the block that makes its object.
.piece_code <- function(code, d, s) {
  block <- function(from, to_pattern) {
    if (!length(from)) return(character())
    rest <- code[seq(from[1L], length(code))]
    end <- grep(to_pattern, rest)
    end <- end[end > 1L]
    if (length(end)) rest[seq_len(end[1L] - 1L)] else rest
  }
  trim <- function(x) {
    while (length(x) && !nzchar(trimws(x[length(x)]))) x <- x[-length(x)]
    x
  }
  out <- switch(s$sec,
    plot = block(grep("^# ---- the figure's settings ----", code), "^# ----|^#{5,}"),
    layers = {
      i <- s$i
      l <- d$layers[[i]]
      k <- l$layer %||% ""
      if (identical(k, "figure")) {
        code
      } else if (i == 1L && !length(grep(sprintf("^# ---- layer %d:", i), code))) {
        # the base layer has no marker: the first line of step 2
        from <- grep("^# Step2", code)
        if (length(from)) block(from[1L] + 2L, "^# ----") else character()
      } else {
        block(grep(sprintf("^# ---- layer %d:", i), code), "^# ----|^#{5,}")
      }
    },
    data = {
      st <- d$data[[s$i]]
      if (identical(st$step, "data_code")) {
        from <- grep("^# your code", code)
        block(from, "^$")
      } else {
        block(grep("^df <- ", code), "^$")
      }
    },
    stats = {
      st <- d$stats[[s$i]]
      if (identical(st$step, "stats_code")) {
        block(grep("^# your code", code), "^$")
      } else {
        nm <- st$name %||% switch(st$step %||% "", survfit = "fit", summary = "sm",
                                  summary_by = "sg", rate = "rt", count = "ct", "")
        block(grep(sprintf("^%s <- ", nm), code), "^$")
      }
    },
    character())
  out <- trim(out)
  if (!length(out)) return(t_static("(nothing yet: this piece writes no line on its own)"))
  paste(out, collapse = "\n")
}
t_static <- function(x) x

# ---- choosing variables in the template form ------------------------------

# Every column of the datasets read, once (the first dataset that has it).
.data_columns <- function(dat) {
  cols <- list()
  for (d in dat) for (nm in names(d)) if (is.null(cols[[nm]])) cols[[nm]] <- d[[nm]]
  cols
}

# Choices shown as "NAME \u2014 label" when the column has a label.
.labelled <- function(nm, cols) {
  nm <- as.character(nm)
  shown <- vapply(nm, function(n) {
    l <- attr(cols[[n]], "label", exact = TRUE)
    if (is.null(l) || !nzchar(l)) n else paste0(n, " \u2014 ", l)
  }, "", USE.NAMES = FALSE)
  stats::setNames(nm, shown)
}

# Columns a summary table can have as rows, shown with how they are
# summarized: "AGE \u2014 Age (numbers)".  Not identifiers, flags, dates.
.row_choices <- function(cols, words = c(continuous = "numbers",
                                          categorical = "counts")) {
  if (!length(cols)) return(character())
  ok <- vapply(names(cols), function(nm) {
    !grepl("^(STUDYID|USUBJID|SUBJID|SITEID)$|FL$", nm) &&
      !.date_like(nm, cols[[nm]]) && !is.na(.column_kind(cols[[nm]]))
  }, NA)
  nm <- names(cols)[ok]
  ch <- .labelled(nm, cols)
  kind <- vapply(nm, function(n) .column_kind(cols[[n]]), "", USE.NAMES = FALSE)
  names(ch) <- paste0(names(ch), " (",
                      words[kind],
                      ")")
  ch
}

# A date, a time, or their imputation flag, whatever its type: named so
# (ADaM / SDTM: ...DTC, ...DT, ...DTM, ...TM, ...DTF, ...TMF, ...DY) or
# holding ISO 8601 dates as text.
.date_like <- function(nm, v) {
  if (inherits(v, c("Date", "POSIXt", "difftime"))) return(TRUE)
  if (grepl("(DTC|DTM|DT|TM|DTF|TMF)$", nm)) return(TRUE)
  if (is.character(v)) {
    x <- utils::head(v[!is.na(v) & nzchar(v)], 20L)
    if (length(x) && all(grepl("^[0-9]{4}-[0-9]{2}(-[0-9]{2})?", x))) return(TRUE)
  }
  FALSE
}

# Columns a figure can be split by: named like a treatment (TRT..., ARM...),
# text or a factor, with few values -- not a date or a time (TRTSDT).
.group_choices <- function(cols, max_levels = 12L) {
  if (!length(cols)) return(character())
  ok <- vapply(names(cols), function(nm) {
    v <- cols[[nm]]
    grepl("^TRT|ARM", nm) && !.date_like(nm, v) &&
      (is.character(v) || is.factor(v)) &&
      length(unique(stats::na.omit(v))) <= max_levels
  }, NA)
  .labelled(names(cols)[ok], cols)
}
