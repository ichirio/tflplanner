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
  bslib::nav_panel(
    t("Plot Designer"), value = "designer",
    shiny::tags$style(shiny::HTML("
      .pd-item { cursor: pointer; padding: .25rem .5rem; font-size: .85rem; }
      .pd-item.active { background: #e7f1ff; border-left: 3px solid #0d6efd; }
      .pd-item .pd-sum { color: #6c757d; font-size: .75rem; }
      .pd-item .pd-bad { color: #dc3545; font-weight: bold; }
      .pd-tools button { padding: 0 .3rem; font-size: .75rem; }
      .pd-sec { font-size: .8rem; font-weight: 600; margin: .5rem 0 .2rem; }
      .pd-code textarea { font-family: monospace; font-size: .8rem; }
      .pd-prev { position: relative; }
      .pd-overlay { position: absolute; top: .5rem; left: .5rem; right: .5rem;
        background: rgba(255,255,255,.92); border: 1px solid #dee2e6; border-radius: .3rem;
        padding: .4rem .6rem; font-size: .8rem; max-height: 45%; overflow: auto; }
      .pd-overlay .badge { font-size: .65rem; }
      .pd-overlay button { padding: 0 .4rem; font-size: .7rem; }
      .pd-overlay-toggle { position: absolute; top: .5rem; right: .5rem; font-size: .75rem; }")),
    shiny::uiOutput("pd_note"),
    shiny::uiOutput("pd_body"))
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
                             guarded, catalog) {
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
      other = t("The Plot Designer designs figures: choose a Figure report in the sidebar."),
      hand = t("This figure's plot is written by hand (its data code, Reports tab). Start a design from a template: its data steps, statistics, settings and layers are filled at once, then each can be changed."),
      t("Choose a piece on the left to change it on the right; the figure is redrawn as its program will save it. Empty = the default (shown grey)."))
    shiny::div(class = "alert alert-info py-2 small", msg)
  })

  # ---- starting: a template, filled in for the study's data --------------
  templates <- tflspec::tfl_fig_templates()
  output$pd_body <- shiny::renderUI({
    m <- mode()
    shiny::req(m %in% c("hand", "design"))
    if (m == "hand") {
      ds <- catalog()$dataset
      pr <- fig_presets()
      return(bslib::card(
        bslib::card_header(t("Start a design")),
        shiny::radioButtons("pd_from", NULL, inline = TRUE,
                            c(stats::setNames("template", t("From a template")),
                              if (nrow(pr)) stats::setNames("preset", t("From a company preset")))),
        shiny::conditionalPanel(
          "input.pd_from == 'template'",
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::div(
              shiny::selectInput("pd_tpl", t("Template"), width = "100%",
                                 c(split(stats::setNames(templates$template, t(templates$label)),
                                         factor(templates$kind, levels = unique(templates$kind))),
                                   stats::setNames(list(stats::setNames("", t("Empty design"))), t("Other")))),
              shiny::selectizeInput("pd_tpl_data", t("Dataset"), c("", ds),
                                    options = list(placeholder = t("the template's default")))),
            shiny::uiOutput("pd_tpl_more"))),
        if (nrow(pr)) shiny::conditionalPanel(
          "input.pd_from == 'preset'",
          shiny::selectInput("pd_preset", t("Preset"),
                             stats::setNames(pr$name, ifelse(nzchar(pr$description),
                                                             paste0(pr$name, " - ", pr$description), pr$name))),
          shiny::p(class = "small text-muted",
                   t("A preset is a design kept with the company standards (Save as preset, on a designed figure). It is copied as it is; change its dataset and parameter after."))),
        shiny::div(.btn("pd_start", t("Start the design"), class = "btn-sm btn-primary"))))
    }
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(3, 5, 4, 12)),
      bslib::card(
        bslib::card_header(shiny::div(
          class = "d-flex justify-content-between align-items-center",
          shiny::span(t("Design")),
          shiny::div(
            .btn("pd_batch", t("Copy to other parameters"), class = "btn-sm btn-outline-secondary"),
            .btn("pd_preset_save", t("Save as preset"), class = "btn-sm btn-outline-secondary"),
            .btn("pd_drop", t("Remove the design"), class = "btn-sm btn-outline-danger")))),
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
          shiny::span(t("Preview (as saved)")),
          .btn("pd_redraw", t("Redraw"), class = "btn-sm btn-outline-primary"))),
        shiny::uiOutput("pd_size"),
        shiny::div(class = "pd-prev",
                   shiny::imageOutput("pd_img", height = "auto"),
                   shiny::uiOutput("pd_overlay")),
        shiny::uiOutput("pd_checks")),
      bslib::card(
        bslib::card_header(t("Edit")),
        shiny::uiOutput("pd_form")),
      bslib::navset_card_tab(
        bslib::nav_panel(t("Code"), shiny::div(
          class = "rp-code", shiny::verbatimTextOutput("pd_code"))),
        bslib::nav_panel(t("Design (YAML)"), shiny::div(
          class = "rp-code", shiny::verbatimTextOutput("pd_yaml")))))
  })

  output$pd_tpl_more <- shiny::renderUI({
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
    whole <- !templates$parts[templates$template == tp]
    shiny::tagList(
      if (kind != "swimmer") sz("pd_tpl_param", t("Parameter (PARAMCD)"), prm),
      sz("pd_tpl_pop", t("Analysis set flag"), grep("FL$", vars, value = TRUE)),
      if (!tp %in% c("km_single_arm", "individual_spider") && !kind %in% c("waterfall", "swimmer"))
        sz("pd_tpl_group", t("Group (treatment)"), grep("^TRT|ARM", vars, value = TRUE)),
      if (kind %in% c("km", "swimmer") || tp == "individual_spider")
        shiny::selectInput("pd_tpl_unit", t("Time unit"), c("months", "weeks", "days", "years")),
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
  })

  shiny::observeEvent(input$pd_start, {
    id <- current()
    shiny::req(id)
    tp <- input$pd_tpl
    nz <- function(v) if (is.null(v) || !nzchar(v)) NULL else v
    d <- if (identical(input$pd_from, "preset")) {
      shiny::req(input$pd_preset)
      guarded(read_fig_preset(input$pd_preset))
    } else if (is.null(tp) || !nzchar(tp)) {
      tflspec::tfl_fig_design(
        data = list(list(step = "read", dataset = nz(input$pd_tpl_data) %||% "ADSL")))
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
    if (is.null(d)) return()
    set_design(d)
  })

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
      class = "btn btn-sm btn-light pd-overlay-toggle", type = "button",
      onclick = "Shiny.setInputValue('pd_overlay_toggle', Math.random(), {priority: 'event'})",
      if (overlay_open()) t("Hide") else sprintf("%s (%d)", t("Advice"), n_bad + n_adv))
    if (!overlay_open()) return(toggle)
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
    shiny::div(class = "pd-overlay", toggle, items)
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
  pv <- shiny::reactiveVal(NULL)
  draw <- function() {
    id <- current()
    d <- design()
    if (is.null(id) || is.null(d) || is.null(rv$study)) {
      pv(NULL)
      return()
    }
    s <- rv$study
    s$planner <- rv$p
    r <- tryCatch(preview_figure(s, id, d), error = function(e)
      list(png = NULL, problems = NULL, warnings = character(),
           error = conditionMessage(e), code = NULL))
    pv(r)
  }
  design_d <- shiny::debounce(design, 700)
  shiny::observeEvent(design_d(), draw(), ignoreNULL = FALSE)
  # another figure chosen: its own drawing, even when its design reads the same
  shiny::observeEvent(fig_id(), draw(), ignoreInit = TRUE)
  shiny::observeEvent(input$pd_redraw, draw())

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
             sprintf("%s x %s %s, %s dpi", z$width, z$height, z$units, z$dpi))
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
