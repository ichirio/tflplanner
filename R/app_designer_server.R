# The Plot Designer's server: .designer_server, the observers and outputs behind
# the designer's pieces, steps and script.
# Split from R/app_designer.R (no code change).

.designer_server <- function(input, output, session, rv, current, t, notify,
                             guarded, catalog,
                             page = function() input$nav, codelists_dialog = NULL,
                             bump = function() NULL, has_study = function() TRUE,
                             read_grid = function(h) rhandsontable::hot_to_r(h),
                             grids_drawn = function() 0L) {
  pd <- new.env()
  session$userData$pd <- pd
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

  # the figure's ARD as it is now (its source's rows; NULL: none, or not made)
  fig_ard <- shiny::reactive({
    id <- current()
    rv$ard_ver
    if (is.null(id) || is.null(rv$study)) return(NULL)
    tryCatch(.fig_ard_rows(rv$study, id, rv$p), error = function(e) NULL)
  })
  # the pieces offered: those reading an ARD only when the figure has one
  shiny::observe({
    id <- current()
    if (is.null(id) || !identical(report_info(rv$p, id)$type, "figure")) return()
    has <- .fig_ard_source(rv$p, id)$kind != "none"
    ch <- lapply(c(data = "data", layers = "layers"), function(sec) {
      x <- pieces_of(sec)
      if (has) x else x[!x %in% .pd_ard_pieces]
    })
    shiny::updateSelectInput(session, "pd_add", choices = ch$layers,
                             selected = shiny::isolate(input$pd_add))
    shiny::updateSelectInput(session, "pd_add_data", choices = ch$data,
                             selected = shiny::isolate(input$pd_add_data))
  })
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
    rv$p <- .fig_ard_steps_rows(set_fig_design(rv$p, id, d), id, d)
  }

  # the study's data, read once a file (and again when it changes)
  study_data <- function(ds) {
    if (is.null(rv$study) || !length(ds)) return(list())
    d <- shiny::isolate(catalog())
    out <- list()
    for (x in ds) {
      pth <- d$path[match(toupper(x), toupper(d$dataset))]
      f <- if (length(pth) == 1L && !is.na(pth)) .data_file(rv$study$path, pth)
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
    stat <- unlist(lapply(d$data, function(s) if (identical(s$step, "summary"))
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
      none = t("Choose a Figure report on the left."),
      other = t("Figures are designed here: choose a Figure report on the left."),
      hand = pane_head(t("A new figure"), t("A new figure: take the first step -- from a template or empty; both become the designer's layers. To write the ggplot code yourself instead: R program at the top of this step.")),
      pane_head(t("The figure's design"),
                t("Choose a piece on the left to change it on the right; the figure is redrawn as its program will save it. Empty = the default (shown grey).")))
    # a heading (its (i) the explanation) as it is; a note in a box
    if (inherits(msg, "shiny.tag") && identical(msg$name, "h6")) return(msg)
    shiny::div(class = "alert alert-info py-2 small", msg)
  })

  # ---- starting: a template, filled in for the study's data --------------
  templates <- tflspec::tfl_fig_templates()
  # the catalog's datasets whose file is there
  data_present <- function() {
    d <- catalog()
    d$dataset[!is.na(d$path) & file.exists(.data_file(rv$study$path, d$path))]
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
        shiny::selectInput("pd_preset", with_tip(t("Preset"), t("A preset is a design kept with the company standards (Save as preset, on a designed figure). It is copied as it is; change its dataset and parameter after.")),
                           stats::setNames(pr$name, ifelse(nzchar(pr$description),
                                                           paste0(pr$name, " - ", pr$description), pr$name)))))
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
            shiny::h6(with_tip(t("Start from a template"),
                               t("Choose a type, its data and a few settings, then apply: the designer gets its layers at once, to add to and change."))),
            start_inputs(),
            shiny::div(.btn("pd_start", t("Apply the template"), class = "btn-sm btn-primary"))),
          shiny::div(
            shiny::h6(with_tip(t("Start empty"),
                               t("Only ggplot() and the data it reads: add the layers one by one."))),
            shiny::div(.btn("pd_empty", t("Start empty"), class = "btn-sm btn-outline-primary"))))))
    }
    id <- current()
    shiny::tagList(
    shiny::div(
      class = "small mb-2",
      shiny::strong(t("This figure's Spec (YAML):")), " ",
      shiny::code(sprintf("spec/outputs/%s.yml", id)), " ",
      shiny::span(class = "text-muted",
                  t("-- the designer's result; the same whether started from a template or empty."))),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(3, 4, 5, 12)),
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
            "pd_add", NULL, width = "100%", pieces_of("layers"))),
          .btn("pd_addbtn", t("Add"), class = "btn-sm btn-outline-primary")),
        shiny::div(class = "small text-muted mt-2",
                   t("The data steps are in step 1 (Data)."))),
      bslib::card(
        bslib::card_header(t("Edit")),
        shiny::uiOutput("pd_form")),
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
      sz("pd_tpl_pop", t("Population flag"),
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
      # a forest plot: its subgroups (ADSL's categorical columns) and the arm
      # compared with the reference (the group's first level)
      if (kind == "forest" && !whole) {
        cat_cols <- names(cols)[vapply(cols, function(v)
          (is.character(v) || is.factor(v)) && length(unique(stats::na.omit(v))) <= 12L, NA)]
        cat_cols <- setdiff(cat_cols, c(.group_choices(cols), grep("FL$", cat_cols, value = TRUE)))
        shiny::selectizeInput("pd_tpl_subgroups", t("Subgroups"), .labelled(cat_cols, cols),
                              intersect(c("SEX", "AGEGR1"), cat_cols), multiple = TRUE,
                              options = list(plugins = list("remove_button", "drag_drop")))
      },
      if (kind == "forest" && !whole) {
        g <- .group_choices(cols)
        lv <- if (length(g)) {
          v <- cols[[g[[1L]]]]
          if (is.factor(v)) levels(v) else sort(unique(stats::na.omit(as.character(v))))
        }
        sz("pd_tpl_comparison", t("Arm compared with the reference (the first)"), lv[-1L])
      },
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
    # its numbers are the figure's own ARD (a forest plot's hazard ratios):
    # the analyses it needs go to step 1, and the figure's ARD is its own
    an <- attr(d, "analyses")
    id <- current()
    if (!is.null(an) && !is.null(id)) {
      pop <- .population_of_flag(rv$p, an$analysis_data$population_id[1L])
      p2 <- guarded(set_fig_own_analyses(rv$p, id, an, population_id = pop))
      if (!is.null(p2)) {
        rv$p <- p2
        notify(sprintf(t("Added analyses %s to step 1 (remove them there if not wanted): update the ARD, then Preview."),
                       paste(attr(p2, "written"), collapse = ", ")))
        if (is.na(pop %||% NA)) notify(sprintf(
          t("No analysis set of the study is the flag %s: choose the analysis data's set in step 1."),
          an$analysis_data$population_id[1L]), "warning")
      }
    }
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
                   at_visit = nz(input$pd_tpl_at_visit),
                   subgroups = if (length(input$pd_tpl_subgroups))
                     paste(input$pd_tpl_subgroups, collapse = ", "),
                   # the arm compared: the one chosen, else the group's second
                   # level (the first is the reference)
                   comparison = nz(input$pd_tpl_comparison) %||%
                     .second_level(study_data(c(ds, "ADSL")), grp))
      if (!is.null(nz(input$pd_tpl_unit))) args$time_unit <- input$pd_tpl_unit
      make <- function(a) guarded(do.call(tflspec::tfl_fig_template,
                                          a[!vapply(a, is.null, logical(1))]))
      d <- make(args)
      # the template's defaults the study's data has not got: the data's own
      if (!is.null(d) && !is.null(ds)) {
        dat <- study_data(c(ds, "ADSL"))
        x <- dat[[toupper(ds)]]
        given <- c(param = "param", pop = "pop", group = "group")[
          !vapply(list(args$param, pop, grp), is.null, logical(1))]
        if (!is.null(x)) d <- .trim_join(d, x)
        fit <- if (!is.null(x)) .template_fit(d, x, dat$ADSL, given) else character()
        if (length(fit)) {
          args[names(fit)] <- as.list(fit)
          args$join_adsl <- if (!all(c(args$group, args$pop) %in% names(x))) TRUE
          d2 <- make(args)
          if (!is.null(d2)) {
            d <- .trim_join(d2, x)
            notify(sprintf(t("The template's defaults are not in %s: it uses %s instead."),
                           ds, paste(sprintf("%s = %s", names(fit), fit),
                                     collapse = ", ")))
          }
        }
      }
      d
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
      t("The figure's plot is then its data code again (step 2, Data code), written by hand."),
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
  # (step 1 the data steps and the code lists, step 2 the settings and the
  # layers)
  stack_ui <- function(step) {
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
      # the code lists go on df: before the first step that makes an object
      if (sec == "data") {
        named <- which(vapply(d$data, function(p) isTRUE(p$step %in% names(.pd_named_steps)), NA))
        items <- append(items, list(cl_item()),
                        after = if (length(named)) named[1L] - 1L else length(items))
      }
      shiny::div(
        shiny::div(class = "pd-sec", t(.pd_sections[[sec]])),
        if (length(items)) items else shiny::div(class = "small text-muted ps-2", t("(none)")))
    }
    # always there, on df before its first object (#293): the report's
    # code lists put on the columns (not the design's: the report's,
    # edited in a dialog)
    cl_item <- function() {
      on <- identical(s$sec, "codelists")
      used <- vapply(.codelist_lines(sheet_rows(rv$p, "codelists", current() %||% ""),
                                     design_vars(d)), `[[`, "", "variable")
      shiny::div(
        class = paste("pd-item", if (on) "active"),
        onclick = .pd_act("sel", "codelists", 1L),
        shiny::span(t("Code lists")),
        shiny::div(class = "pd-sum", if (length(used)) paste(used, collapse = ", ") else t("(none)")))
    }
    secs <- intersect(names(.pd_sections), .pd_step_secs[[step]])
    shiny::tagList(
      if (!is.null(d$template) && step == "figure") shiny::p(class = "small text-muted mb-1",
                                         paste0(t("Template"), ": ", d$template)),
      lapply(secs, sec_ui))
  }
  output$pd_stack <- shiny::renderUI(stack_ui("figure"))
  output$pd_stack_data <- shiny::renderUI(stack_ui("data"))

  # A figure's step 1: its data steps (as step 2 draws the rest) -- or,
  # with no design yet, where to start it
  output$pd_data_body <- shiny::renderUI({
    m <- mode()
    shiny::req(m %in% c("hand", "design"))
    if (m == "hand") {
      return(shiny::div(
        class = "alert alert-info py-2 small",
        t("The figure's data steps come with its design: start it in step 2 (Figure), from a template or empty; they are then here."),
        " ", .btn("pd_goto_figure", t("Go to step 2"), class = "btn-sm btn-outline-primary py-0")))
    }
    bslib::card(
      bslib::card_header(with_tip(t("The data the figure reads"),
        t("The figure's data steps, in their order: the dataset read, the rows kept, the variables joined or derived, the objects made (a KM fit, summary statistics ...). The figure (step 2) draws what they make."))),
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
        shiny::div(
          shiny::uiOutput("pd_stack_data"),
          shiny::div(
            class = "d-flex gap-1 mt-2 align-items-start",
            shiny::div(style = "flex: 1", shiny::selectInput(
              "pd_add_data", NULL, width = "100%", pieces_of("data"))),
            .btn("pd_addbtn_data", t("Add"), class = "btn-sm btn-outline-primary"))),
        shiny::uiOutput("pd_form_data")),
      shiny::div(
        class = "border-top pt-2 mt-2",
        shiny::div(
          class = "d-flex flex-wrap gap-2 align-items-center mb-1",
          shiny::h6(class = "mb-0", with_tip(t("What the steps make"),
            t("The data frames and objects the data steps leave (df, a KM fit, the ARD's statistics ...), made from the study's data as the program makes them: their size, columns and first rows."))),
          .btn("pd_data_preview", t("Preview the data"), class = "btn-sm btn-outline-primary py-0")),
        shiny::uiOutput("pd_objects")))
  })
  shiny::observeEvent(input$pd_data_preview, draw())
  # what the steps made, from the last drawing of this figure
  output$pd_objects <- shiny::renderUI({
    r <- pv()
    if (drawing()) {
      return(shiny::div(class = "small text-muted",
                        shiny::span(class = "spinner-border spinner-border-sm me-2"),
                        t("Making the data ...")))
    }
    if (is.null(r) || !identical(r$id, current())) {
      return(shiny::p(class = "small text-muted mb-0",
                      t("Not made yet: press Preview the data (or draw the figure in step 2).")))
    }
    ob <- r$objects %||% list()
    shiny::tagList(
      if (stale()) shiny::div(class = "alert alert-warning py-1 px-2 small mb-2",
                              t("The steps have changed since: press Preview the data again.")),
      if (!is.null(r$error)) shiny::div(class = "alert alert-danger py-1 px-2 small mb-2",
                                        shiny::strong(t("The steps stopped: ")), r$error),
      if (!length(ob)) shiny::p(class = "small text-muted mb-0", t("(nothing made)")),
      lapply(names(ob), function(nm) .pd_object_ui(nm, ob[[nm]], t)))
  })
  shiny::observeEvent(input$pd_goto_figure, bslib::nav_select("step", "content"))
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
  add_piece <- function(k) {
      d <- design()
      shiny::req(d, k)
      sec <- parts$section[parts$piece == k][1L]
      f <- parts[parts$piece == k, , drop = FALSE]
      req_def <- f$required & !is.na(f$default)
      p <- c(stats::setNames(list(k), if (sec == "layers") "layer" else "step"),
             stats::setNames(as.list(f$default[req_def]), f$field[req_def]))
      x <- d[[sec]]
      at <- .pd_add_at(x, k, sec, sel())
      d[[sec]] <- append(x, list(p), after = at)
      set_design(d)
      sel(list(sec = sec, i = at + 1L))
      redraw_form()
  }
  shiny::observeEvent(input$pd_addbtn, add_piece(input$pd_add))
  shiny::observeEvent(input$pd_addbtn_data, add_piece(input$pd_add_data))

  # ---- the form of the chosen piece -----------------------------------------
  piece_now <- function(d, s) {
    if (s$sec == "plot") return(list(kind = "plot", p = d$plot))
    if (s$sec == "codelists") return(list(kind = "codelists", p = list()))
    x <- d[[s$sec]]
    if (s$i > length(x)) return(NULL)
    p <- x[[s$i]]
    list(kind = p$step %||% p$layer, p = p)
  }
  # the form of the chosen piece, with the code it makes, in the step it
  # belongs to; the other step's says where to choose one (no code box)
  code_box <- function(id) shiny::tagList(
    shiny::h6(class = "mt-3", with_tip(t("Code of this piece"),
                                       t("What this piece writes into the program; it follows every change."))),
    shiny::div(class = "rp-code pd-piece-code", shiny::verbatimTextOutput(id)))
  output$pd_form <- shiny::renderUI({
    form_ver()
    fig_id()
    if (!.pd_in_step(shiny::isolate(sel()), "figure")) {
      return(shiny::p(class = "small text-muted",
                      t("Choose the figure settings or a layer on the left.")))
    }
    shiny::tagList(form_ui(), code_box("pd_piece_code"))
  })
  output$pd_form_data <- shiny::renderUI({
    form_ver()
    fig_id()
    if (!.pd_in_step(shiny::isolate(sel()), "data")) {
      return(shiny::p(class = "small text-muted",
                      t("Choose a data step on the left, or add one.")))
    }
    shiny::tagList(form_ui(), code_box("pd_piece_code_data"))
  })
  form_ui <- function() {
    form_on_page$id <- shiny::isolate(current())
    if (isTRUE(form_on_page$waiting)) {
      form_on_page$waiting <- FALSE
      session$onFlushed(draw_after, once = TRUE)
    }
    d <- shiny::isolate(design())
    shiny::req(d)
    s <- shiny::isolate(sel())
    pn <- piece_now(d, s)
    shiny::req(pn)
    if (identical(pn$kind, "codelists")) {
      pd$fields <- NULL
      pd$fig_args <- NULL
      return(shiny::tagList(shiny::uiOutput("pd_cl"), shiny::uiOutput("pd_cl_detail")))
    }
    f <- parts[parts$piece == pn$kind, , drop = FALSE]
    if (identical(pn$kind, "join")) f <- .pd_join_fields(f, pn$p)
    # a variable with a code list: its values' order and text are the code
    # list's (the data's last step), not a field here
    listed <- unique(sheet_rows(shiny::isolate(rv$p), "codelists",
                                shiny::isolate(current()) %||% "")$variable)
    cl_var <- switch(pn$kind, levels = pn$p$variable, count = pn$p$category, NULL)
    by_cl <- !is.null(cl_var) && cl_var %in% listed
    if (by_cl) f <- f[!f$field %in% c("levels", "labels", "order_by"), , drop = FALSE]
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
    # each field's value as the browser first sends it (as drawn): a field
    # is written only once it differs, so a value the form cannot show
    # (text in a number field, a choice it does not offer) stays as it is
    pd$raw <- list()
    vars <- shiny::isolate(design_vars(d))
    params <- shiny::isolate(design_params(d))
    io <- tryCatch(tflspec::tfl_fig_objects(d), error = function(e) NULL)
    upto <- if (identical(s$sec, "data") && !is.null(io)) seq_len(s$i - 1L) else seq_len(NROW(io))
    objs <- unique(c(if (!is.null(io)) stats::na.omit(io$out[upto]), "df"))
    # the figure's data: kept as `df`, shown by the name the program writes
    # (adtte_ttde: the program renames df)
    objects <- stats::setNames(objs, ifelse(objs == "df", tflspec::tfl_fig_data_name(d) %||% "df", objs))
    io_now <- if (identical(s$sec, "data") && !is.null(io) && s$i <= nrow(io)) io[s$i, ]
    # an ARD analysis: the study's analysis data and ARD functions
    # (the figure's own first; another report's is copied into it)
    an_data <- unique(c(.adata_rows(shiny::isolate(rv$p), shiny::isolate(current()) %||% "")$data_id,
                        stats::na.omit(shiny::isolate(rv$p)$ard$analysis_data$data_id)))
    ard_fns <- tryCatch({
      fx <- tflspec::tfl_ard_functions()
      fx$call[fx$installed %in% TRUE & !fx$offered %in% FALSE]
    }, error = function(e) character())
    # the figure's ARD: its analyses; the variables and statistics of the
    # one chosen (of the ARD made; the analyses sheet's when not made yet)
    ard <- shiny::isolate(fig_ard())
    src <- shiny::isolate(.fig_ard_source(rv$p, current() %||% ""))
    an_ids <- if (!is.null(ard) && nrow(ard)) unique(ard$analysis_id) else
      if (src$kind %in% c("own", "table")) ard_rows(shiny::isolate(rv$p), "analyses", src$id)$analysis_id
    an_now <- .ard_ids(pn$p$analysis_id)
    ard_an <- if (!is.null(ard) && nrow(ard)) ard[ard$analysis_id %in% an_now, , drop = FALSE]
    ard_vars <- if (!is.null(ard_an)) unique(ard_an$variable)
    ard_stat <- if (!is.null(ard_an)) {
      k <- if (!is.null(pn$p$variable)) ard_an$variable %in% pn$p$variable else TRUE
      unique(ard_an$stat_name[k])
    }
    cat_ds <- shiny::isolate(catalog())$dataset
    one <- function(r) {
      v <- pn$p[[r$field]]
      def <- r$default
      # a named step's name: its Out
      as_out <- identical(r$field, "name") && pn$kind %in% names(.pd_named_steps)
      lab <- shiny::tags$span(
        title = if (!is.na(r$help) && nzchar(r$help)) t(r$help),
        if (as_out) t("Out") else t(r$label),
        if (isTRUE(r$required)) shiny::span(class = "text-danger", " *"),
        if (!is.na(def) && !as_out) shiny::tags$small(class = "text-muted", paste0(" (", t("default"), ": ", def, ")")))
      id <- pd_id(r$field)
      # the placeholder: the default, else what the field takes (its help)
      ph <- if (!is.na(def)) def else if (!is.na(r$help) && nzchar(r$help)) t(r$help) else ""
      # From / Into: what they are when not said, as their placeholder
      if (r$field %in% c("in", "out") && !is.null(io_now) && !identical(pn$kind, "ard")) {
        ph <- io_now[[r$field]] %||% ""
        if (is.na(ph)) ph <- ""
        lab <- shiny::tags$span(title = t(r$help), t(r$label))
      }
      sz <- function(ch, multiple = FALSE, create = TRUE) {
        sel_v <- if (multiple) trimws(strsplit(paste(v %||% "", collapse = ","), ",")[[1L]]) else v %||% ""
        sel_v <- sel_v[nzchar(sel_v)]
        ch <- c(if (!multiple) "", ch, sel_v)
        shiny::selectizeInput(id, lab, ch[!duplicated(ch)],
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
        object = sz(if (identical(r$field, "in")) c(objects, tolower(cat_ds)) else objects),
        analysis = sz(an_ids),
        analysis_data = sz(an_data),
        ard_function = sz(ard_fns),
        ard_variable = sz(ard_vars),
        ard_stat = sz(ard_stat),
        ard_stats = sz(ard_stat, multiple = TRUE),
        choice = sz(.pd_choice_words(r, t), create = FALSE),
        shape = sz(c("circle", "square", "diamond", "triangle", "triangle_down", "x",
                     "plus", "dot", "solid_square", "solid_triangle", "star", "open_circle"),
                   create = FALSE),
        number = shiny::numericInput(id, lab, value = if (is.null(v)) NA else as.numeric(v), width = "100%"),
        logical = shiny::checkboxInput(id, lab, value = isTRUE(as.logical(v %||% def))),
        code = shiny::div(class = "pd-code",
          shiny::textAreaInput(id, lab, value = paste(v %||% "", collapse = "\n"),
                               rows = 6, width = "100%")),
        if (identical(r$field, "fn") && identical(pn$kind, "function")) sz(.pd_fn_suggest)
        else shiny::textInput(id, lab, value = paste(v %||% "", collapse = ", "),
                              placeholder = ph, width = "100%"))
      # a function's arguments: their names, under the field
      # a function's arguments, named under its arguments' field
      h <- if (identical(r$field, "args") && pn$kind %in% c("function", "ard")) .pd_fn_args_hint(pn$p$fn)
      if (!is.null(h))
        shiny::tagList(el, shiny::div(class = "form-text small mt-n2 mb-2", sprintf(t("Its arguments: %s"), h)))
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
          choice = shiny::selectizeInput(id, lab, unique(c("", ch, v)), selected = v %||% "", width = "100%",
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
    # the objects it reads and leaves, on one line at its foot (a named
    # step's name is its Out); an ARD analysis reads an analysis data
    foot <- if (!identical(pn$kind, "ard")) c(which(f$field == "in"),
      which(f$field == "out" | (f$field == "name" & pn$kind %in% names(.pd_named_steps))))
    lead <- f$piece_help[1L] %||% ""
    shiny::tagList(
      shiny::h6(if (nzchar(lead)) with_tip(head, t(lead)) else head),
      lapply(setdiff(seq_len(nrow(f)), foot), function(i) one(f[i, ])),
      if (length(foot)) shiny::div(
        class = "d-flex gap-2",
        lapply(foot, function(i) shiny::div(style = "flex: 1 1 0; min-width: 0", one(f[i, ])))),
      # an ARD analysis of a figure that reads a table's ARD: not used yet
      if (identical(pn$kind, "ard") && identical(src$kind, "table")) shiny::p(
        class = "small text-warning",
        sprintf(t("This figure reads %s's ARD: its own analyses are used once it reads its own."), src$id), " ",
        shiny::tags$button(
          type = "button", class = "btn btn-sm btn-outline-secondary py-0",
          onclick = "$('input[name=fig_ard_kind][value=own]').click();",
          t("Read its own ARD"))),
      if (by_cl) shiny::p(class = "small text-muted",
                          sprintf(t("%s has a code list: its values' order and text are the code list's (the data steps' last, Code lists)."),
                                  cl_var)))
  }
  # the code lists' step: the report's code lists of the design's columns
  # (R/codelists.R), its dialog the app's
  cl_ids <- .codelist_part_ids("pd_cl", "clfig", edit = "clfig_open_btn",
                               copy = "clfig_copy_btn")
  cl_data <- function() {
    d <- design()
    ds <- unique(toupper(unlist(lapply(d$data, function(s) s$dataset))))
    dat <- study_data(ds)
    if (length(dat)) do.call(c, lapply(unname(dat), as.list))
  }
  cl_part <- if (!is.null(codelists_dialog)) .codelist_part_server(
    input, output, rv, current, cl_ids, vars = function() design_vars(design()), data = cl_data,
    title = function() sprintf(t("Code lists: the columns of %s's data"), current()),
    dialog = codelists_dialog, t = t, notify = notify, guarded = guarded, bump = bump,
    has_study = has_study, read_grid = read_grid, grids_drawn = grids_drawn)
  output$pd_cl <- shiny::renderUI({
    d <- design()
    shiny::req(d, current(), !is.null(cl_part))
    cl <- sheet_rows(rv$p, "codelists", current())
    shiny::tagList(
      .codelist_part_ui(
        .codelist_lines(cl, design_vars(d)),
        cl_part$missing(), t, cl_ids,
        t("This report's code lists of the figure's columns: its program puts them on the data as the data steps' last (set_levels()), each column a factor in the list's order, its values the labels -- the order and text of the legend and the axis. A value a list does not have stops the program."),
        open = cl_part$open(), others = .codelist_others(cl, design_vars(d))))
  })

  # the form, read back into the piece
  pd_values <- shiny::reactive({
    pd_drawn()
    shiny::req(identical(pd$id, current()))
    f <- pd$fields
    shiny::req(!is.null(f), nrow(f))
    out <- list()
    raw <- list()
    # a field's value, when it differs from what the form drew
    moved <- function(key) {
      v <- input[[pd_id(key)]]
      if (is.null(v)) return(NULL)
      if (is.null(pd$raw[[key]])) {
        pd$raw[[key]] <- list(v)
        return(NULL)
      }
      if (identical(pd$raw[[key]][[1L]], v)) return(NULL)
      raw[[key]] <<- v
      list(v)
    }
    if (!is.null(pd$fig_args)) {
      fa <- pd$fig_args
      for (i in seq_len(nrow(fa))) {
        v <- moved(paste0("arg_", fa$field[i]))
        if (is.null(v)) next
        out[fa$field[i]] <- list(.pd_value(
          if (fa$kind[i] %in% c("variables", "param")) "variables" else fa$kind[i], v[[1L]]))
      }
      shiny::req(length(raw))
      return(list(sel = pd$sel, fields = out, raw = raw, whole = TRUE))
    }
    for (i in seq_len(nrow(f))) {
      v <- moved(f$field[i])
      if (is.null(v)) next
      out[f$field[i]] <- list(.pd_value(f$kind[i], v[[1L]]))
    }
    shiny::req(length(raw))
    list(sel = pd$sel, fields = out, raw = raw)
  })
  pd_values_d <- shiny::debounce(pd_values, 500)
  shiny::observeEvent(pd_values_d(), {
    v <- pd_values_d()
    d <- design()
    if (is.null(d) || !identical(pd$id, current())) return()
    s <- v$sel
    old <- if (s$sec == "plot") d$plot else if (s$i <= length(d[[s$sec]])) d[[s$sec]][[s$i]]
    if (is.null(old)) return()
    for (k in names(v$raw)) pd$raw[[k]] <- list(v$raw[[k]])
    new <- old
    if (isTRUE(v$whole)) {
      # the whole figure's arguments: those changed, the others (one the
      # schema does not list too) as they were
      args <- old$args %||% list()
      for (k in names(v$fields)) {
        if (k == "style") new["style"] <- list(v$fields[[k]]) else args[k] <- list(v$fields[[k]])
      }
      args <- args[!vapply(args, is.null, logical(1))]
      new["args"] <- list(if (length(args)) args)
    } else {
      for (k in names(v$fields)) new[k] <- list(v$fields[[k]])
    }
    new <- new[!vapply(new, is.null, logical(1))]
    if (s$sec == "plot") d$plot <- new else d[[s$sec]][[s$i]] <- new
    if (identical(.fig_norm(unclass(d)), .fig_norm(unclass(design())))) return()
    set_design(d)
    # another dataset changes what the forms offer; another function, the
    # arguments its form names
    # (a join's how: whether its variables are asked)
    if (!identical(old$dataset, new$dataset) || !identical(old$fn, new$fn) ||
        !identical(old$how, new$how)) redraw_form()
  })

  # ---- the drawing ----------------------------------------------------------
  # The figure is drawn for the screen (about 1400 px wide, the saved
  # size), only while this tab shows, and -- unless "Redraw on change" is
  # off -- 0.6 s after the last change.  The code on the right follows
  # every change at once (it takes a few ms).
  pv <- shiny::reactiveVal(NULL)
  drawn <- shiny::reactiveVal(NULL)          # the design the picture shows
  fig_drawn <- new.env(parent = emptyenv())  # each figure's last drawing
  on_tab <- shiny::reactive(identical(page(), "designer"))
  draw <- function() {
    id <- shiny::isolate(current())
    d <- shiny::isolate(design())
    if (is.null(id) || is.null(d) || is.null(shiny::isolate(rv$study))) {
      pv(NULL)
      drawn(NULL)
      return()
    }
    s <- shiny::isolate(rv$study)
    s$planner <- shiny::isolate(rv$p)
    # drawn already from the same design, definition, data and ARD (going
    # back to a figure): that drawing (a drawing takes a second or more)
    data_at <- file.info(list.files(file.path(s$path, "data"), recursive = TRUE,
                                    full.names = TRUE))$mtime
    # (and the study ARD's: a figure may print a table's numbers)
    ard_at <- file.info(file.path(s$path, study_layout()[["ard"]], "ard.rds"))$mtime
    key <- list(.fig_norm(unclass(d)), s$planner, data_at, ard_at)
    hit <- fig_drawn[[id]]
    if (!is.null(hit) && identical(hit$key, key) &&
        (is.null(hit$r$png) || file.exists(hit$r$png))) {
      r <- hit$r
    } else {
      r <- tryCatch(preview_figure(s, id, d, max_px = 1400), error = function(e)
        list(png = NULL, problems = NULL, warnings = character(),
             error = conditionMessage(e), code = NULL))
      # (the app's log keeps why: a drawing that fails can be looked into)
      if (!is.null(r$error)) message("tflplanner: ", id, " could not be drawn: ", r$error)
      fig_drawn[[id]] <- list(key = key, r = r)
    }
    r$id <- id
    pv(r)
    drawn(list(id = id, design = .fig_norm(unclass(d))))
    drawing(FALSE)
  }
  # the form, the code and the checks first: the figure is drawn after the
  # update that shows them (a drawing takes a second or more, and the whole
  # page waited for it), "Drawing ..." in its place meanwhile
  drawing <- shiny::reactiveVal(FALSE)
  form_on_page <- new.env(parent = emptyenv())   # the figure whose form was sent
  draw_after <- function() later::later(function() draw(), 0)
  draw_soon <- function() {
    drawing(TRUE)
    # its form on the page already (a change typed): drawn now; else once
    # the update that sends the form is done (pd_form)
    if (identical(form_on_page$id, shiny::isolate(current()))) draw_after() else
      form_on_page$waiting <- TRUE
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
      draw_soon()
    }
  })
  shiny::observeEvent(input$pd_redraw, draw())
  output$pd_state <- shiny::renderUI({
    if (drawing()) {
      return(shiny::div(class = "small text-muted mb-2",
                        shiny::span(class = "spinner-border spinner-border-sm me-2"),
                        t("Drawing ...")))
    }
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
             sprintf(t("On the page %s x %s in (step 3), %s dpi (shown at screen resolution)"),
                     z$width, z$height, z$dpi))
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
  # the lines of the script that the chosen piece makes (in its step)
  piece_code <- function(step) {
    id <- current()
    d <- design()
    s <- sel()
    shiny::req(id, d)
    if (!.pd_in_step(s, step)) return("")
    # an ARD analysis: no line of the figure's program, its row of the ARD
    # definition (the figure's ARD program makes it)
    p <- if (identical(s$sec, "data") && s$i <= length(d$data)) d$data[[s$i]]
    if (identical(p$step, "ard")) {
      if (is.null(p$fn) || is.null(p$analysis_id)) return(t("(nothing yet: this piece writes no line on its own)"))
      return(paste0("# ", t("Its row of the ARD definition (the figure's ARD program):"), "\n",
                    p$analysis_id, ": ", p$fn, "(",
                    paste(c(p[["in"]] %||% "<data>", if (!is.null(p$args)) paste(p$args, collapse = ", ")),
                          collapse = ", "), ")"))
    }
    cl <- .study_codelists(rv$p)
    code <- tryCatch(.fig_design_script(d, id, codelists = cl),
                     error = function(e) conditionMessage(e))
    if (length(code) == 1L && !grepl("\n", code)) return(code)
    out <- .piece_code(code, d, s, function(d, codelists = TRUE)
      .fig_design_script(d, id, codelists = if (codelists) cl))
    # (a note, not code: in the app's language)
    if (startsWith(out, "(")) t(out) else out
  }
  output$pd_piece_code <- shiny::renderText(piece_code("figure"))
  output$pd_piece_code_data <- shiny::renderText(piece_code("data"))
  output$pd_code <- shiny::renderText({
    id <- current()
    d <- design()
    shiny::req(id, d)
    paste(tryCatch(.fig_design_lines(rv$p, id, d),
                   error = function(e) conditionMessage(e)),
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
