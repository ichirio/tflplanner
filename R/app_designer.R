# The Plot Designer: a figure designed on a form, drawn as it is designed.
#
# Left, the design: the figure type and style, then its arguments by
# section (data, mapping, style, axes, legend, output) -- the basic ones,
# and the advanced ones on request -- each with its default shown, the
# data's variables and PARAMCDs to pick from.  Middle, the figure as its
# program will save it (the PNG, at its size), redrawn on each change.
# Right, what is wrong: the design against the data, the figure checks.
# Below, the code the design makes and the design itself (YAML).
#
# What a design can be comes from tflspec's tfl_fig_schema(); the design is
# the figure's (fig_design()), saved with the study.

.pd_sections <- c(data = "Data", mapping = "Mapping (variables)",
                  style = "Style", axes = "Axes", legend = "Legend",
                  output = "Output")

.designer_ui <- function(t) {
  bslib::nav_panel(
    t("Plot Designer"), value = "designer",
    shiny::uiOutput("pd_note"),
    shiny::uiOutput("pd_body"))
}

# the value an input gives, as a design argument: NULL = not set (default)
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
  if (kind == "named") {
    # "Death = DTHADY | Ongoing = EOSSTT"
    parts <- trimws(unlist(strsplit(v, "|", fixed = TRUE)))
    parts <- parts[grepl("=", parts, fixed = TRUE)]
    if (!length(parts)) return(NULL)
    return(stats::setNames(trimws(sub("^[^=]*=", "", parts)),
                           trimws(sub("=.*$", "", parts))))
  }
  if (kind %in% c("variables")) return(v)
  if (kind == "param" && length(v) > 1L) return(v)
  v[1L]
}

.pd_show <- function(v) {
  if (is.null(v)) return("")
  if (!is.null(names(v))) return(paste(names(v), v, sep = " = ", collapse = " | "))
  v
}

.designer_server <- function(input, output, session, rv, current, t, notify,
                             guarded, catalog) {
  pd <- new.env()
  pd$n <- 0L
  pd$data <- list()
  pd_drawn <- shiny::reactiveVal(0L)
  pd_id <- function(x) paste0("pd", pd$n, "_", x)
  adv <- shiny::reactiveVal(FALSE)
  # the form is drawn again only when what it offers changes: another
  # figure or type, another style, another dataset (its variables), the
  # advanced settings -- never while one types (that would lose the typing)
  form_ver <- shiny::reactiveVal(0L)
  redraw_form <- function() form_ver(form_ver() + 1L)

  design <- shiny::reactive({
    id <- current()
    if (is.null(id) || !identical(report_info(rv$p, id)$type, "figure")) {
      return(NULL)
    }
    fig_design(rv$p, id)
  })
  # what the tab shows, and the form's figure and type: they change less
  # often than the design (each edit), so the page is redrawn only for them
  mode <- shiny::reactiveVal("none")
  form_key <- shiny::reactiveVal(NA_character_)
  shiny::observe({
    id <- current()
    d <- design()
    m <- if (is.null(id)) "none" else
      if (!identical(report_info(rv$p, id)$type, "figure")) "other" else
        if (is.null(d)) "hand" else "design"
    mode(m)
    form_key(if (m == "design") paste(id, d$type) else NA_character_)
  })

  # the study's data, read once a file (and again when it changes)
  study_data <- function(ds) {
    if (is.null(rv$study)) return(list())
    d <- catalog()
    out <- list()
    for (x in ds) {
      pth <- d$path[match(toupper(x), toupper(d$dataset))]
      f <- if (length(pth) == 1L && !is.na(pth)) file.path(rv$study$path, pth)
      k <- if (!is.null(f) && file.exists(f)) paste(f, file.mtime(f))
      if (is.null(k)) next
      if (is.null(pd$data[[k]])) {
        s <- rv$study
        s$planner <- rv$p
        pd$data[[k]] <- .study_data(s, x)[[toupper(x)]]
      }
      if (!is.null(pd$data[[k]])) out[[toupper(x)]] <- pd$data[[k]]
    }
    out
  }

  output$pd_note <- shiny::renderUI({
    m <- mode()
    msg <- if (m == "none") {
      t("Choose a Figure report in the sidebar.")
    } else if (m == "other") {
      t("The Plot Designer designs figures: choose a Figure report in the sidebar.")
    } else if (m == "hand") {
      t("This figure's plot is written by hand (its data code, Reports tab). Start a design to make it from a figure type instead: the plot's code is then made from the design.")
    } else {
      t("The figure is made from its design. Change it on the left: the figure is redrawn as its program will save it. Empty = the default (shown grey).")
    }
    shiny::div(class = "alert alert-info py-2 small", msg)
  })

  types <- shiny::reactive({
    cat <- tflspec::tfl_fig_catalog()
    cat <- cat[cat$status == "implemented", , drop = FALSE]
    first <- !duplicated(cat$type)
    stats::setNames(cat$type[first],
                    paste0(cat$category[first], " - ", cat$type[first]))
  })

  output$pd_body <- shiny::renderUI({
    m <- mode()
    shiny::req(m %in% c("hand", "design"))
    if (m == "hand") {
      return(bslib::card(
        bslib::card_header(t("Start a design")),
        shiny::selectInput("pd_new_type", t("Figure type"), types(),
                           width = "100%"),
        shiny::uiOutput("pd_new_styles"),
        shiny::div(.btn("pd_start", t("Start the design"),
                        class = "btn-sm btn-primary"))))
    }
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(4, 5, 3, 12)),
      bslib::card(
        bslib::card_header(t("Design")),
        shiny::div(
          class = "d-flex justify-content-between align-items-center",
          shiny::checkboxInput("pd_adv", t("Show advanced settings"),
                               value = shiny::isolate(adv())),
          .btn("pd_drop", t("Remove the design"),
               class = "btn-sm btn-outline-danger")),
        shiny::uiOutput("pd_form")),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::div(
          class = "d-flex justify-content-between align-items-center",
          shiny::span(t("Preview (as saved)")),
          .btn("pd_redraw", t("Redraw"), class = "btn-sm btn-outline-primary"))),
        shiny::uiOutput("pd_size"),
        shiny::imageOutput("pd_img", height = "auto")),
      bslib::card(
        bslib::card_header(t("Checks")),
        shiny::uiOutput("pd_checks")),
      bslib::navset_card_tab(
        bslib::nav_panel(t("Code"), shiny::div(
          class = "rp-code", shiny::verbatimTextOutput("pd_code"))),
        bslib::nav_panel(t("Design (YAML)"), shiny::div(
          class = "rp-code", shiny::verbatimTextOutput("pd_yaml")))))
  })
  shiny::observeEvent(input$pd_adv, adv(isTRUE(input$pd_adv)),
                      ignoreInit = TRUE)

  output$pd_new_styles <- shiny::renderUI({
    shiny::req(input$pd_new_type)
    cat <- tflspec::tfl_fig_catalog()
    cat <- cat[cat$status == "implemented" & cat$type == input$pd_new_type, ]
    shiny::radioButtons("pd_new_style", t("Style"),
                        stats::setNames(cat$style,
                                        paste0(cat$style, ": ", cat$description)))
  })

  # a new design: the type's defaults, and the datasets the figure reads
  shiny::observeEvent(input$pd_start, {
    id <- current()
    shiny::req(id, input$pd_new_type)
    d <- guarded(tflspec::tfl_fig_design(input$pd_new_type, input$pd_new_style))
    if (is.null(d)) return()
    rv$p <- set_fig_design(rv$p, id, d)
  })
  shiny::observeEvent(input$pd_drop, {
    shiny::showModal(shiny::modalDialog(
      title = t("Remove the design?"),
      t("The figure's plot is then its data code again (Reports tab), written by hand."),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("pd_drop_ok", t("Remove"),
                                   class = "btn-danger"))))
  })
  shiny::observeEvent(input$pd_drop_ok, {
    shiny::removeModal()
    id <- current()
    shiny::req(id)
    rv$p <- set_fig_design(rv$p, id, NULL)
  })

  schema <- shiny::reactive({
    shiny::req(!is.na(form_key()))
    tflspec::tfl_fig_schema(shiny::isolate(design())$type)
  })

  # the form: one input an argument, by section
  output$pd_form <- shiny::renderUI({
    form_ver()
    form_key()
    d <- shiny::isolate(design())
    shiny::req(d)
    sc <- schema()
    show_adv <- adv()
    pd$n <- pd$n + 1L
    pd_drawn(pd$n)
    pd$id <- shiny::isolate(current())
    pd$args <- sc$arg
    val <- function(a) d$args[[a]]
    data_of <- function(of) {
      nm <- if (is.na(of)) NULL else if (of %in% c("data", "response_data")) {
        val(of) %||% sc$default[sc$arg == of]
      } else of
      if (is.null(nm) || is.na(nm)) return(NULL)
      shiny::isolate(study_data(nm))[[toupper(nm)]]
    }
    cat_ds <- shiny::isolate(catalog())$dataset
    styles <- strsplit(sc$choices[sc$arg == "style"], " | ", fixed = TRUE)[[1L]]
    one <- function(r) {
      a <- r$arg
      v <- val(a)
      def <- r$default
      lab <- shiny::tags$span(
        t(r$label),
        if (!is.na(def)) shiny::tags$small(class = "text-muted",
                                           paste0(" (", t("default"), ": ", def, ")")))
      lab <- shiny::tags$span(title = r$help, lab)
      id <- pd_id(a)
      ph <- if (is.na(def)) "" else def
      switch(r$kind,
        dataset = shiny::selectizeInput(
          id, lab, c("", cat_ds), selected = v %||% "",
          options = list(placeholder = ph)),
        param = {
          dd <- data_of(r$of)
          ch <- if (!is.null(dd) && "PARAMCD" %in% names(dd)) sort(unique(dd$PARAMCD))
          shiny::selectizeInput(id, lab, unique(c("", ch, v)), selected = v %||% "",
                                options = list(placeholder = ph, create = TRUE))
        },
        variable = , flag = , variables = {
          dd <- data_of(r$of)
          ch <- if (!is.null(dd)) names(dd)
          shiny::selectizeInput(id, lab, unique(c(if (r$kind != "variables") "", ch, v)),
                                selected = v %||% if (r$kind != "variables") "",
                                multiple = r$kind == "variables",
                                options = list(placeholder = ph, create = TRUE))
        },
        choice = {
          ch <- if (!is.na(r$choices)) strsplit(r$choices, " | ", fixed = TRUE)[[1L]]
          if (a == "style") {
            shiny::selectInput(id, lab, styles, selected = d$style)
          } else {
            shiny::selectizeInput(id, lab, c("", ch), selected = v %||% "",
                                  options = list(placeholder = ph))
          }
        },
        number = shiny::numericInput(id, lab, value = v %||% NA),
        logical = shiny::checkboxInput(id, lab,
                                       value = isTRUE(v %||% as.logical(def))),
        shiny::textInput(id, lab, value = .pd_show(v), placeholder = ph))
    }
    secs <- lapply(names(.pd_sections), function(s) {
      rows <- sc[sc$section == s, , drop = FALSE]
      # an advanced argument set in the design is always shown
      keep <- rows$level == "basic" | show_adv | rows$arg %in% names(d$args)
      rows <- rows[keep, , drop = FALSE]
      if (!nrow(rows)) return(NULL)
      bslib::accordion_panel(
        t(.pd_sections[[s]]), value = s,
        lapply(seq_len(nrow(rows)), function(i) one(rows[i, ])))
    })
    secs <- secs[!vapply(secs, is.null, logical(1))]
    shiny::tagList(
      shiny::p(class = "small mb-2",
               shiny::strong(d$type), " ",
               shiny::span(class = "text-muted", t("figure type"))),
      do.call(bslib::accordion, c(list(id = "pd_acc", multiple = TRUE,
                                       open = c("data", "mapping")), secs)))
  })

  # the form, read back into the design
  pd_values <- shiny::reactive({
    pd_drawn()
    shiny::req(identical(pd$id, current()))
    d <- shiny::isolate(design())
    shiny::req(d)
    sc <- shiny::isolate(schema())
    args <- d$args
    style <- d$style
    seen <- FALSE
    for (i in seq_len(nrow(sc))) {
      a <- sc$arg[i]
      v <- input[[pd_id(a)]]
      if (is.null(v)) next   # not on the form (advanced, hidden)
      seen <- TRUE
      if (a == "style") {
        style <- v
        next
      }
      args[[a]] <- .pd_value(sc$kind[i], v)
    }
    shiny::req(seen)
    list(style = style, args = args[!vapply(args, is.null, logical(1))])
  })
  pd_values_d <- shiny::debounce(pd_values, 500)
  shiny::observeEvent(pd_values_d(), {
    v <- pd_values_d()
    id <- current()
    d <- design()
    if (is.null(id) || is.null(d) || !identical(pd$id, id)) return()
    new <- guarded(tflspec::tfl_fig_design(d$type, v$style,
                                           .fig_args_norm(v$args)))
    if (is.null(new)) return()
    new$args <- .fig_args_norm(new$args)
    d$args <- .fig_args_norm(d$args)
    if (identical(unclass(new), unclass(d))) return()
    rv$p <- set_fig_design(rv$p, id, new)
    # a new style, or another dataset, changes what the form offers
    ds <- function(x) x$args[c("data", "response_data")]
    if (!identical(new$style, d$style) || !identical(ds(new), ds(d))) {
      redraw_form()
    }
  })

  # the drawing
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
    r$design <- d
    pv(r)
  }
  design_d <- shiny::debounce(design, 700)
  shiny::observeEvent(design_d(), draw(), ignoreNULL = FALSE)
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
                        shiny::strong(t("The figure could not be drawn: ")),
                        r$error))
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
                       shiny::strong(pr$arg[i]), ": ", pr$problem[i])),
      lapply(r$warnings, function(w) shiny::tags$li(class = "text-warning", w)))
    if (!length(items)) {
      return(shiny::p(class = "text-success small", t("No problems found.")))
    }
    shiny::tags$ul(class = "small ps-3", items)
  })
  output$pd_code <- shiny::renderText({
    id <- current()
    d <- design()
    shiny::req(id, d)
    paste(.fig_design_plot(d, id), collapse = "\n")
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
