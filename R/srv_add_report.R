# app_server(): add report -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_add_report <- function() {
  # the new report's section, as the Add dialog gave it
  put_section <- function(id) {
    s <- trimws(input$modal_section %||% "")
    i <- match(id, rv$p$outputs$output_id)
    if (nzchar(s) && !is.na(i)) rv$p$outputs$section[i] <- s
    # and its analysis set (its data made)
    pop <- input$modal_pop %||% ""
    if (nzchar(pop) && !is.na(i)) {
      p <- guarded(set_report_population(rv$p, id, pop))
      if (!is.null(p)) {
        attr(p, "made") <- attr(p, "left") <- NULL
        rv$p <- p
      }
    }
  }
  shiny::observeEvent(input$add_ok, {
    id <- .id_with_prefix(input$modal_id, input$modal_type)
    # (a new report until the study is saved: the usual cards, empty -- #440)
    bnew$ids <- unique(c(bnew$ids, id))
    d <- trimws(input$modal_desc)
    desc <- if (nzchar(d)) d else NA
    if (identical(input$modal_how, "template")) return(add_template_report(id, desc))
    if (identical(input$modal_type, "listing") && isTRUE(input$modal_first_l)) {
      on.exit(put_section(id), add = TRUE)
      return(start_first_listing(id, desc))
    }
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
  tpl_data <- function() .tpl_data_cols(rv$study$path)
  tpl_all_cols <- function() unique(unlist(lapply(tpl_data(), names)))
  # an argument's heading: "meaning (name)", its doc in the (i)
  # (.tpl_arg_labels; the doc itself when it has none)
  tpl_label <- function(prm) {
    doc <- prm$doc %||% ""
    short <- if (doc %in% names(.tpl_doc_labels)) .tpl_doc_labels[[doc]] else .tpl_arg_labels[prm$name]
    head <- if (!is.na(short)) t(short) else if (nzchar(doc)) t(doc) else ""
    lab <- if (nzchar(head)) sprintf(t("%s (%s)"), head, prm$name) else prm$name
    shiny::span(lab, if (isTRUE(prm$required)) shiny::span(class = "text-danger", " *"),
                if (!is.na(short) && nzchar(doc)) help_tip(t(doc)))
  }
  # the input of an argument, by its type (.tpl_type(): tflspec's
  # tfl_param(type = ), guessed only when it declares none).  `params`,
  # `args`: the template's parameters and the report's arguments (a levels
  # parameter's variable is another's value)
  tpl_widget <- function(input_id, prm, value, dataset = NULL, params = list(), args = list(),
                         label = tpl_label(prm)) {
    data <- tpl_data()
    type <- .tpl_type(prm, tpl_all_cols())
    cols <- .tpl_columns(data, dataset)
    shown <- if (is.null(value)) prm$default else value
    lab <- label
    # (a value taken away with x: the list closed again)
    shut <- I("function() { this.close(); this.blur(); }")
    rm_btn <- list(plugins = list("remove_button"), onItemRemove = shut)
    switch(type,
      population = shiny::selectInput(input_id, lab, pop_choices(),
                                      selected = as.character(shown %||% ""), width = "100%"),
      data = shiny::selectInput(
        input_id, lab,
        c(if (is.null(shown)) stats::setNames("", t("(default)")),
          union(as.character(shown %||% character()), names(data))),
        selected = as.character(shown %||% "")[1L], width = "100%"),
      levels = {
        vr <- .tpl_levels_of(prm, params, args)
        lv <- .tpl_levels(data, vr, dataset)
        several <- isTRUE(prm$multiple)
        shiny::selectizeInput(
          input_id, lab, c(if (!several) "", union(as.character(shown %||% character()), lv)),
          selected = as.character(shown %||% if (several) character() else ""),
          multiple = several, width = "100%",
          options = c(if (several) rm_btn, list(create = TRUE)))
      },
      flag = bslib::input_switch(input_id, lab, value = isTRUE(shown)),
      number = shiny::numericInput(input_id, lab, value = shown, width = "100%", updateOn = "blur"),
      vars = shiny::selectizeInput(input_id, lab, union(as.character(shown), cols),
                                   selected = as.character(shown), multiple = TRUE,
                                   width = "100%",
                                   options = list(plugins = list("remove_button", "drag_drop"),
                                                  onItemRemove = shut)),
      var = shiny::selectizeInput(input_id, lab, c("", union(as.character(shown %||% character()), cols)),
                                  selected = as.character(shown %||% ""), width = "100%"),
      chips = shiny::selectizeInput(
        input_id, lab, as.character(shown), selected = as.character(shown), multiple = TRUE,
        width = "100%", options = c(rm_btn, list(create = TRUE))),
      choice = shiny::selectInput(
        input_id, lab, c(if (is.null(shown)) stats::setNames("", t("(default)")),
                         union(as.character(shown %||% character()), as.character(prm$choices))),
        selected = as.character(shown %||% "")[1L], width = "100%"),
      choices = shiny::selectizeInput(
        input_id, lab, union(as.character(shown), as.character(prm$choices)),
        selected = as.character(shown), multiple = TRUE, width = "100%", options = rm_btn),
      text = shiny::textInput(input_id, lab, value = if (is.null(shown)) "" else .tpl_show(shown),
                              width = "100%", updateOn = "blur",
                              placeholder = if (is.null(prm$default)) t("(default)")),
      list = {
        txt <- .tpl_list_text(shown, prm)
        if (is.null(txt)) {
          shiny::div(
            class = "mb-3",
            shiny::tags$label(class = "form-label", lab),
            shiny::div(class = "font-monospace small border rounded p-1", style = "word-break: break-all",
                       .tpl_show(shown)),
            shiny::div(class = "small text-muted", t("Edit in R (the report's YAML file)")))
        } else {
          shiny::div(
            shiny::textAreaInput(
              input_id, lab, value = txt, width = "100%", updateOn = "blur",
              rows = max(2L, min(8L, length(strsplit(txt, "\n")[[1L]]) + 1L)),
              placeholder = paste(.tpl_list_fields(prm), collapse = " | ")),
            shiny::div(class = "small text-muted mt-n2 mb-2",
                       sprintf(t("A line a row: %s"), paste(.tpl_list_fields(prm), collapse = " | "))))
        }
      })
  }
  # The new report's dialog: the templates of each kind, a searchable list
  # with a line on each; the required arguments of each template, below it
  # -- all drawn with the dialog (shown by the choice in the page itself:
  # nothing is waited for from the server)
  tpl_input <- function(kind) paste0("modal_tpl_", kind)
  tpl_arg_id <- function(nm, a) paste0("mtpl_", nm, "__", gsub("[^A-Za-z0-9_]", "_", a))
  tpl_chosen <- function() {
    kind <- input$modal_type %||% "table"
    nm <- input[[tpl_input(kind)]]
    if (is.null(nm) || !nzchar(nm)) NULL else nm
  }
  tpl_modal_ui <- function() {
    lapply(report_types(), function(kind) {
      tp <- .tpl_list(kind)
      shiny::conditionalPanel(
        sprintf("input.modal_type == '%s'", kind),
        if (!nrow(tp)) {
          shiny::div(class = "alert alert-secondary py-1 small",
                     t("No template of this kind: choose Code."))
        } else shiny::tagList(
          shiny::selectizeInput(
            tpl_input(kind), t("Template"),
            stats::setNames(tp$name, vapply(tp$name, function(n) {
              d <- if (!is.na(.tpl_desc[n])) t(.tpl_desc[[n]]) else ""
              if (nzchar(d)) paste0(n, " \u2014 ", d) else n
            }, "")),
            selected = tp$name[1L], width = "100%",
            options = list(searchField = list("label", "value"))),
          lapply(tp$name, function(nm) {
            prm_all <- .tpl_params(nm)
            prm <- .tpl_shown(prm_all)
            req_p <- Filter(function(x) isTRUE(x$required), prm)
            shiny::conditionalPanel(
              sprintf("input.modal_type == '%s' && input['%s'] == '%s'", kind, tpl_input(kind), nm),
              if (.tpl_own_data(prm_all)) tpl_modal_data_ui(nm),
              if (!length(req_p)) {
                shiny::p(class = "small text-muted",
                         t("No argument is required: every one has its default."))
              } else shiny::div(
                class = "border rounded p-2 mb-2",
                shiny::h6(t("Required arguments")),
                # (the study's value first: its treatment for `by`)
                lapply(req_p, function(x) tpl_widget(tpl_arg_id(nm, x$name), x,
                                                     .tpl_study_value(rv$p$spec$study, x),
                                                     dataset = prm$data$default, params = prm))))
          })))
    })
  }
  # a template whose data stage is the report's (a KM type): the report's
  # data asked in the dialog -- the dataset, its parameter, the analysis
  # set -- written as its `data:` steps
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
        shiny::updateSelectizeInput(session, pid, choices = c("", pcd), selected = "")
      }, ignoreInit = TRUE)
    }
    pcd <- d[[first]]$PARAMCD
    pcd <- if (is.null(pcd)) character() else sort(unique(as.character(stats::na.omit(pcd))))
    shiny::div(
      class = "border rounded p-2 mb-2",
      shiny::h6(t("Data (the report's data: steps)")),
      shiny::div(
        class = "d-flex flex-wrap gap-2",
        shiny::div(style = "flex: 1 1 140px",
                   shiny::selectizeInput(did, t("Dataset (read)"), c("", ds_all), selected = first,
                                         width = "100%", options = list(create = TRUE))),
        shiny::div(style = "flex: 1 1 140px",
                   shiny::selectizeInput(pid, t("Parameter (param)"), c("", pcd),
                                         width = "100%", options = list(create = TRUE))),
        shiny::div(style = "flex: 1 1 140px",
                   shiny::selectInput(tpl_arg_id(nm, ".data_pop"), t("Analysis set"),
                                      tpl_pop_choices(), width = "100%"))))
  }
  # the dialog's data as `data:` steps and the population (NULL: not a
  # template of the report's data; `missing`: what is still to say)
  tpl_modal_data <- function(nm) {
    if (!.tpl_own_data(.tpl_params(nm))) return(NULL)
    ds <- toupper(trimws(input[[tpl_arg_id(nm, ".data_ds")]] %||% ""))
    par <- trimws(input[[tpl_arg_id(nm, ".data_param")]] %||% "")
    pid <- input[[tpl_arg_id(nm, ".data_pop")]] %||% ""
    if (!nzchar(ds)) return(structure(list(), missing = t("Dataset (read)")))
    if (ds != "ADSL" && !nzchar(par)) return(structure(list(), missing = t("Parameter (param)")))
    # (a flag the study does not define: the report's own join on it, no
    # population)
    if (startsWith(pid, "flag:")) {
      fl <- sub("^flag:", "", pid)
      pop <- list(population_id = NA_character_, dataset = "ADSL", where = sprintf("%s == \"Y\"", fl))
      return(list(steps = .tpl_steps_from(ds, pop, par), population = NULL))
    }
    pops <- rv$p$ard$populations
    k <- if (!is.null(pops) && nzchar(pid)) match(pid, pops$population_id) else NA
    pop <- if (!is.na(k)) as.list(pops[k, intersect(c("population_id", "dataset", "where"), names(pops)),
                                       drop = FALSE])
    list(steps = .tpl_steps_from(ds, pop, par), population = if (nzchar(pid)) pid)
  }
  # the study's populations, then ADSL's Y/N flags none of them says
  # ("flag:<variable>", marked: not defined in the study)
  tpl_pop_choices <- function() {
    fl <- .tpl_flags_undefined(tpl_data()[["ADSL"]], rv$p$ard$populations)
    c(pop_choices(), stats::setNames(paste0("flag:", fl),
                                     sprintf("%s == \"Y\" %s", fl, t("(not defined in the study)"))))
  }
  # the required arguments the dialog has (NULL: one is empty)
  tpl_modal_args <- function(nm) {
    args <- list()
    for (x in .tpl_shown(.tpl_params(nm))) {
      if (!isTRUE(x$required)) next
      v <- .tpl_value(input[[tpl_arg_id(nm, x$name)]], .tpl_type(x), x)
      if (is.null(v)) return(structure(list(), missing = x$name))
      # (the study's own value, as prefilled: left to the study)
      sv <- .tpl_study_value(rv$p$spec$study, x)
      if (!is.null(sv) && identical(as.character(unlist(v)), as.character(unlist(sv)))) next
      args[[x$name]] <- v
    }
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
    notify(sprintf(t("%s is a code report now: its items are on the usual cards (save to keep it)."), id))
  })
  shiny::observeEvent(input$tpl_goto_card, {
    go(switch(report_kind(), listing = "lf", figure = "designer", "tables"))
    session$onFlushed(function() session$sendCustomMessage("rp-tpl-card-show", TRUE), once = TRUE)
  })
  # the page told at once that a report is being opened, and whether it is
  # a template report (.tpl_js): its steps hidden until drawn
  tpl_names <- function(tpl, kind) {
    own <- .step_labels_of[[kind]] %||% character()
    nm <- .step_labels
    nm[names(own)] <- own
    if (isTRUE(tpl)) nm[names(.tpl_step_labels)] <- .tpl_step_labels
    stats::setNames(as.list(t(unname(nm))), names(nm))
  }
  # (`ready`: "Loading ..." taken away after this flush; a report being
  # added waits for its own -- tpl_pending)
  tpl_pending <- new.env()
  tpl_mode_send <- function(tpl, kind = "", loading = TRUE, ready = loading, id = NULL) {
    session$sendCustomMessage("rp-tpl-mode", list(tpl = isTRUE(tpl), loading = loading,
                                                  names = tpl_names(tpl, kind), id = id))
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
    if (identical(id, current()) && !is.null(tp) && !length(.tpl_missing(tp$name, tp$args, rv$p$spec$study))) {
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
  add_template_report <- function(id, desc) {
    .tpl_trace("OK", start = TRUE)
    # (not made: the page's "Loading ..." taken away again)
    fail <- function(msg, type) {
      session$sendCustomMessage("rp-report-ready", TRUE)
      notify(msg, type)
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
    kind <- input$modal_type
    sec <- trimws(input$modal_section %||% "")
    # (the page shows the new report's steps from its first draw)
    tpl_mode_send(TRUE, kind, ready = FALSE, id = id)
    tpl_pending$id <- id
    if (identical(kind, "table")) tpl_autoprev$want <- id
    # (a KM type: its data the report's own `data:` steps, from the dialog)
    dat <- tpl_modal_data(nm)
    pop <- as.character(dat$population %||% args$population %||% NA_character_)
    o <- tflspec::tfl_output(id, kind, label = if (is.na(desc)) NA_character_ else desc,
                             population = pop, template = list(name = nm, args = args),
                             data = if (length(dat$steps)) list(steps = dat$steps))
    # (straight into the spec: its toc line and its file, the view of
    # that report alone -- .spec_add_output())
    p2 <- guarded(.spec_add_output(rv$p, o, label = desc,
                                   section = if (nzchar(sec)) sec else NA_character_,
                                   population = pop))
    .tpl_trace("written into the spec")
    if (is.null(p2)) {
      tpl_pending$id <- NULL
      return(tpl_mode_send(!is.null(.tpl_of(rv$p, current() %||% "")), report_kind()))
    }
    after_id_change(p2, id)
    if (!is.null(p2)) go(switch(kind, table = "tables", listing = "lf", figure = "designer", "outputs"))
  }
  # The template's card: the required arguments and those the report gives
  # above, the others (their defaults, greyed) folded below; each with
  # where its value comes from.  Its own output (tpl_card, tpl_card_lf,
  # tpl_card_fig), whatever step it is put in.
  tpl_ver <- shiny::reactiveVal(0L)
  tpl_obs <- new.env()
  tpl_iid <- function(name, a) paste0("tpl_", name, "__", gsub("[^A-Za-z0-9_]", "_", a))
  tpl_write <- function(id, args, name = NULL, steps = NULL) {
    o <- .tpl_output(rv$p, id, args, name = name)
    if (length(steps)) o$data$steps <- steps
    p2 <- guarded(.spec_set_output(rv$p, id, o))
    if (is.null(p2)) return(invisible(FALSE))
    rv$p <- p2
    rv$bver <- rv$bver + 1L
    rv$status_ver <- rv$status_ver + 1L
    tpl_ver(tpl_ver() + 1L)
    bump()
    invisible(TRUE)
  }
  tpl_edit <- function(name, a, v) {
    id <- current()
    tp <- .tpl_of(rv$p, id %||% "")
    if (is.null(tp) || !identical(tp$name, name)) return()
    prm <- .tpl_params(name)[[a]]
    type <- .tpl_type(prm, tpl_all_cols())
    if (identical(type, "list") && is.null(.tpl_list_text(tp$args[[a]] %||% prm$default, prm))) return()
    val <- .tpl_value(v, type, prm)
    # (a field drawn again says its value back: what it shows already --
    # the report's, the study's or the default -- is no edit)
    shown <- tp$args[[a]] %||% .tpl_study_value(rv$p$spec$study, prm) %||% prm$default
    same <- if (identical(type, "list")) {
      identical(.tpl_list_text(val, prm), .tpl_list_text(shown, prm))
    } else !is.null(val) && identical(as.character(unlist(val)), as.character(unlist(shown)))
    if (same) return()
    args <- .tpl_set_arg(tp$args, prm, val)
    if (identical(args, tp$args)) return()
    tpl_write(id, args)
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
  # dropped -- said by name before; the report's own items stay
  shiny::observeEvent(input$tpl_switch, {
    id <- current()
    tp <- .tpl_of(rv$p, id %||% "")
    to <- input$tpl_switch
    if (is.null(tp) || !length(to) || !nzchar(to) || identical(to, tp$name)) return()
    to_prm <- .tpl_params(to)
    keep <- intersect(names(tp$args), names(.tpl_shown(to_prm)))
    # (to a KM type: the data stage goes to the report's `data:` steps)
    moved <- if (.tpl_own_data(to_prm)) intersect(names(tp$args), c("data", "param", "population"))
    drop <- setdiff(names(tp$args), c(keep, moved))
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Change the template to %s"), to),
      shiny::p(t("Arguments kept:"), " ",
               if (length(keep)) shiny::code(paste(keep, collapse = ", ")) else t("(none)")),
      if (length(moved)) shiny::p(t("To the report's data: steps:"), " ",
                                  shiny::code(paste(moved, collapse = ", "))),
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
  shiny::observeEvent(input$tpl_switch_cancel, tpl_ver(tpl_ver() + 1L))
  shiny::observeEvent(input$tpl_switch_ok, {
    shiny::removeModal()
    id <- current()
    tp <- .tpl_of(rv$p, id %||% "")
    to <- input$tpl_switch
    if (is.null(tp) || is.null(to)) return()
    to_prm <- .tpl_params(to)
    args <- tp$args[intersect(names(tp$args), names(.tpl_shown(to_prm)))]
    # (to a KM type: the old template's data stage -- its dataset, its
    # parameter, the report's analysis set -- becomes the report's `data:`
    # steps, written with the template in one go; steps of the report's
    # own that say a parameter are kept)
    steps <- NULL
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
        steps <- .tpl_steps_from(ds, tpl_data_pop(id), par)
      }
    }
    tpl_write(id, args, name = to, steps = steps)
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
    card <- tpl_card_draw(id)
    tpl_card_drawn(id)
    card
  }
  # A template whose data stage is the report's (a KM type): the report's
  # `data:` steps -- the dataset, its analysis set's subjects (the report's
  # population), the parameter, a filter -- written by Apply.  Steps the
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
      shiny::selectInput("tpl_data_pop", t("Analysis set"), tpl_data_pop_choices(id, f),
                         selected = tpl_data_pop_sel(id, f), width = "100%"),
      shiny::textInput("tpl_data_filter", t("Subset"), f$filter, width = "100%",
                       placeholder = t("an R condition on the records, e.g. AVAL > 0")),
      shiny::div(class = "small text-muted mb-1",
                 if (is.null(pop) && f$join) {
                   sprintf(t("Analysis set: the report's own join (%s: %s)."), f$join_dataset, f$join_where)
                 } else if (is.null(pop)) t("Analysis set: none (the report has no population).") else
                   sprintf(t("Analysis set: %s (%s: %s), its subjects kept (join)."),
                           pop$population_id, pop$dataset, pop$where %||% "")),
      if (nzchar(f$dataset) && .tpl_join_stale(f, pop)) shiny::div(
        class = "alert alert-warning py-1 small",
        t("The analysis set changed: Apply writes the new join.")),
      .btn("tpl_data_apply", t("Apply"), class = "btn-sm btn-outline-primary"),
      if (length(steps)) shiny::tags$details(
        class = "mt-1",
        shiny::tags$summary(class = "small", t("The steps written")),
        shiny::tags$pre(class = "small mb-0", yaml::as.yaml(list(data = list(steps = steps))))))
  }
  # the card's analysis-set choice: the report's population, else its own
  # join -- a flag ("flag:<variable>") or as it is (".own") -- else none
  tpl_data_pop_sel <- function(id, f) {
    pid <- rv$p$spec$outputs[[id]]$population
    if (length(pid) && !is.na(pid[1L]) && nzchar(pid[1L])) return(as.character(pid[1L]))
    if (!f$join) return("")
    fl <- .pop_flag(f$join_where)
    if (!is.na(fl) && identical(toupper(f$join_dataset), "ADSL")) paste0("flag:", fl) else ".own"
  }
  tpl_data_pop_choices <- function(id, f) {
    ch <- tpl_pop_choices()
    # (the report's own join, when it has no population of the study)
    if (f$join && is.null(tpl_data_pop(id))) {
      fl <- .pop_flag(f$join_where)
      own <- if (!is.na(fl) && identical(toupper(f$join_dataset), "ADSL")) paste0("flag:", fl) else ".own"
      if (!own %in% ch) {
        ch <- c(ch, stats::setNames(own, sprintf("%s (%s: %s)", t("The report's own join"),
                                                  f$join_dataset, f$join_where)))
      }
    }
    ch
  }
  tpl_data_pop <- function(id) {
    pid <- rv$p$spec$outputs[[id]]$population
    pops <- rv$p$ard$populations
    if (is.null(pid) || is.null(pops) || !nrow(pops)) return(NULL)
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
    # the analysis set chosen: the study's (the report's population), a flag
    # the study does not define, the report's own join as it is (".own"),
    # or none
    sel <- input$tpl_data_pop %||% tpl_data_pop_sel(id, f)
    pop <- NULL
    if (identical(sel, ".own")) {
      pop <- list(dataset = f$join_dataset, where = f$join_where)
      o$population <- NA_character_
    } else if (startsWith(sel, "flag:")) {
      pop <- list(dataset = "ADSL", where = sprintf("%s == \"Y\"", sub("^flag:", "", sel)))
      o$population <- NA_character_
    } else if (nzchar(sel)) {
      o$population <- sel
      pops <- rv$p$ard$populations
      k <- match(sel, pops$population_id)
      if (!is.na(k)) pop <- as.list(pops[k, intersect(c("population_id", "dataset", "where"), names(pops)),
                                         drop = FALSE])
    } else {
      o$population <- NA_character_
    }
    steps <- .tpl_steps_from(input$tpl_data_ds, pop, input$tpl_data_param %||% "",
                             input$tpl_data_filter %||% "")
    if (is.null(steps)) o$data$steps <- NULL else o$data$steps <- steps
    if (!length(o$data)) o$data <- NULL
    p2 <- guarded(.spec_set_output(.spec_set_toc_population(rv$p, id, o$population), id, o))
    if (is.null(p2)) return()
    rv$p <- p2
    rv$bver <- rv$bver + 1L
    rv$status_ver <- rv$status_ver + 1L
    tpl_ver(tpl_ver() + 1L)
    bump()
  })
  tpl_card_draw <- function(id) {
    tpl_ver()
    rv$ver
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
    up <- names(prm)[vapply(prm, function(x) isTRUE(x$required) || x$name %in% names(args), NA)]
    low <- setdiff(names(prm), up)
    miss <- .tpl_missing(tp$name, args, p$spec$study)
    kind <- report_info(p, id)$type
    study <- p$spec$study
    # where a value comes from: this report / the study standard / the
    # function's default (and the report's over the study's, said)
    filled <- .tpl_filled(p, id)
    from_of <- function(a) {
      og <- .tpl_arg_origin(study, prm[[a]], args, filled)
      sv <- og$study
      if (identical(og$from, "own")) {
        return(shiny::span(class = "rp-tpl-arg-from",
                           t("This table"),
                           if (!is.null(sv)) paste0(" \u2014 ", sprintf(t("over the study standard (%s)"),
                                                                       .tpl_show(sv)))))
      }
      # (the study standard, and where in it: "Study standard
      # (settings.treatment)")
      std <- if (!is.na(og$source)) sprintf(t("Study standard (%s)"), og$source) else t("Study standard")
      shiny::span(class = "rp-tpl-arg-from",
                  if (identical(og$from, "study")) paste0(std, ": ", .tpl_show(sv)) else
                    t("The function's default"))
    }
    row <- function(a, upper) {
      x <- prm[[a]]
      og <- .tpl_arg_origin(study, x, args, filled)
      sv <- if (identical(og$from, "study")) og$study
      shiny::div(
        class = paste("d-flex gap-2 align-items-start", if (!upper) "rp-tpl-default"),
        shiny::div(style = "flex: 1 1 auto; min-width: 0",
                   tpl_widget(tpl_iid(tp$name, a), x, args[[a]] %||% sv, dataset, params = prm,
                              args = args,
                              label = shiny::span(tpl_label(x), " ", from_of(a)))),
        if (upper && !isTRUE(x$required)) shiny::tags$button(
          type = "button", class = "btn btn-sm btn-outline-secondary mt-4",
          title = t("Back to the default (the line is taken out of args)"),
          onclick = sprintf("Shiny.setInputValue('tpl_revert', '%s', {priority: 'event'})", a),
          t("Revert")))
    }
    open <- isTRUE(shiny::isolate(input$tpl_open))
    # (a figure's preview is under the card; a table's and a listing's in
    # the right pane)
    preview_btn <- if (identical(kind, "figure")) {
      .btn("tpl_fig_preview", t("Update preview"), class = "btn-sm btn-primary")
    }
    others <- .tpl_list(kind)$name
    bslib::card(
      class = "mb-2 border-primary",
      bslib::card_header(shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-center",
        shiny::strong(t("Template")),
        shiny::code(.tpl_call_name(tp$name)),
        shiny::span(class = "badge text-bg-light border", paste0("template:", tp$name),
                    title = sprintf(t("Saved in spec/outputs/%s.yml as template: name and its args only."), id)),
        # (another template: chosen here, confirmed in a dialog)
        shiny::tags$select(
          class = "form-select form-select-sm ms-auto", style = "width: auto",
          title = t("Change the template"),
          onchange = "Shiny.setInputValue('tpl_switch', this.value, {priority: 'event'})",
          lapply(others, function(n) shiny::tags$option(
            value = n, selected = if (identical(n, tp$name)) NA, n))),
        # (one way: the template's items written into the report as code)
        .btn("tpl_to_code", paste0(t("Convert to code"), " (tfl_template_to_spec)"),
             class = "btn-sm btn-outline-primary"),
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
        if (!is.na(.tpl_desc[tp$name])) shiny::div(class = "small text-muted w-100", t(.tpl_desc[[tp$name]])))),
      bslib::card_body(
        if (length(miss)) shiny::div(
          class = "alert alert-danger py-1 small",
          sprintf(t("Required arguments are empty: %s. The preview waits for them."),
                  paste(miss, collapse = ", "))),
        if (own_data) tpl_data_block(id, steps),
        lapply(up, row, upper = TRUE),
        shiny::tags$details(
          class = "mt-1", open = if (open) NA,
          ontoggle = "Shiny.setInputValue('tpl_open', this.open)",
          shiny::tags$summary(class = "small", sprintf(t("Arguments with defaults (%d)"), length(low))),
          shiny::div(class = "mt-2", lapply(low, row, upper = FALSE))),
        if (!is.null(preview_btn)) shiny::div(class = "mt-2", preview_btn)))
  }
  # a template figure: drawn here from the template's items
  # (tflspec::tfl_content_run(), the figure as its program makes it), the
  # PNG under the card and the expanded code folded below it
  tpl_fig <- shiny::reactiveVal(NULL)
  tpl_fig_draw <- function() {
    id <- current()
    p <- rv$p
    if (is.null(id) || is.null(.tpl_of(p, id))) return()
    miss <- .tpl_missing(.tpl_of(p, id)$name, .tpl_of(p, id)$args, p$spec$study)
    if (length(miss)) {
      return(tpl_fig(list(id = id, error = sprintf(
        t("Required arguments are empty: %s. The preview waits for them."), paste(miss, collapse = ", ")))))
    }
    res <- shiny::withProgress(message = sprintf(t("Preview of %s"), id), tryCatch({
      r <- tflspec::tfl_resolve_output(p$spec$outputs[[id]], p$spec$study)
      d <- tpl_data()
      names(d) <- tolower(names(d))
      g <- tflspec::tfl_content_run(r, d)
      f <- tempfile("tplfig", fileext = ".png")
      save <- getExportedValue("ggplot2", "ggsave")
      save(f, g, width = 7.3, height = 4.2, dpi = 110)
      list(id = id, png = f, code = paste(tflspec::tfl_content_code(r), collapse = "\n"))
    }, error = function(e) list(id = id, error = conditionMessage(e))))
    tpl_fig(res)
  }
  shiny::observeEvent(input$tpl_fig_preview, tpl_fig_draw())
  output$tpl_fig_out <- shiny::renderUI({
    if (!identical(report_kind(), "figure")) return(NULL)
    id <- current()
    if (is.null(id) || is.null(.tpl_of(shiny::isolate(rv$p), id))) return(NULL)
    r <- tpl_fig()
    body <- if (is.null(r) || !identical(r$id, id)) {
      shiny::p(class = "small text-muted", t("Update preview draws the figure."))
    } else if (!is.null(r$error)) {
      shiny::div(class = "alert alert-danger py-2 small",
                 shiny::strong(t("The figure could not be drawn: ")), r$error)
    } else {
      bytes <- readBin(r$png, "raw", file.size(r$png))
      shiny::tagList(
        shiny::tags$img(src = paste0("data:image/png;base64,", jsonlite::base64_enc(bytes)),
                        style = "max-width: min(100%, 760px); border: 1px solid #dee2e6"),
        shiny::tags$details(
          class = "mt-2",
          shiny::tags$summary(class = "small", t("The expanded code")),
          shiny::div(class = "rp-code", shiny::tags$pre(class = "small", r$code))))
    }
    bslib::card(
      class = "mb-2",
      bslib::card_header(t("Preview")),
      bslib::card_body(body))
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
  # (drawn on any step: its draw ends "Loading ...")
  for (o in c("tpl_card", "tpl_card_lf", "tpl_card_fig")) {
    shiny::outputOptions(output, o, suspendWhenHidden = FALSE)
  }
  # The right pane of a template table: ARD (the ARD its template makes,
  # as the preview made it), and in Code the ARD's code expanded first,
  # the content's expanded code folded after the program
  tpl_now <- shiny::reactive({
    id <- current()
    if (is.null(id)) return(NULL)
    tp <- .tpl_of(rv$p, id)
    if (is.null(tp)) NULL else id
  })
  output$tpl_ard_pane <- shiny::renderUI({
    id <- tpl_now()
    shiny::req(id)
    shiny::tagList(
      shiny::p(class = "small text-muted mb-1",
               t("The ARD the template makes: Update preview makes it with the table.")),
      shiny::div(class = "rp-resize", DT::DTOutput("tpl_ard_table", height = "auto", fill = FALSE)))
  })
  output$tpl_ard_table <- DT::renderDT({
    id <- tpl_now()
    shiny::req(id)
    rv$ard_ver
    r <- ard_res()
    a <- if (!is.null(r) && identical(r$scope, id)) r$ard else
      tryCatch(ard_raw(current_study(), id, rows = TRUE), error = function(e) NULL)
    shiny::validate(shiny::need(!is.null(a), t("No ARD yet: Update preview makes it.")))
    DT::datatable(ard_view(a), rownames = FALSE, filter = "top",
                  selection = "none", class = "compact stripe nowrap", fillContainer = FALSE,
                  options = list(pageLength = 50, scrollX = TRUE, scrollY = "55vh",
                                 scrollCollapse = TRUE, dom = "tip"))
  })
  output$tpl_code_ard <- shiny::renderUI({
    id <- tpl_now()
    shiny::req(id)
    a <- structure(rv$p$ard, class = "tfl_ard_spec")
    code <- if (any(a$analyses$output_id %in% id)) tryCatch(
      ard_program_code(a, id, dir = rv$study$path, codelists = .study_codelists(rv$p)),
      error = function(e) paste(t("The code cannot be written yet:"), conditionMessage(e)))
    if (is.null(code)) return(NULL)
    shiny::div(
      class = "mb-2",
      shiny::tags$details(
        shiny::tags$summary(class = "small fw-bold", t("The code that makes the ARD (expanded)")),
        shiny::div(class = "rp-code", shiny::tags$pre(class = "small rp-flow", paste(code, collapse = "\n")))),
      shiny::h6(class = "small fw-bold mb-1 mt-2", t("The table's program")))
  })
  # SPEC: the report's YAML file as written, and expanded (the template's
  # items with the report's over them), read only
  output$tpl_spec_yaml <- shiny::renderUI({
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
  output$tpl_code_expanded <- shiny::renderUI({
    id <- tpl_now()
    shiny::req(id)
    sp <- tryCatch(.code_spec(rv$p), error = function(e) NULL)
    o <- sp$outputs[[id]]
    shiny::req(!is.null(o))
    code <- tryCatch({
      b <- tflspec::tfl_template_to_spec(o, sp$study)
      paste(tflspec::tfl_content_code(tflspec::tfl_resolve_output(b, sp$study), pipe = "|>"),
            collapse = "\n")
    }, error = function(e) conditionMessage(e))
    shiny::tags$details(
      class = "mt-2",
      shiny::tags$summary(class = "small", t("The content's code expanded")),
      shiny::div(class = "rp-code", shiny::tags$pre(class = "small rp-flow", code)))
  })
  # -- the first listing: the data and its columns --------------------------
  first_data_l <- shiny::reactive({
    shiny::req(input$ml_data %in% first_files())
    guarded(read_data_head(.data_file(rv$study$path, input$ml_data), n = 2000L))
  })
  output$modal_first_l_ui <- shiny::renderUI({
    f <- first_files()
    if (!length(f)) {
      return(shiny::div(class = "alert alert-warning py-1 small",
        t("No data files yet: put them on the Data tab (data/adam), then add the listing.")))
    }
    shiny::tagList(
      shiny::selectInput("ml_data", t("Data"), f,
                         selected = c(grep("adae", f, value = TRUE), f)[1L],
                         width = "100%"),
      shiny::uiOutput("ml_cols"))
  })
  output$ml_cols <- shiny::renderUI({
    d <- first_data_l()
    shiny::req(d)
    cols <- as.list(d)
    grp <- .group_choices(cols)
    all <- .labelled(names(cols), cols)
    usual <- c("USUBJID", "AEBODSYS", "AEDECOD", "ASTDT", "AENDT", "AESEV",
               "AESER", "AEREL", "AEOUT", "PARAM", "AVISIT", "AVAL", "AVALC",
               "CMDECOD", "MHDECOD")
    shiny::tagList(
      shiny::selectInput("ml_group", t("Group (ordered by first; optional)"),
                         c(stats::setNames("", t("(none)")), grp),
                         selected = c(intersect(c("TRTA", "TRT01A", "ARM"), grp), "")[1L],
                         width = "100%"),
      shiny::selectizeInput(
        "ml_cols_pick", t("Columns (in order)"), all,
        selected = intersect(usual, names(cols)), multiple = TRUE,
        width = "100%",
        options = list(plugins = list("remove_button", "drag_drop"))),
      shiny::p(class = "small text-muted",
               t("Each column is headed by the data's label; the order is the group, the subject and the start date. Everything can be changed afterwards in step 2 (Content).")))
  })
  start_first_listing <- function(id, desc) {
    d <- first_data_l()
    if (is.null(d)) return(notify(t("Choose the data"), "warning"))
    g <- input$ml_group
    p2 <- guarded(first_listing(rv$p, id, input$ml_data, d,
                                columns = input$ml_cols_pick,
                                group = if (length(g) && nzchar(g)) g,
                                description = desc))
    if (is.null(p2)) return()
    after_id_change(p2, id)
    go("lf")
    lf_show_preview(id)
  }
  # -- the data files a report can start from (the first listing) ---------
  first_files <- shiny::reactive({
    f <- data_files()
    ok <- !is.na(vapply(f$folder, .folder_level, "", USE.NAMES = FALSE)) &
      tolower(tools::file_ext(f$file)) %in% .data_exts
    mine <- file.path(f$folder, f$file)[ok]
    # and the catalog's files outside the study folder (ADaM kept
    # elsewhere), said so
    p <- rv$p$ard$datasets$path
    p <- unique(p[!is.na(p) & tolower(tools::file_ext(p)) %in% .data_exts])
    p <- p[.data_outside(rv$study$path, p)]
    p <- p[file.exists(.data_file(rv$study$path, p))]
    c(stats::setNames(mine, mine),
      stats::setNames(p, sprintf(t("%s (outside the study folder)"), p)))
  })
  .srv_keep(environment())
}
