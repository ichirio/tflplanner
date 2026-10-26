# app_server(): ard analysis -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_ard_analysis <- function() {
  # the groups and the variables, from the data the analysis reads: groups
  # first those that look like treatments; variables of the method's kind
  output$ard_an_vars <- shiny::renderUI({
    st_drawn()
    r <- st_row_now()
    hcall <- .ard_method_call(r$method, .std_ard_methods())
    # inside a stack: the parent's data; its groups are the parent's
    in_stack <- identical(st_role(shiny::isolate(st_row())), "inside")
    if (in_stack) {
      pr <- st_parent_of(shiny::isolate(st_row()))
      r$dataset <- pr$dataset
      r$population_id <- pr$population_id
      r$data <- pr$data %||% NA_character_
      in_stack <- identical(pr$method, .stack_fn)
    }
    fd <- if (identical(st_role(shiny::isolate(st_row())), "inside"))
      .an_data_split(.an_data_value_row(r), .adata_rows(rv$p, ard_target())) else form_data(r)
    ds <- an_dataset(fd$dataset, fd$pop)
    d <- if (!is.na(ds %||% NA)) an_data(ds)
    by_now <- shiny::isolate(input[[st_id("by")]]) %||% .split_bar(r$by)
    var_now <- shiny::isolate(input[[st_id("vars")]]) %||% .split_bar(r$variables)
    strata_now <- shiny::isolate(input[[st_id("strata")]]) %||%
      .split_bar(r$strata %||% NA)
    den_now <- shiny::isolate(input[[st_id("den")]]) %||%
      (if (is.na(r$denominator %||% NA)) "" else r$denominator)
    kind <- st_kind(r)
    if (is.null(d)) {
      bch <- by_now
      vch <- var_now
      sch <- strata_now
    } else {
      cols <- as.list(d)
      rows <- .row_choices(cols, c(continuous = t("numbers"),
                                   categorical = t("counts")))
      kinds <- vapply(unname(rows), function(n) .column_kind(cols[[n]]), "")
      vch <- switch(kind,
        continuous = rows[kinds == "continuous"],
        categorical = rows[kinds == "categorical"],
        rows)
      grp <- .group_choices(cols)
      bch <- c(grp, rows[kinds == "categorical" & !rows %in% grp])
      # what the row has stays offered, even if the data do not say so
      bch <- c(bch, stats::setNames(setdiff(by_now, bch), setdiff(by_now, bch)))
      sch <- c(rows[kinds == "categorical"],
               stats::setNames(setdiff(strata_now, rows), setdiff(strata_now, rows)))
      vch <- c(vch, stats::setNames(setdiff(var_now, vch), setdiff(var_now, vch)))
    }
    # the chosen first, in the definition's order (a field shows them in
    # the order of its choices): AEBODSYS before AEDECOD, BASE | AVAL | CHG
    first <- function(ch, now) c(ch[match(now, ch, nomatch = 0L)], ch[!ch %in% now])
    vch <- first(vch, var_now)
    bch <- first(bch, by_now)
    sch <- first(sch, strata_now)
    shiny::tagList(
      if (!in_stack) shiny::selectizeInput(
        st_id("by"), with_tip(argl("Grouping variables", "by"), t("The variables the analysis is grouped by (e.g. TRT01A); whether they are the table's columns, rows or pages is step 2's. A combination with no records is shown, with 0.")),
        bch, by_now, multiple = TRUE,
        width = "100%", options = list(plugins = list("remove_button", "drag_drop"))),
      # the same analysis once more without its groups: cards' overall rows,
      # which a table shows as its Total column (step 3 switches it for
      # every analysis by the column variable)
      if (!in_stack) shiny::checkboxInput(
        st_id("overall"),
        with_tip(argl("Also over all subjects", "overall"),
                 t("The analysis once more without its grouping variables, over all its subjects: the Total column of a table (step 3's switch sets it for every analysis by the column variable). Needs grouping variables.")),
        .overall_on(r$overall), width = "100%"),
      shiny::selectizeInput(
        st_id("vars"), with_tip(argl("Analysis variables", "variables"),
                                arg_hint(hcall, "variables")), vch, var_now,
        multiple = TRUE, width = "100%",
        options = list(plugins = list("remove_button", "drag_drop"))),
      if (in_stack) shiny::p(class = "small text-muted mt-n2 mb-2",
                             t("The order of the variables is the order of the rows of a table that does not order them itself.")),
      shiny::div(
        class = "d-flex flex-wrap gap-2",
        if (!in_stack) shiny::div(class = "flex-grow-1", shiny::selectizeInput(
          st_id("strata"), with_tip(argl("Repeated within", "strata"),
                                    t("Repeated within each combination the data has (e.g. PARAMCD \u00d7 AVISIT). Unlike the groups (by), a combination with no records is not shown: for 0s, use the groups.")), sch, strata_now,
          multiple = TRUE, width = "100%",
          options = list(plugins = list("remove_button", "drag_drop"),
                         placeholder = t("none")))),
        shiny::div(class = "flex-grow-1", shiny::selectInput(
          st_id("den"), with_tip(argl("Percentages of", "denominator",
                                      st_arg_default(r, "denominator")),
                                 t("What a % is of. The population: its subjects in the group (the column headers' N; an AE table: the SAF's N per arm). Within a row: the total of the row (the variable's level). Within a column: the total of the group. Of the whole table: all of it. The method's default: blank.")),
          den_choices(den_now), den_now, width = "100%"))),
      if (is.null(d)) shiny::p(
        class = "small text-muted",
        t("The data of this analysis cannot be read (no file in the data catalog): the choices are the row's own.")))
  })
  output$ard_stat_part <- shiny::renderUI({
    st_drawn()
    r <- st_row_now()
    r0 <- shiny::isolate(st_row())
    kind <- st_kind(r)
    # inside a stack, only a summary, counts or missing counts keep some of
    # their statistics (tflspec's rule)
    if (identical(st_role(r0), "inside") &&
        identical(st_parent_of(r0)$method, .stack_fn) &&
        !kind %in% c("continuous", "categorical", "missing")) {
      return(shiny::p(class = "small text-muted",
                      t("Inside a stack this method keeps every result it gives: to keep only some, make it an analysis of its own.")))
    }
    kinds <- .stat_kinds(kind)
    if (identical(.std_ard_methods()$call[match(r$method, .std_ard_methods()$method)],
                  "(subjects)")) kinds <- "categorical"
    cat <- .std_ard_statistics(kinds)
    cat <- cat[!duplicated(cat$statistic), , drop = FALSE]
    have <- if (identical(r$method, r0$method)) .split_bar(r0$statistics) else
      character()
    extra <- setdiff(have, cat$statistic)
    # the catalog's labels in the app's language ("stat-label:<label>" in
    # the translations; as they are when there is none)
    cat$label <- vapply(cat$label, function(l) {
      if (is.na(l)) return(l)
      k <- paste0("stat-label:", l)
      v <- t(k)
      if (identical(v, k)) l else v
    }, "", USE.NAMES = FALSE)
    ch <- lapply(split(cat, factor(cat$group, levels = unique(cat$group))),
                 function(g) stats::setNames(as.list(g$statistic),
                                             paste0(g$statistic, " \u2014 ",
                                                    g$label)))
    if (length(extra)) ch[[t("not in the catalog")]] <-
      stats::setNames(as.list(extra), extra)
    note <- switch(kinds,
      continuous = t("Statistics of the numeric variables, in this order. cards computes its own; tflplanner writes a function for the others (CV, geometric mean, percentiles, CI of the mean ...)."),
      categorical = NULL,
      missing = NULL,
      t("This method gives a fixed set of results (estimate, confidence limits, p-value ...): the statistics picked here are the ones kept. Blank keeps them all."))
    dflt <- .method_default_stats(kinds)
    lab <- function(x) {
      l <- cat$label[match(x, cat$statistic)]
      ifelse(is.na(l), x, paste0(x, " \u2014 ", l))
    }
    shiny::tagList(
      shiny::selectizeInput(
        st_id("pick"), argl("Statistics", "statistics"), choices = ch, selected = have,
        multiple = TRUE, width = "100%",
        options = list(plugins = list("remove_button", "drag_drop"),
                       placeholder = t("the method's default (below)"))),
      if (!length(have)) shiny::div(
        class = "small mt-n2 mb-1",
        if (length(dflt)) shiny::tagList(
          shiny::span(class = "text-muted",
                      sprintf(t("Blank = the method's default (%d):"), length(dflt))),
          lapply(dflt, function(x) shiny::span(
            class = "badge text-bg-light border fw-normal ms-1", lab(x))),
          .btn(st_id("use_default"), t("Start from these"),
               class = "btn-sm btn-link py-0"))
        else shiny::span(class = "text-muted",
                         t("Blank = every result the method gives."))),
      if (!is.null(note)) shiny::p(class = "small text-muted mb-1", note),
      shiny::uiOutput("ard_stat_fmts"))
  })
  # a formula written from columns (the field keeps what was there until
  # the button is pressed)
  shiny::observe({
    st_drawn()
    r <- shiny::isolate(st_row_now())
    f <- .ard_form_fields(.ard_method_call(r$method, .std_ard_methods()))
    for (a in f$arg[f$kind %in% "formula"]) {
      n <- input[[st_id(paste0("fb_go_", a))]]
      key <- paste0("fb_", st_env$n, "_", a)
      if (is.null(n) || n < 1L || identical(n, st_env[[key]])) next
      st_env[[key]] <- n
      fm <- shiny::isolate(.fb_formula(input[[st_id(paste0("fb_resp_", a))]],
                                       input[[st_id(paste0("fb_terms_", a))]],
                                       isTRUE(input[[st_id(paste0("fb_int_", a))]])))
      if (is.null(fm)) {
        notify(t("Choose the response and at least one term."), "warning")
        next
      }
      shiny::updateTextInput(session, st_id(paste0("a_", a)), value = fm)
    }
  })
  # the defaults as a start: they become the analysis's own, to change
  shiny::observe({
    st_drawn()
    n <- input[[st_id("use_default")]]
    if (is.null(n) || n < 1L || identical(n, st_env[["used"]])) return()
    st_env[["used"]] <- n
    r <- shiny::isolate(st_row_now())
    shiny::updateSelectizeInput(session, st_id("pick"),
                                selected = .method_default_stats(.stat_kinds(st_kind(r))))
  })
  output$ard_stat_fmts <- shiny::renderUI({
    st_drawn()
    r <- st_row_now()
    pick <- input[[st_id("pick")]]
    if (!length(pick)) return(NULL)
    f <- .parse_formats(shiny::isolate(st_row())$formats)
    shiny::tagList(
      shiny::h6(class = "mt-2 mb-1", with_tip(argl("Formats", "formats"), t("xx.x = 1 decimal, xx.x% = a proportion as a percent, 2 = 2 decimals, pvalue = <0.001 or 3 decimals. Blank = the default shown."))),
      shiny::div(
      class = "rp-stat-fmt",
      lapply(seq_along(pick), function(i) {
        s <- pick[i]
        shiny::textInput(st_id(paste0("f_", s)), s,
                         value = if (!is.na(f[s])) f[[s]] else "",
                         placeholder = st_default(r, s))
      })))
  })
  # The analysis form's parts are outputs inside the form (ard_stat_ui).
  # Hidden until the form is drawn, shiny resumes them only after that
  # update, and nothing schedules another: they came seconds later, when
  # some unrelated timer woke the session.  Made with the form instead
  # (they draw nothing until an analysis is chosen).
  for (o in c("ard_stat_part", "ard_an_vars", "ard_an_args", "ard_method_note",
              "ard_fn_preset", "ard_fn_list", "ard_fn_now", "ard_an_code",
              "ard_stat_fmts")) {
    shiny::outputOptions(output, o, suspendWhenHidden = FALSE)
  }
  shiny::observeEvent(input$ard_stat_apply, {
    r <- st_row()
    g <- function(x) input[[st_id(x)]]
    one_val <- function(v) {
      v <- trimws(paste(v %||% character(), collapse = " | "))
      if (nzchar(v)) v else NA_character_
    }
    role <- st_role(r)
    new_id <- trimws(g("id") %||% r$analysis_id)
    if (!nzchar(new_id)) return(notify(t("Give the analysis an ID."), "warning"))
    a <- rv$p$ard$analyses
    mine <- !is.na(a$output_id) & a$output_id == r$output_id
    if (!identical(new_id, r$analysis_id) &&
        any(mine & !is.na(a$analysis_id) & a$analysis_id == new_id)) {
      return(notify(sprintf(t("%s already has an analysis %s."), r$output_id,
                            new_id), "warning"))
    }
    where <- trimws(g("where") %||% "")
    if (nzchar(where) &&
        inherits(try(parse(text = where), silent = TRUE), "try-error")) {
      return(notify(sprintf(t("The subset is not an R condition: %s"), where),
                    "warning"))
    }
    # a stack: on what it runs them, and what it adds
    if (role == "parent") {
      a <- rv$p$ard$analyses
      i <- which(mine & a$analysis_id == r$analysis_id)[1L]
      a$label[i] <- one_val(g("label"))
      fd <- form_data(r)
      a <- .an_data_write(a, i, fd)
      # a field not on screen (yet) keeps the row's value
      if (!is.null(g("where"))) a$where[i] <- one_val(where)
      if (!is.null(g("by"))) a$by[i] <- one_val(g("by"))
      if (!is.null(g("overall"))) {
        if (is.null(a$overall)) a$overall <- NA_character_
        a$overall[i] <- if (isTRUE(g("overall"))) "TRUE" else NA_character_
      }
      old <- .stack_flags_of(r$args)$flags
      flags <- vapply(names(.stack_flag_words), function(k) {
        v <- g(paste0("fl", k))
        if (is.null(v)) isTRUE(old[[k]]) else isTRUE(v)
      }, NA)
      a$args[i] <- .stack_args(flags, g("args_other") %||% .stack_flags_of(r$args)$other)
      a$analysis_id[i] <- new_id
      rv$p$ard$analyses <- a
      rv$p <- stack_rename(rv$p, r$output_id, r$analysis_id, new_id)
      an_pick(new_id)
      bump()
      notify(sprintf(t("%s: written"), new_id))
      # written, but the subjects per group are counted twice: said
      tw <- stack_n_twice(ard_rows(rv$p, "analyses", r$output_id))
      k <- match(new_id, tw$analysis_id)
      if (!is.na(k)) notify(sprintf(t(.n_twice_words), new_id, tw$with[k]), "warning")
      return()
    }
    # inside a stack: a variable the others have is refused (its rows could
    # not be told apart)
    if (role == "inside" && identical(st_parent_of(r)$method, .stack_fn)) {
      sib <- a[mine & !is.na(a$parent) & a$parent == r$parent &
                 a$analysis_id != r$analysis_id, , drop = FALSE]
      dup <- intersect(g("vars") %||% character(),
                       unlist(lapply(sib$variables, .split_bar)))
      if (length(dup)) {
        who <- sib$analysis_id[vapply(sib$variables, function(v) any(.split_bar(v) %in% dup), NA)]
        return(notify(sprintf(t("%s is computed by %s as well: make one of them an analysis of its own."),
                              paste(dup, collapse = ", "), paste(who, collapse = ", ")),
                      "warning"))
      }
    }
    pick <- g("pick") %||% character()
    fm <- vapply(pick, function(s) trimws(g(paste0("f_", s)) %||% ""), "")
    fm <- fm[nzchar(fm)]
    bad <- names(fm)[!.fmt_ok(fm)]
    if (length(bad)) {
      return(notify(sprintf(t("Not a format: %s"), paste(bad, collapse = ", ")),
                    "warning"))
    }
    # the analysis's variable-specific formats (AGE:mean=...) stay
    old <- .parse_formats(r$formats)
    fm <- c(fm, old[grepl(":", names(old), fixed = TRUE)])
    i <- which(mine & a$analysis_id == r$analysis_id)[1L]
    a$analysis_id[i] <- new_id
    a$label[i] <- one_val(g("label"))
    a$method[i] <- one_val(st_method())
    call <- .ard_method_call(a$method[i], .std_ard_methods())
    f <- .ard_form_fields(call)
    # a field not on screen (yet) keeps the row's value
    pa <- .ard_args_parse(if (identical(a$method[i], r$method)) r$args else NA, f)
    vals <- if (!is.null(f)) stats::setNames(lapply(f$arg, function(x)
      g(paste0("a_", x)) %||% pa$values[[x]]), f$arg) else list()
    new_args <- .ard_args_build(vals, f, g("args_other") %||% pa$other)
    if (!.ard_args_same(new_args, r$args)) a$args[i] <- new_args
    fd <- form_data(r)
    a <- .an_data_write(a, i, fd)
    a$where[i] <- one_val(where)
    a$by[i] <- one_val(g("by"))
    if (!is.null(g("overall"))) {
      if (is.null(a$overall)) a$overall <- NA_character_
      a$overall[i] <- if (isTRUE(g("overall"))) "TRUE" else NA_character_
    }
    if (!is.null(g("strata"))) a$strata[i] <- one_val(g("strata"))
    if (!is.null(g("den"))) a$denominator[i] <- one_val(g("den"))
    a$variables[i] <- one_val(g("vars"))
    a$statistics[i] <- one_val(pick)
    a$formats[i] <- if (length(fm))
      paste(paste0(names(fm), "=", fm), collapse = " | ") else NA
    if (identical(a$method[i], "custom") && !is.null(g("code"))) a$code[i] <- one_val(g("code"))
    # it becomes a stack: what it computed goes into one inside it
    to_stack <- identical(a$method[i], .stack_fn) && !identical(r$method, .stack_fn)
    if (to_stack) {
      own <- c("variables", "optional", "statistics", "strata", "denominator", "formats", "args")
      for (cn in intersect(own, names(a))) a[[cn]][i] <- NA
    }
    rv$p$ard$analyses <- a
    an_pick(new_id)
    bump()
    notify(sprintf(t("%s: written"), new_id))
    if (to_stack) notify(sprintf(t("%s runs analyses together now: add them inside it."), new_id))
  })
  # an analysis the fields cannot say: written as code (a custom analysis),
  # starting from the code its fields make now -- the same ARD; the
  # definition keeps the code (the program is never edited)
  an_as_code <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$ard_an_as_code, {
    r <- st_row()
    shiny::req(r)
    v <- tryCatch(tflspec::tfl_ard_as_custom(structure(rv$p$ard, class = "tfl_ard_spec"),
                                             r$output_id, r$analysis_id),
                  error = function(e) NULL)
    if (is.null(v) || is.na(v$code)) {
      return(notify(t("This analysis cannot be written as code from here (one run with others: write the stack's)."), "warning"))
    }
    an_as_code(c(list(output_id = r$output_id, analysis_id = r$analysis_id), v))
    shiny::showModal(shiny::modalDialog(
      title = sprintf(t("Write %s as code"), r$analysis_id), size = "l", easyClose = TRUE,
      shiny::p(t("The method becomes custom and this code, written from the definition as it is now, is the analysis: the same ARD (press Apply to the analysis first). The code is part of the definition: the program is never edited.")),
      shiny::div(class = "rp-code", shiny::tags$pre(v$code)),
      if (!is.na(v$formats)) shiny::p(class = "small text-muted",
                                     sprintf(t("Formats written out (its method's defaults with its own): %s"), v$formats)),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("ard_an_as_code_ok", t("Write it as code"), class = "btn-primary"))))
  })
  shiny::observeEvent(input$ard_an_as_code_ok, {
    v <- an_as_code()
    shiny::req(v)
    a <- rv$p$ard$analyses
    if (is.null(a$code)) a$code <- rep(NA_character_, nrow(a))
    i <- which(!is.na(a$output_id) & a$output_id == v$output_id & a$analysis_id == v$analysis_id)[1L]
    shiny::req(!is.na(i))
    a$method[i] <- v$method
    a$code[i] <- v$code
    a$formats[i] <- v$formats
    # what the code says now
    for (cn in c("args", "strata", "denominator")) a[[cn]][i] <- NA
    rv$p$ard$analyses <- a
    shiny::removeModal()
    an_as_code(NULL)
    bump()
    notify(sprintf(t("%s is written as code (custom)."), v$analysis_id))
  })
  # a new analysis for the report: the data, analysis set and groups of the
  # one shown (or the report's analysis data), to be filled in on the form
  shiny::observeEvent(input$ard_an_new, an_add())
  an_add <- function(data = NULL) {
    tg <- ard_target()
    if (is.null(tg)) return()
    a <- rv$p$ard$analyses
    have <- a$analysis_id[!is.na(a$output_id) & a$output_id == tg]
    k <- 1L
    while (paste0("A", k) %in% have) k <- k + 1L
    id <- paste0("A", k)
    a0 <- shiny::isolate(st_rows())
    r <- if (!is.null(a0) && nrow(a0)) shiny::isolate(st_row()) else
      list(dataset = NA, population_id = NA, by = NA)
    a[nrow(a) + 1L, ] <- NA
    a$output_id[nrow(a)] <- tg
    a$analysis_id[nrow(a)] <- id
    a$method[nrow(a)] <- "cards::ard_tabulate"
    # the data of the analysis shown (or the one given), as written there
    if (is.null(a$data)) a$data <- rep(NA_character_, nrow(a))
    if (is.null(data)) {
      rd <- r$data %||% NA
      mine <- .adata_of_report(rv$p, tg)
      if (!is.na(rd)) data <- rd else if (length(mine)) data <- mine[length(mine)]
      rp <- report_population(rv$p, tg)
      own <- if (is.null(data)) .report_toc_data(rv$p, tg) else character()
      if (length(own)) data <- own[1L]
      if (is.null(data) && !is.na(rp)) {
        q <- .ensure_pop_adata(rv$p, tg, rp)
        data <- attr(q, "data_id")
        attr(q, "data_id") <- attr(q, "added") <- NULL
        rv$p$ard$analysis_data <- q$ard$analysis_data
      }
    }
    if (!is.null(data)) {
      a$data[nrow(a)] <- data
    } else {
      a$dataset[nrow(a)] <- r$dataset
      a$population_id[nrow(a)] <- r$population_id
    }
    a$by[nrow(a)] <- r$by
    rv$p$ard$analyses <- a
    an_pick(id)
    bump()
  }
  # -- analyses run together (cards::ard_stack) ------------------------------
  # The outline of the report's analyses is the way in: a click shows one as
  # the form below; the ones inside a stack under it, moved up and down
  # there (the ARD keeps their order).  A stack's own form says on what it
  # runs them (data, analysis set, condition, groups) and what it adds (the
  # subjects per group, the total N ...); one inside says what it computes.
  st_role <- function(r, a = shiny::isolate(st_rows())) {
    if (isTRUE(r$method %in% .stack_wrappers)) return("parent")
    if (!.is_blank(r$parent %||% NA) && isTRUE(r$parent %in% a$analysis_id)) return("inside")
    "single"
  }
  st_parent_of <- function(r, a = shiny::isolate(st_rows())) {
    a[a$analysis_id == r$parent, , drop = FALSE][1L, ]
  }
  # the data an analysis reads, in words: "ADSL x SAF"
  data_words <- function(ds, pop, data = NA) {
    if (!.is_blank(data)) return(data)
    d <- an_dataset(ds, pop)
    paste(c(if (!.is_blank(d)) d, if (!.is_blank(pop)) pop), collapse = " \u00d7 ")
  }
  # the words of a method in the outline: the function's name on screen
  fn_label <- function(m) {
    e <- fn_entries(m)
    l <- e$label[match(m, e$value)]
    if (is.na(l)) m else l
  }
  # the function a method calls (cards::ard_stack_hierarchical, for the
  # company's keyword `hierarchical` too); NULL for R of one's own
  fn_call <- function(m) {
    if (.is_blank(m)) return(NULL)
    e <- fn_entries(m)
    v <- e$call[match(m, e$value)]
    if (is.na(v) || !nzchar(v) || startsWith(v, "(")) NULL else v
  }
  fn_code <- function(m, cls) {
    v <- fn_call(m)
    if (!is.null(v)) shiny::tags$code(class = cls, v)
  }
  # what a report's study ARD said went wrong, by analysis (the ARD as it
  # was last made)
  st_conditions <- shiny::reactive({
    rv$status_ver
    tg <- ard_target()
    if (is.null(tg) || !has_study()) return(NULL)
    a <- tryCatch(study_ard_rows(current_study(), tg), error = function(e) NULL)
    if (is.null(a) || !nrow(a)) return(NULL)
    tryCatch(tflspec::tfl_ard_conditions(a), error = function(e) NULL)
  })
  # an analysis's items of the review (its row of `analyses`): its errors,
  # and what to check (the subjects per group counted twice among them)
  st_problems <- function(tg, id, level = "error") {
    r <- review_now()
    if (is.null(r) || !nrow(r)) return(character())
    k <- r$sheet == "analyses" & !is.na(r$output_id) & r$output_id == tg &
      r$row == id & r$level %in% level
    unique(r$message[k])
  }
  # -- 1-1. the analysis data a report reads -------------------------------
  # Named data (tflspec's sheet analysis_data), the study's.  The report's
  # in two groups, made in this order: its subjects (an analysis set's own
  # data, kept to the report's subjects: one row a subject, the
  # denominator), then the data it analyses (a dataset kept to those
  # subjects, the rows a condition keeps, columns taken and made).  Each
  # with what it is, its records and subjects and the analyses on it; the
  # data the report's analyses read without a name (a dataset x analysis
  # set) can be given one.  1-2 below: the analyses.
  adata_js <- function(input, value) sprintf(
    "Shiny.setInputValue('%s', %s, {priority: 'event'});", input,
    jsonlite::toJSON(value, auto_unbox = TRUE))
  # The report's analysis set: the report list's, the TOC's -- one value.
  # Chosen here, it is written there, the data of its subjects (adsl_<set>)
  # is made, and the report's analyses move to the new set's data.
  # `now`, the report's set when the populations sheet does not have it,
  # stays a choice: a select without it would show "(not chosen)" and send
  # it (#418)
  pop_choices <- function(now = NA) {
    po <- rv$p$ard$populations
    po <- po[!is.na(po$population_id), , drop = FALSE]
    adsl <- tryCatch(an_data(po$dataset[1L] %||% "ADSL"), error = function(e) NULL)
    lab <- vapply(po$population_id, function(id) {
      l <- .pop_label(rv$p, id, adsl)
      if (identical(l, id)) id else sprintf("%s \u2014 %s", id, l)
    }, "")
    out <- c(stats::setNames("", t("(not chosen)")), stats::setNames(po$population_id, lab))
    if (!.is_blank(now) && !now %in% po$population_id) {
      out <- c(out, stats::setNames(now, sprintf(t("%s (not in the populations sheet)"), now)))
    }
    out
  }
  output$ard_report_pop <- shiny::renderUI({
    tg <- ard_target()
    shiny::req(has_study(), tg)
    rv$ver
    now <- report_population(rv$p, tg)
    shiny::div(
      class = "d-flex flex-wrap align-items-baseline gap-2 mb-2",
      shiny::tags$label(`for` = "ard_report_pop_pick", class = "small fw-bold mb-0",
                        with_tip(t("This report's population"), t("The report's analysis set (the TOC's; also the header's <OUTPUT_POPULATION> text): one value for the report. Chosen, the data of its subjects (adsl_<set>) is made, and the report's analyses move to it; 1-1 and 1-2 start from it. An analysis may still read another."))),
      shiny::div(style = "min-width: 16rem",
                 .for_report(shiny::selectInput("ard_report_pop_pick", NULL, pop_choices(now),
                                                selected = if (is.na(now)) "" else now,
                                                width = "100%"), tg)))
  })
  # only the user's change, of the report the select was drawn for (#418)
  shiny::observeEvent(input$ard_report_pop_pick_by, {
    tg <- ard_target()
    shiny::req(has_study(), tg)
    v <- .report_pick(input$ard_report_pop_pick_by, tg)
    if (is.null(v)) return()
    v <- if (length(v)) v[1L] else ""
    now <- report_population(rv$p, tg)
    if (identical(v, if (is.na(now)) "" else now)) return()
    p <- guarded(set_report_population(rv$p, tg, if (nzchar(v)) v else NA))
    if (is.null(p)) return()
    made <- attr(p, "made")
    left <- attr(p, "left")
    attr(p, "made") <- attr(p, "left") <- NULL
    rv$p <- p
    bump()
    if (length(made)) notify(sprintf(t("%s made (save to keep it)."), paste(made, collapse = ", ")))
    if (length(left)) notify(sprintf(t("%s left as it is: change it in 1-1 if it should follow the population."),
                                     paste(left, collapse = ", ")), "warning")
  }, ignoreInit = TRUE)
  output$ard_adata <- shiny::renderUI({
    tg <- ard_target()
    shiny::req(has_study(), tg)
    p <- rv$p
    ad <- .adata_rows(p, ard_target())
    po <- p$ard$populations
    mine <- .adata_of_report(p, tg)
    n <- tryCatch(adata_counts(), error = function(e) character())
    w <- data_words_list()
    link <- function(input, value, label, cls = "") shiny::tags$button(
      type = "button", class = paste("btn btn-sm btn-link p-0", cls),
      onclick = adata_js(input, value), label)
    pick <- adata_pick() %||% NA
    row_of <- function(id) {
      u <- .adata_uses(p, tg, id)
      here <- u$analyses[startsWith(u$analyses, paste0(tg, " / "))]
      lab <- ad$label[match(id, ad$data_id)]
      cnt <- n[id]
      on <- identical(id, pick)
      shiny::div(
        class = paste("d-flex align-items-start gap-2 px-2 py-1 border-bottom small",
                      if (on) "bg-primary-subtle"),
        style = "cursor: pointer;", onclick = adata_js("ard_adata_pick", id),
        role = "option", `aria-selected` = if (on) "true" else "false",
        shiny::span(class = if (on) "text-primary" else "text-muted",
                    if (on) "\u25c9" else "\u25cb"),
        shiny::div(
          class = "flex-grow-1",
          shiny::strong(id), if (!.is_blank(lab)) shiny::span(class = "text-muted", lab),
          shiny::div(class = "text-muted",
                     paste(c(.adata_words(ad, id, w),
                             if (length(cnt) && !is.na(cnt)) cnt), collapse = " \u00b7 ")),
          if (length(here)) shiny::div(
            class = "text-muted",
            sprintf(t("Read by %s"), paste(sub("^.* / ", "", here), collapse = ", ")),
            if (length(setdiff(u$analyses, here)))
              sprintf(t("; other reports' analyses: %d"), length(setdiff(u$analyses, here)))),
          if (length(cl_of(id))) shiny::div(
            class = "text-muted",
            sprintf(t("Code lists: %s (Column definitions \u2463)"), paste(cl_of(id), collapse = ", ")))))
    }
    # the columns of a data with a code list of this report: its dataset's,
    # those it adds and makes
    cl_rows <- sheet_rows(p, "codelists", tg)
    cl_of <- function(id) {
      r <- ad[match(id, ad$data_id), , drop = FALSE]
      cols <- c(adata_cols(adata_root(r$from)), .split_bar(r$add),
                vapply(.drv_read_all(r$derive %||% NA_character_), function(x) x$name, ""))
      vapply(.codelist_lines(cl_rows, cols), `[[`, "", "variable")
    }
    # the data the report's analyses read without a name
    a <- st_rows()
    auto <- character()
    if (!is.null(a) && nrow(a)) {
      d <- a$data %||% rep(NA_character_, nrow(a))
      par <- a$parent %||% rep(NA_character_, nrow(a))
      b <- a[.is_blank_v(d) & .is_blank_v(par) &
               !(.is_blank_v(a$dataset) & .is_blank_v(a$population_id)), , drop = FALSE]
      auto <- unique(vapply(seq_len(nrow(b)), function(i)
        .an_data_value(b$dataset[i], b$population_id[i]), ""))
    }
    # a data the analyses read without a name (a dataset x analysis set):
    # chosen as the others, its settings open below with the program's name
    # for it; saving gives it that name
    auto_row <- function(v) {
      sp <- .an_data_split(v)
      nm <- .an_data_name(sp$dataset, sp$pop, po)
      words <- sprintf(t("%s \u2014 no name yet (in the program: %s)"),
                       data_words(sp$dataset, sp$pop), nm)
      if (is.na(sp$pop)) {
        return(shiny::div(
          class = "d-flex align-items-start gap-2 px-2 py-1 border-bottom small",
          shiny::span(class = "text-muted", "\u2013"),
          shiny::div(class = "flex-grow-1 text-muted", words)))
      }
      on <- identical(paste0("auto:", v), pick)
      shiny::div(
        class = paste("d-flex align-items-start gap-2 px-2 py-1 border-bottom small",
                      if (on) "bg-primary-subtle"),
        style = "cursor: pointer;", onclick = adata_js("ard_adata_name", v),
        role = "option", `aria-selected` = if (on) "true" else "false",
        shiny::span(class = if (on) "text-primary" else "text-muted",
                    if (on) "\u25c9" else "\u25cb"),
        shiny::div(class = "flex-grow-1 text-muted", words))
    }
    picked <- !is.na(pick) && pick %in% ad$data_id
    shiny::div(
      class = "rp-b-card mb-2",
      shiny::h6(class = "mb-2", with_tip(
        t("1-1 Analysis data"),
        t("The data this report's analyses read, made from the study's datasets: the rows a condition keeps, the subjects of another analysis data, columns made. They are this report's (another report's adsl_saf may differ: copy one with the button). The denominator of a percent is chosen with each analysis, in 1-2."))),
      shiny::div(
        class = "d-flex flex-wrap gap-1 align-items-center mb-1",
        .btn("ard_adata_new", t("New analysis data"), class = "btn-sm btn-outline-primary py-0"),
        .btn("ard_adata_copy",
             if (picked) sprintf(t("Copy %s"), pick) else t("Copy the one chosen"),
             class = "btn-sm btn-outline-secondary py-0", disabled = if (!picked) NA),
        .btn("ard_adata_import", t("Copy from another report..."), class = "btn-sm btn-outline-secondary py-0"),
        .btn("ard_adata_del",
             if (picked) sprintf(t("Delete %s..."), pick) else t("Delete the one chosen..."),
             class = "btn-sm btn-outline-danger py-0", disabled = if (!picked) NA)),
      shiny::p(class = "small text-muted mb-1",
               t("Choose one in the list (\u25c9): its settings open below it.")),
      # the report's own: the analysis data its analyses read (and those
      # they are made from), and the data they read without a name; the
      # study's others are on the Data tab
      shiny::div(
        class = "border rounded",
        # a study of the old format: why its analysis data are not here
        if (length(.old_format(rv$p)))
          shiny::p(class = "small text-warning m-2", t(.old_format_line))
        else if (!length(mine) && !length(auto))
          shiny::p(class = "small text-muted m-2", t("None yet: New analysis data makes one.")),
        lapply(mine, row_of),
        lapply(auto, auto_row)))
  })
  output$ard_2_2_head <- shiny::renderUI({
    shiny::req(has_study(), ard_target())
    shiny::tagList(
      shiny::h6(class = "mt-3 mb-1", with_tip(
        t("1-2 Analyses"),
        t("The report's analyses. Each one's Data: an analysis data of 1-1; the denominator of a percent: chosen with it. ARD: the data of the analyses' results (the tables are made from it). Population: the subjects a percent is of (SAF ...)."))),
      if (length(.old_format(rv$p)))
        shiny::p(class = "small text-warning mb-1", t(.old_format_line)))
  })
  .srv_keep(environment())
}
