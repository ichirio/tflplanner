# app_server(): ard data -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_ard_data <- function() {
  # the form: what is being made (`old`: the one changed; `name`: a
  # report's data being given a name; `pop`: the analysis set it was kept to)
  adata_edit <- shiny::reactiveVal(NULL)
  # the analysis data clicked in 1-1 (its form open below)
  adata_pick <- shiny::reactiveVal(NULL)
  adata_form_ui <- shiny::reactiveVal(NULL)
  # the right of step 1 (SPEC | Code | Result) follows what is open on the
  # left: an analysis data of 1-1, else 1-2's analyses
  step2_focus <- shiny::reactiveVal("analysis")
  output$adata_detail <- shiny::renderUI(adata_form_ui())
  adata_close <- function() {
    step2_focus("analysis")
    adata_form_ui(NULL)
    adata_edit(NULL)
    adata_pick(NULL)
  }
  adata_cols <- function(ds) {
    d <- if (!.is_blank(ds)) an_data(ds)
    if (is.null(d)) character() else names(d)
  }
  # the data of one row a subject a data can be kept to: those above it in
  # the sheet (all for a new one), the report's first
  adata_subject_cands <- function(old = NULL) {
    p <- rv$p
    ad <- .adata_rows(p, ard_target())
    ids <- .adata_subject_level(p, ard_target())
    if (!is.null(old)) {
      k <- match(old, ad$data_id)
      ids <- ids[!is.na(k) & match(ids, ad$data_id) < k]
    }
    mine <- .adata_of_report(p, ard_target())
    c(intersect(mine, ids), setdiff(ids, mine))
  }
  adata_pop_ds <- function() {
    ds <- unique(stats::na.omit(rv$p$ard$populations$dataset))
    if (length(ds)) ds else "ADSL"
  }
  # the rows kept: the condition builder (a condition rows cannot hold
  # stays as R, as written)
  adata_cond_value <- shiny::reactiveVal(NA_character_)
  adata_cond_key <- shiny::reactiveVal(0L)
  adata_cond <- condition_builder_server(
    "adata_cond",
    data = shiny::reactive({
      shiny::req(!is.null(adata_edit()))
      ds <- adata_root(input$adata_from %||% "")
      if (.is_blank(ds)) NULL else an_data(ds)
    }),
    value = adata_cond_value, lang = lang, key = adata_cond_key)
  # for shiny::testServer(): the condition as the builder gives it
  session$userData$adata_cond <- list(value = adata_cond_value, key = adata_cond_key)
  # the columns made (R/derive_builder.R): of the data's columns, those added
  adata_drv <- .derive_editor_server(
    input, output, session, t, notify,
    cols = shiny::reactive({
      shiny::req(!is.null(adata_edit()))
      unique(c(adata_cols(adata_root(input$adata_from %||% "")), input$adata_add))
    }),
    data = shiny::reactive({
      shiny::req(!is.null(adata_edit()))
      ds <- adata_root(input$adata_from %||% "")
      if (.is_blank(ds)) NULL else an_data(ds)
    }),
    lang = lang)
  adata_derive_now <- function() {
    v <- adata_drv$text()
    if (is.na(v)) "" else v
  }
  adata_where_now <- function() {
    v <- adata_cond()$expr
    if (is.null(v) || !length(v) || is.na(v) || !nzchar(trimws(v))) NA_character_ else v
  }
  # a new data still under its suggested name: named after what it is
  # made from and its condition (ADSL, SAFFL == "Y": adsl_saf)
  shiny::observeEvent(list(adata_cond()$expr, input$adata_pop, input$adata_subj_on,
                           input$adata_from), {
    e <- adata_edit()
    shiny::req(!is.null(e), is.null(e$old), identical(input$adata_id, e$suggested))
    root <- adata_root(input$adata_from)
    nm <- .adata_name_from(root, adata_where_now(), rv$p$ard$populations,
                           pop = adata_pop_name())
    # taken, or nothing to name it after: the usual suggestion for its data
    if ((is.na(nm) || nm %in% .adata_taken_names(rv$p, ard_target())) && !.is_blank(root)) {
      nm <- .adata_suggest(rv$p, ard_target(), root, adata_pop_name(), adata_where_now())
    }
    if (!is.na(nm) && !identical(nm, input$adata_id)) {
      shiny::updateTextInput(session, "adata_id", value = nm)
      e$suggested <- nm
      adata_edit(e)
    }
  }, ignoreInit = TRUE)
  # The analysis set: the first row of the condition, put in at once --
  # the study's analysis sets (pop:SAF) and the population flags of the
  # data (flag:PPROTFL), each with the condition it writes
  adata_pop_options <- function(from) {
    po <- rv$p$ard$populations
    po <- po[!.is_blank_v(po$where), , drop = FALSE]
    val <- paste0("pop:", po$population_id)
    cond <- po$where
    lab <- sprintf("%s (%s)", po$population_id, po$where)
    d <- if (!.is_blank(from)) an_data(adata_root(from))
    flags <- if (!is.null(d)) .cond_population_flags(names(d)) else character()
    have <- unlist(regmatches(po$where, gregexpr("[A-Za-z0-9_]+FL", po$where)))
    flags <- setdiff(flags, have)
    # (paste0() of no flags would still give one "flag:")
    fv <- if (length(flags)) paste0("flag:", flags) else character()
    fc <- if (length(flags)) sprintf("%s == %s", flags, encodeString("Y", quote = "\"")) else character()
    data.frame(value = c(val, fv), cond = c(cond, fc), label = c(lab, fc),
               name = c(po$population_id, vapply(flags, .population_id_for, "")),
               stringsAsFactors = FALSE)
  }
  adata_pop_choices <- function(from) {
    o <- adata_pop_options(from)
    c(stats::setNames(".none", t("(none)")), stats::setNames(o$value, o$label))
  }
  # the option the condition's first row is (".none": none)
  adata_pop_of_where <- function(where, from) {
    o <- adata_pop_options(from)
    f <- .cond_first(where)
    k <- match(f, vapply(o$cond, .cond_first, ""))
    if (is.na(f) || is.na(k)) ".none" else o$value[k]
  }
  adata_pop_name <- function() {
    v <- input$adata_pop %||% ""
    o <- adata_pop_options(input$adata_from %||% "")
    o$name[match(v, o$value)] %||% NA_character_
  }
  # chosen: the condition's first row becomes it (a first row that is an
  # analysis set's goes; "none" takes it out)
  shiny::observeEvent(input$adata_pop, {
    shiny::req(!is.null(adata_edit()))
    o <- adata_pop_options(input$adata_from %||% "")
    v <- input$adata_pop
    new <- o$cond[match(v, o$value)]
    w <- .cond_set_first(adata_where_now(), if (length(new)) new else NA, o$cond)
    if (!identical(w, adata_where_now())) {
      adata_cond_value(w)
      adata_cond_key(shiny::isolate(adata_cond_key()) + 1L)
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$adata_from, {
    shiny::req(!is.null(adata_edit()))
    shiny::updateSelectInput(session, "adata_pop", choices = adata_pop_choices(input$adata_from),
                             selected = adata_pop_of_where(adata_where_now(), input$adata_from))
  }, ignoreInit = TRUE)
  adata_form <- function(r, title) {
    blank_na <- function(x) if (is.null(x) || !length(x) || is.na(x[1L])) "" else x[1L]
    p <- rv$p
    ad <- .adata_rows(p, ard_target())
    e <- adata_edit()
    # its population_id: the condition's first row (saved back as it while
    # unchanged); one that cannot be drawn so is kept as it is (said so)
    e$legacy_pop <- blank_na(r$population_id)
    where <- blank_na(r$where)
    if (nzchar(e$legacy_pop) && .is_blank(r$subjects)) {
      po <- p$ard$populations
      w2 <- .cond_put_pop(where, po$where[match(e$legacy_pop, po$population_id)])
      if (!is.na(w2)) {
        where <- w2
        e$legacy_pop <- ""
      }
    }
    old <- e$old
    above <- if (is.null(old)) ad$data_id else
      ad$data_id[seq_len(match(old, ad$data_id) - 1L)]
    bar <- function(v) .split_bar(v)
    cands <- adata_subject_cands(old)
    if (!.is_blank(r$subjects) && !r$subjects %in% cands) cands <- c(r$subjects, cands)
    # kept to the report's analysis set's subjects (adsl_<set>), if it is one
    rp_data <- {
      rp <- report_population(p, ard_target() %||% "")
      pds <- p$ard$populations$dataset[match(rp, p$ard$populations$population_id)]
      if (is.na(rp)) NA else .adata_same_as(ad, if (is.na(pds)) "ADSL" else pds, rp, NA, NA)
    }
    subj_now <- if (!.is_blank(r$subjects)) r$subjects else
      if (!is.na(rp_data) && rp_data %in% cands) rp_data else if (length(cands)) cands[1L] else ""
    # kept to them: as it is; a new one, unless it is itself one row a subject
    subj_on <- if (!is.null(old) || !is.null(e$name)) !.is_blank(r$subjects) else
      length(cands) > 0L && !adata_root(r$from) %in% adata_pop_ds()
    e$cands <- cands
    adata_edit(e)
    # the analysis set: the condition's first row; a new data not kept to
    # other subjects starts with the study's first
    pop_ch <- adata_pop_choices(r$from)
    pop_now <- adata_pop_of_where(where, r$from)
    if (is.null(old) && is.null(e$name) && !subj_on && identical(pop_now, ".none") &&
        length(pop_ch) > 1L) {
      rp <- paste0("pop:", report_population(p, ard_target() %||% ""))
      pop_now <- if (rp %in% pop_ch) rp else unname(pop_ch[2L])
      o <- adata_pop_options(r$from)
      where <- .cond_set_first(where, o$cond[match(pop_now, o$value)], o$cond)
    }
    adata_cond_value(if (.is_blank(where)) NA_character_ else where)
    adata_cond_key(shiny::isolate(adata_cond_key()) + 1L)
    ds <- p$ard$datasets$dataset
    from_ch <- c(stats::setNames(ds, ds),
                 stats::setNames(above, sprintf(t("%s (analysis data)"), above)))
    # what the sheet says, though the form has no such choice: kept as it is
    if (!.is_blank(r$from) && !r$from %in% from_ch) {
      from_ch <- c(stats::setNames(r$from, sprintf(t("%s (as written in the sheet: not a dataset nor an analysis data above)"),
                                                   r$from)), from_ch)
    }
    more <- !.is_blank(r$add) || !.is_blank(r$derive) || !.is_blank(r$keep) || !.is_blank(r$distinct)
    has_cl <- nrow(sheet_rows(p, "codelists", ard_target() %||% "") %||% data.frame()) > 0L
    adata_drv$load(r$derive)
    # add: columns of the subjects' data joined in -- not offered while it is
    # made from the analysis set's own data (it has them all), unless the
    # sheet says some
    add_ui <- shiny::selectizeInput(
      "adata_add", with_tip(argl("Columns added from the subjects' data", "add"),
                            t("Columns of the subjects' data (the population's, or the analysis data its subjects are kept to) joined by the subject key. Nothing chosen: nothing added. A column of the same name takes the subjects' values.")),
      choices = unique(c(bar(r$add), adata_add_choices(r$from, subj_now))),
      selected = bar(r$add), multiple = TRUE, width = "100%",
      options = list(create = TRUE, plugins = list("remove_button")))
    own <- unname(from_ch)[vapply(unname(from_ch), function(v)
      adata_root(v) %in% adata_pop_ds(), NA)]
    if (.is_blank(r$add) && length(own)) {
      own_js <- paste(encodeString(own, quote = "\""), collapse = ", ")
      # (part 1 says why it has nothing to add, rather than stand empty)
      add_ui <- shiny::tagList(
        shiny::conditionalPanel(sprintf("[%s].indexOf(input.adata_from) < 0", own_js), add_ui),
        shiny::conditionalPanel(
          sprintf("[%s].indexOf(input.adata_from) >= 0", own_js),
          shiny::p(class = "small text-muted mb-2",
                   t("Columns added from the subjects' data: none to add (this data is the subjects' data itself)."))))
    }
    step2_focus("adata")
    adata_form_ui(shiny::div(
      class = "rp-b-card mb-2 border-primary",
      shiny::h6(class = "mb-2", title),
      e$note,
      # first what it is made from and its analysis set (the name follows them)
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::selectInput("adata_from", argl("Made from", "from"), from_ch,
                           selected = blank_na(r$from), width = "100%"),
        shiny::selectInput("adata_pop", with_tip(
          t("Population"),
          t("The analysis set (Populations tab) this data is kept to, or a population flag of the data (SAFFL = Y). Make an analysis data only when the report needs more than a dataset kept to a set.")),
          pop_ch, selected = pop_now, width = "100%")),
      shiny::uiOutput("adata_pop_note"),
      if (nzchar(e$legacy_pop)) shiny::p(
        class = "small text-muted mb-2",
        sprintf(t("Also kept to the population %s (population_id, as the sheet has it: SPEC shows it)."),
                e$legacy_pop)),
      bslib::layout_columns(
        col_widths = c(5, 7),
        shiny::textInput("adata_id", argl("Name (used in the program)", "data_id"),
                         blank_na(r$data_id), width = "100%"),
        shiny::textInput("adata_label", t("Label"), blank_na(r$label), width = "100%")),
      shiny::uiOutput("adata_same"),
      shiny::div(
      id = "adata_gui", class = if (!.is_blank(r$code)) "rp-greyed",
      if (length(cands)) shiny::div(
        class = "border-start border-primary border-3 ps-2 mb-2",
        shiny::div(
          class = "d-flex align-items-baseline gap-1",
          shiny::checkboxInput(
            "adata_subj_on",
            shiny::span(if (length(cands) == 1L) sprintf(t("Keep to the subjects of %s"), cands[1L]) else
              t("Keep to the subjects of an analysis data of one row a subject"),
              shiny::span(class = "text-muted small", "(subjects)")),
            subj_on),
          help_tip(t("Only the subjects in that analysis data: its condition (its population's too) is not written here again."))),
        if (length(cands) > 1L) shiny::conditionalPanel(
          "input.adata_subj_on",
          shiny::selectInput("adata_subj", NULL, stats::setNames(cands, cands),
                             selected = subj_now, width = "100%")),
        NULL),
      shiny::tags$label(class = "form-label", with_tip(argl("Filter (a condition)", "where"), t("The condition the data's rows are filtered by (filter()). The rows are AND (\"+ and\"); \"+ or (another group)\" is OR. A flag: \"= Y\". What the rows cannot say: \"Write the condition as R\"."))),
      condition_builder_ui("adata_cond", lang),
      # its columns, in the order the program makes them: added, made or
      # changed, kept, and the code lists put on them
      shiny::tags$details(
        class = "mb-2", open = if (more || has_cl) NA,
        shiny::tags$summary(class = "small", with_tip(
          t("Column definitions"),
          t("The columns of this analysis data, in the order its program makes them: \u2460 added from the subjects' data, \u2461 made or changed, \u2462 kept, \u2463 the code lists put on them."))),
        shiny::div(class = "rp-num-part", shiny::span(class = "rp-num", "\u2460"), shiny::div(add_ui)),
        shiny::div(class = "rp-num-part", shiny::span(class = "rp-num", "\u2461"),
          .derive_editor_ui(t, with_tip(argl("Columns made or changed", "derive"),
                                        t("The columns this data makes, or changes when the name is a column it has: split by conditions, cut a number into groups, days between two dates, or any R (mutate()). The sheet keeps each as NAME = R (derive, | between them); one the form cannot draw is kept as written.")),
                            blank_na(r$derive))),
        shiny::div(class = "rp-num-part", shiny::span(class = "rp-num", "\u2462"),
          shiny::selectizeInput("adata_keep", with_tip(
            argl("Columns kept (blank: all)", "keep"),
            t("The columns kept (select()). Given, the subject key and the columns this report's analyses read are kept too, without naming them.")),
                                choices = unique(c(bar(r$keep), adata_cols(adata_root(r$from)))),
                                selected = bar(r$keep), multiple = TRUE, width = "100%",
                                options = list(create = TRUE, plugins = list("remove_button")))),
        # (one row per ...: after the columns kept, before the code lists,
        # as the program has it)
        shiny::div(class = "rp-num-part", shiny::span(class = "rp-num", ""),
          shiny::selectizeInput("adata_distinct", argl("One row per (e.g. subject; subject \u00d7 phase)", "distinct"),
                                choices = unique(c(bar(r$distinct), adata_cols(adata_root(r$from)))),
                                selected = bar(r$distinct), multiple = TRUE, width = "100%",
                                options = list(create = TRUE, plugins = list("remove_button")))),
        shiny::div(class = "rp-num-part", shiny::span(class = "rp-num", "\u2463"),
          shiny::div(shiny::uiOutput("adata_cl"), shiny::uiOutput("adata_cl_detail"))))),
      # the way out: R that makes the data itself (the definition keeps it)
      shiny::tags$details(
        class = "mb-2", open = if (!.is_blank(r$code)) NA,
        shiny::tags$summary(class = "small", with_tip(t("Write this analysis data whole as R (code)"), t("When the fields cannot say it: R whose value is the whole data (the datasets, pop_<set> and the analysis data above are in reach); written, none of the fields above is used. Only the filter: \"Write the condition as R\" above. The program is never edited: this R is the definition."))),
        shiny::textAreaInput("adata_code", NULL, blank_na(r$code), width = "100%", rows = 6,
                             resize = "vertical"),
        .btn("adata_code_start", t("Start from the generated code"),
             class = "btn-sm btn-outline-secondary")),
      shiny::div(
        class = "d-flex gap-2 mt-2 align-items-center",
        .btn("adata_save", t("Save"), class = "btn-sm btn-primary"),
        .btn("adata_close", t("Close"), class = "btn-sm btn-outline-secondary"))))
    output$adata_preview_out <- shiny::renderUI(NULL)
    output$adata_preview_tbl <- shiny::renderTable(NULL)
  }
  # the dataset a data is first made from (a dataset: itself)
  adata_root <- function(from) {
    d <- .adata_dataset(.adata_rows(rv$p, ard_target()), from)
    if (is.na(d)) from else d
  }
  # the columns `add` can take: those of the subjects' data (an analysis
  # data: its dataset's and those it makes), or of the analysis set's
  adata_add_choices <- function(from, subj) {
    subj <- subj %||% ""
    if (!nzchar(subj)) return(character())
    ad <- .adata_rows(rv$p, ard_target())
    made <- trimws(sub("=.*$", "", .split_bar(ad$derive[match(subj, ad$data_id)])))
    unique(c(adata_cols(adata_root(subj)), made))
  }
  adata_subj_value <- function() {
    e <- adata_edit()
    if (!isTRUE(input$adata_subj_on) || !length(e$cands)) return(NA_character_)
    v <- input$adata_subj %||% ""
    if (nzchar(v) && v %in% e$cands) v else e$cands[1L]
  }
  shiny::observeEvent(list(input$adata_subj_on, input$adata_subj), {
    shiny::req(!is.null(adata_edit()))
    s <- adata_subj_value()
    shiny::updateSelectizeInput(
      session, "adata_add",
      choices = unique(c(input$adata_add, if (!is.na(s)) adata_add_choices(input$adata_from, s))),
      selected = input$adata_add)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$adata_from, {
    e <- adata_edit()
    shiny::req(!is.null(e), is.null(e$old), is.null(e$name), length(e$cands))
    shiny::updateCheckboxInput(session, "adata_subj_on",
                               value = !adata_root(input$adata_from) %in% adata_pop_ds())
  }, ignoreInit = TRUE)
  # a form field as one cell ("A | B"; NA when blank)
  one_of <- function(v) if (is.null(v) || !length(v) || !nzchar(trimws(paste(v, collapse = "")))) NA else
    paste(v, collapse = " | ")
  # The analysis set and the condition as the sheet has them: the chosen
  # set's own condition as the first rows, unchanged, is its population_id
  # (not in `where`); changed, it is a condition like the others.  Kept to
  # another data's subjects: no population_id (not both).
  adata_pop_split <- function(subjects) {
    e <- adata_edit()
    where <- adata_where_now()
    if (!is.na(subjects)) return(list(pop = NA_character_, where = where))
    # the set chosen, else the one the first row is
    v <- input$adata_pop %||% ""
    if (!startsWith(v, "pop:")) v <- adata_pop_of_where(where, input$adata_from %||% "")
    pop <- if (startsWith(v, "pop:")) substring(v, 5L) else NA_character_
    ps <- .cond_take_pop(where, rv$p$ard$populations, pop)
    # another dataset's flag that is not the set's for every subject (or
    # missing there): a condition, not the set
    dis <- if (!is.na(ps$pop)) adata_pop_disagrees(ps$pop)
    if (!is.null(dis) && dis != 0L) ps <- list(pop = NA_character_, where = where)
    if (is.na(ps$pop) && nzchar(e$legacy_pop %||% "")) ps$pop <- e$legacy_pop
    ps
  }
  # A set's condition on a dataset that is not the set's own (ADAE's
  # SAFFL): how many subjects have another flag there than in the set's
  # dataset (0: the same; NA: a column is missing; NULL: not that case)
  adata_pop_disagrees <- function(pop) {
    if (is.na(pop)) return(NULL)
    po <- rv$p$ard$populations
    k <- match(pop, po$population_id)
    root <- adata_root(input$adata_from %||% "")
    if (is.na(k) || .is_blank(root) || identical(root, po$dataset[k])) return(NULL)
    fl <- .pop_flag(po$where[k])
    if (is.na(fl)) return(NULL)
    d <- tryCatch(an_data(root), error = function(e) NULL)
    s <- tryCatch(an_data(po$dataset[k]), error = function(e) NULL)
    if (is.null(d) || is.null(s)) return(NULL)
    n <- .flag_disagrees(d, s, fl, .adata_subject_key(rv$p))
    if (is.na(n)) -1L else n
  }
  output$adata_pop_note <- shiny::renderUI({
    shiny::req(!is.null(adata_edit()))
    v <- input$adata_pop %||% ""
    shiny::req(startsWith(v, "pop:"))
    pop <- substring(v, 5L)
    n <- adata_pop_disagrees(pop)
    shiny::req(!is.null(n), n != 0L)
    po <- rv$p$ard$populations
    k <- match(pop, po$population_id)
    root <- adata_root(input$adata_from %||% "")
    shiny::div(class = "alert alert-warning small py-1 mb-2",
               if (n < 0L) sprintf(t("%s has no %s: its condition is not taken as the population %s (written as a condition)."), root, .pop_flag(po$where[k]), pop) else
                 sprintf(t("%s's %s is not %s's for %d subjects: its condition is not taken as the population %s (written as a condition)."), root, .pop_flag(po$where[k]), po$dataset[k], n, pop))
  })
  # the same data already there: said, so it is not made twice
  output$adata_same <- shiny::renderUI({
    e <- adata_edit()
    shiny::req(!is.null(e), is.null(e$old), is.null(e$name))
    more <- c(input$adata_add, adata_derive_now(), input$adata_keep,
              input$adata_distinct, input$adata_code)
    shiny::req(!any(nzchar(trimws(more))))
    subjects <- adata_subj_value()
    ps <- adata_pop_split(subjects)
    same <- .adata_same_as(.adata_rows(rv$p, ard_target()), one_of(input$adata_from), ps$pop, subjects, ps$where)
    if (is.na(same)) return(NULL)
    shiny::div(class = "alert alert-info small py-1 mb-2",
               sprintf(t("%s, already there, is this same data: no need to make it again (in 1-2, choose %s)."),
                       same, same))
  })
  # the form's values as a planner with them (an error, said, when it is
  # not a valid analysis data)
  adata_planner <- function() {
    e <- adata_edit()
    one <- one_of
    id <- trimws(input$adata_id %||% "")
    p <- rv$p
    if (!is.null(e$name)) {
      sp <- .an_data_split(e$name)
      p <- name_analysis_data(p, ard_target(), sp$dataset, sp$pop, id, label = one(input$adata_label))
      e$old <- id
    }
    from <- one(input$adata_from)
    code <- if (nzchar(trimws(input$adata_code %||% ""))) input$adata_code else NA_character_
    subjects <- adata_subj_value()
    ps <- adata_pop_split(subjects)
    pop <- ps$pop
    add <- one(input$adata_add)
    keep <- one(input$adata_keep)
    distinct <- one(input$adata_distinct)
    where <- ps$where
    derive <- one(adata_derive_now())
    # written as R: the fields that make a data are not used
    if (!is.na(code)) {
      pop <- subjects <- where <- add <- derive <- keep <- distinct <- NA
    }
    p <- set_analysis_data(p, ard_target(), id, from = from, population_id = pop, subjects = subjects,
                           where = where, add = add,
                           derive = derive, keep = keep,
                           distinct = distinct, label = one(input$adata_label),
                           old = e$old, code = code)
    suppressWarnings(.ard_spec(p$ard))
    p
  }
  shiny::observeEvent(input$ard_adata_new, {
    tg <- ard_target()
    shiny::req(has_study(), tg)
    p <- rv$p
    adata_pick(NULL)
    # the first one is the subjects' (ADSL); then the data kept to them
    subj <- .adata_subject_level(p, ard_target())
    ds <- p$ard$datasets$dataset
    pop_ds <- intersect(adata_pop_ds(), ds)
    from <- if (!length(subj) && length(pop_ds)) pop_ds[1L] else c(setdiff(ds, pop_ds), ds)[1L]
    mine <- .adata_of_report(p, tg)
    s1 <- c(intersect(mine, subj), subj)[1L]
    suf <- if (!is.na(s1) && grepl("_", s1)) sub("^[^_]*_", "", s1) else NA
    nm <- if (!is.na(suf) && !from %in% pop_ds) .adata_default_name(from, suf) else NA
    # not a name the program has already (adae_saf for ADAE x SAF read as it is)
    if (is.na(nm) || nm %in% .adata_taken_names(p, ard_target())) nm <- .adata_suggest(p, ard_target(), from, NA)
    adata_edit(list(old = NULL, suggested = nm))
    adata_form(list(from = from, data_id = nm), t("New analysis data"))
  })
  shiny::observeEvent(input$adata_close, adata_close())
  shiny::observeEvent(input$ard_adata_pick, {
    id <- input$ard_adata_pick
    if (identical(shiny::isolate(adata_pick()), id)) return(adata_close())
    ad <- .adata_rows(rv$p, ard_target())
    shiny::req(id %in% ad$data_id)
    adata_pick(id)
    u <- .adata_uses(rv$p, ard_target(), id)
    note <- if (length(u$analyses) || length(u$data)) shiny::div(
      class = "alert alert-secondary small py-1",
      sprintf(t("Used by: %s."), paste(c(u$analyses, u$data), collapse = ", ")), " ",
      t("A change makes these reports' ARD need making again."))
    adata_edit(list(old = id, note = note))
    adata_form(as.list(ad[match(id, ad$data_id), ]), sprintf(t("Analysis data %s"), id))
  })
  shiny::observeEvent(input$ard_adata_name, {
    v <- input$ard_adata_name
    # a second click closes it, as on a named data
    if (identical(shiny::isolate(adata_pick()), paste0("auto:", v))) return(adata_close())
    sp <- .an_data_split(v)
    shiny::req(!is.na(sp$pop))
    po <- rv$p$ard$populations
    pop_ds <- po$dataset[match(sp$pop, po$population_id)]
    from <- if (is.na(sp$dataset)) pop_ds else sp$dataset
    a <- st_rows()
    d <- a$data %||% rep(NA_character_, nrow(a))
    par <- a$parent %||% rep(NA_character_, nrow(a))
    v_of <- vapply(seq_len(nrow(a)), function(i)
      .an_data_value(a$dataset[i], a$population_id[i]), "")
    on <- a[.is_blank_v(d) & .is_blank_v(par) & v_of == v, , drop = FALSE]
    w <- unique(on$where)
    common <- if (length(w) == 1L) w else NA
    note <- shiny::div(
      class = "alert alert-secondary small py-1",
      sprintf(t("On saving, this report's analyses %s read this data."),
              paste(on$analysis_id, collapse = ", ")), " ",
      if (!is.na(common)) sprintf(t("The condition they all have, %s, moves from them to the data (each one's own conditions stay)."),
                                  common))
    adata_pick(paste0("auto:", v))
    adata_edit(list(old = NULL, name = v, note = note, reads = on$analysis_id))
    # the name the program gives it now -- unless that is the program's own
    # (adsl_sub) or an analysis data has it
    nm <- .an_data_name(sp$dataset, sp$pop, po)
    if (.an_program_name(nm) || nm %in% .adata_rows(rv$p, ard_target())$data_id ||
        nm %in% tolower(rv$p$ard$datasets$dataset)) {
      nm <- .adata_suggest(rv$p, ard_target(), from, sp$pop, common)
    }
    adata_form(list(data_id = nm, from = from, population_id = sp$pop, where = common),
               sprintf(t("%s (no name yet: saving names it)"), data_words(sp$dataset, sp$pop)))
  })
  shiny::observeEvent(input$adata_code_start, {
    shiny::req(!is.null(adata_edit()))
    e <- adata_edit()
    p <- tryCatch({
      q <- rv$p
      # the fields as they are now, without a code
      tmp <- trimws(input$adata_id %||% "")
      if (!nzchar(tmp)) stop("no name")
      ps <- adata_pop_split(adata_subj_value())
      set_analysis_data(q, ard_target(), tmp, from = one_of(input$adata_from), subjects = adata_subj_value(),
                        population_id = ps$pop,
                        where = ps$where, add = one_of(input$adata_add),
                        derive = one_of(adata_derive_now()), keep = one_of(input$adata_keep),
                        distinct = one_of(input$adata_distinct), old = e$old)
    }, error = function(err) NULL)
    v <- if (is.null(p)) NA else .adata_code_start(p, ard_target(), trimws(input$adata_id))
    if (is.na(v)) return(notify(t("The code cannot be written yet: give the data a name and what it is made from."), "warning"))
    shiny::updateTextAreaInput(session, "adata_code", value = v)
  })
  shiny::observeEvent(input$adata_preview, {
    p <- tryCatch(adata_planner(), error = function(e) e)
    output$adata_preview_out <- shiny::renderUI({
      if (inherits(p, "error")) {
        return(shiny::div(class = "alert alert-warning small py-1 mt-2", conditionMessage(p)))
      }
      id <- trimws(input$adata_id)
      d <- .adata_make(p, rv$study$path, ard_target(), id)
      if (is.null(d) || !is.null(attr(d, "error"))) {
        return(shiny::div(class = "alert alert-warning small py-1 mt-2",
                          t("It could not be made:"), " ", attr(d, "error") %||% ""))
      }
      w <- adata_count_words
      cols <- .adata_preview_cols(d, list(add = one_of(input$adata_add),
                                          distinct = one_of(input$adata_distinct),
                                          derive = one_of(adata_derive_now()),
                                          where = adata_where_now()))
      output$adata_preview_tbl <- shiny::renderTable(
        utils::head(d[cols], 5L), striped = TRUE, spacing = "xs")
      shiny::tagList(
        shiny::p(class = "small mt-2 mb-1", .adata_count_words(d, w)),
        shiny::div(style = "max-height: 14rem; overflow: auto;", class = "small",
                   shiny::tableOutput("adata_preview_tbl")))
    })
  })
  shiny::observeEvent(input$adata_save, {
    p <- tryCatch(adata_planner(), error = function(e) e)
    if (inherits(p, "error")) return(notify(conditionMessage(p), "warning"))
    e <- adata_edit()
    id <- trimws(input$adata_id)
    kept <- character()
    if (is.null(e$old) && id %in% .adata_subject_level(p, ard_target())) {
      others <- setdiff(.adata_of_report(p, ard_target()), .adata_subject_level(p, ard_target()))
      p <- .adata_keep_to(p, ard_target(), id, others)
      kept <- attr(p, "kept")
      attr(p, "kept") <- NULL
    }
    rv$p <- p
    adata_close()
    bump()
    if (length(kept)) {
      notify(sprintf(t("%s are kept to the subjects of %s now (untick it in their form if one should not be)."),
                     paste(kept, collapse = ", "), id))
    }
    notify(if (!is.null(e$name)) {
      sprintf(t("%s is made; %s read it now."), id, paste(e$reads, collapse = ", "))
    } else if (is.null(e$old)) sprintf(t("%s is made."), id) else sprintf(t("%s is changed."), id))
  })
  # analysis data of another report copied into this one (the same names;
  # one the report has already gets _1 ...)
  shiny::observeEvent(input$ard_adata_import, {
    tg <- ard_target()
    shiny::req(has_study(), tg)
    ad <- .adata_rows(rv$p)
    from <- setdiff(unique(ad$output_id[!is.na(ad$output_id)]), tg)
    if (!length(from)) return(notify(t("No other report has analysis data."), "warning"))
    shiny::showModal(shiny::modalDialog(
      title = t("Copy analysis data from another report"), easyClose = TRUE,
      shiny::selectInput("adata_import_from", t("From the report"), from, width = "100%"),
      shiny::uiOutput("adata_import_which"),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("adata_import_ok", t("Copy"), class = "btn-primary"))))
  })
  output$adata_import_which <- shiny::renderUI({
    f <- input$adata_import_from
    shiny::req(f)
    ad <- .adata_rows(rv$p, f)
    lab <- ifelse(.is_blank_v(ad$label), ad$data_id, paste(ad$data_id, "\u2014", ad$label))
    shiny::checkboxGroupInput("adata_import_ids", t("Its analysis data (what they are made from and kept to come with them)"),
                              stats::setNames(ad$data_id, lab), selected = ad$data_id,
                              width = "100%")
  })
  shiny::observeEvent(input$adata_import_ok, {
    tg <- ard_target()
    ids <- input$adata_import_ids
    if (!length(ids)) return(notify(t("Choose at least one."), "warning"))
    p2 <- guarded(import_analysis_data(rv$p, input$adata_import_from, tg, ids))
    if (is.null(p2)) return()
    made <- attr(p2, "copied")
    attr(p2, "copied") <- NULL
    rv$p <- p2
    shiny::removeModal()
    adata_close()
    bump()
    notify(sprintf(t("%s copied from %s (save to keep them)."), paste(made, collapse = ", "), input$adata_import_from))
  })
  shiny::observeEvent(input$ard_adata_copy, {
    id <- adata_pick()
    shiny::req(id)
    p2 <- guarded(copy_analysis_data(rv$p, ard_target(), id))
    if (is.null(p2)) return()
    cid <- attr(p2, "copied")
    attr(p2, "copied") <- NULL
    rv$p <- p2
    adata_close()
    bump()
    notify(sprintf(t("%s is copied as %s."), id, cid))
  })
  shiny::observeEvent(input$ard_adata_del, {
    id <- adata_pick()
    shiny::req(id)
    u <- .adata_uses(rv$p, ard_target(), id)
    if (length(u$analyses) || length(u$data)) {
      return(notify(sprintf(t("%s is used: %s"), id, paste(c(u$analyses, u$data), collapse = ", ")),
                    "warning"))
    }
    shiny::showModal(shiny::modalDialog(
      title = t("Delete the analysis data"), easyClose = TRUE,
      shiny::p(sprintf(t("Delete %s? No analysis reads it."), id)),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("ard_adata_del_ok", t("Delete"), class = "btn-danger"))))
  })
  shiny::observeEvent(input$ard_adata_del_ok, {
    id <- adata_pick()
    shiny::req(id)
    p2 <- guarded(remove_analysis_data(rv$p, ard_target(), id))
    adata_close()
    shiny::removeModal()
    if (is.null(p2)) return()
    rv$p <- p2
    bump()
    notify(sprintf(t("%s is deleted."), id))
  })
  output$ard_outline <- shiny::renderUI({
    a <- st_rows()
    tg <- ard_target()
    if (is.null(tg)) return(shiny::p(class = "small text-muted", t("Choose a report above, or in the list on the left (> opens it).")))
    if (is.null(a) || !nrow(a)) {
      return(shiny::div(class = "small text-muted mb-2",
                        sprintf(t("%s has no analyses yet."), tg), " ",
                        .btn("ard_an_new", t("New analysis"), class = "btn-sm btn-outline-primary")))
    }
    o <- .an_outline(a)
    pick <- an_pick() %||% NA
    cond <- st_conditions()
    js <- function(input, value) sprintf(
      "Shiny.setInputValue('%s', %s, {priority: 'event'}); event.stopPropagation();",
      input, value)
    rows <- lapply(seq_len(nrow(o)), function(k) {
      r <- a[o$row[k], ]
      id <- r$analysis_id
      role <- st_role(r, a)
      what <- if (role == "parent") {
        paste0(data_words(r$dataset, r$population_id, r$data),
               if (!.is_blank(r$by)) paste0(" \u00b7 ", t("by"), " ",
                                            gsub(" | ", ", ", r$by, fixed = TRUE)) else "",
               if (.overall_on(r$overall)) paste0(" \u00b7 ", t("and over all")) else "")
      } else paste(.split_bar(r$variables), collapse = ", ")
      own <- if (role == "single") {
        c(data_words(r$dataset, r$population_id, r$data),
          if (!.is_blank(r$by)) paste(t("by"), gsub(" | ", ", ", r$by, fixed = TRUE)),
          if (.overall_on(r$overall)) t("and over all"),
          if (!.is_blank(r$where)) r$where)
      }
      what <- paste(c(if (nzchar(what)) what, own), collapse = " \u00b7 ")
      probs <- st_problems(tg, id)
      cn <- if (!is.null(cond) && nrow(cond) && "analysis_id" %in% names(cond))
        cond[cond$analysis_id %in% id, , drop = FALSE] else NULL
      kids <- if (role == "inside") .stack_kids(a, r$parent) else integer()
      first <- length(kids) && o$row[k] == min(kids)
      last <- length(kids) && o$row[k] == max(kids)
      shiny::div(
        class = paste("ard-ol-row d-flex align-items-start gap-2 px-2 py-1 border-bottom small",
                      if (identical(id, pick)) "bg-primary-subtle"),
        style = paste0("cursor: pointer; padding-left: ", 0.5 + 1.75 * o$depth[k], "rem !important;"),
        onclick = js("ard_ol_pick", jsonlite::toJSON(id, auto_unbox = TRUE)),
        shiny::div(class = "flex-grow-1",
                   shiny::strong(id), " ",
                   shiny::span(fn_label(r$method)),
                   fn_code(r$method, "small text-body-secondary"),
                   if (!.is_blank(r$label)) shiny::span(class = "text-muted", paste0(" \u2014 ", r$label)),
                   shiny::div(class = "text-muted", what)),
        if (length(probs)) shiny::span(class = "badge text-bg-danger", title = paste(probs, collapse = "\n"),
                                       t("definition error")),
        if (length(chk <- st_problems(tg, id, c("check", "hand")))) shiny::span(
          class = "badge text-bg-warning", title = paste(chk, collapse = "\n"),
          t("to check")),
        if (!is.null(cn) && nrow(cn)) shiny::span(
          class = paste("badge", if (any(cn$level == "error")) "text-bg-danger" else "text-bg-warning"),
          title = paste(unique(cn$message), collapse = "\n"),
          sprintf(t("ARD: %d messages"), nrow(cn))),
        if (role == "inside") shiny::div(
          class = "btn-group btn-group-sm",
          shiny::tags$button(type = "button", class = "btn btn-outline-secondary py-0",
                             title = t("Up"), disabled = if (first) NA,
                             onclick = js("ard_ol_move", sprintf("{id: %s, by: -1}",
                                                                 jsonlite::toJSON(id, auto_unbox = TRUE))),
                             "\u2191"),
          shiny::tags$button(type = "button", class = "btn btn-outline-secondary py-0",
                             title = t("Down"), disabled = if (last) NA,
                             onclick = js("ard_ol_move", sprintf("{id: %s, by: 1}",
                                                                 jsonlite::toJSON(id, auto_unbox = TRUE))),
                             "\u2193")))
    })
    # the report's ARD: made, outdated or not yet (one for the report)
    stt <- tryCatch(ard_state(), error = function(e) NULL)
    k <- if (!is.null(stt)) match(tg, stt$output_id) else NA
    state_line <- if (!is.na(k)) {
      s0 <- stt$state[k]
      # what the state means, and the next step (the Preview button makes
      # this report's ARD)
      words <- switch(s0,
        outdated = t("needs making again (its definition has changed)"),
        `not built` = t("not made yet (nothing to show)"),
        t(.ard_state_labels[[s0]]))
      why <- switch(s0,
        outdated = t("Since this report's ARD was made, something it is made from has changed: its analyses, their populations or datasets, the study's settings, the study's own ARD functions or the code lists. Make it again to see the tables as the definition is now."),
        `not built` = t("This report's ARD has not been made yet: make it to see its tables."),
        error = t("Its ARD program failed: see what it printed (Data > Study ARD), fix it and make it again."),
        NULL)
      go <- switch(s0, `not built` = t("Make the ARD"),
                   outdated = , error = t("Make the ARD again"), NULL)
      shiny::div(
        class = "small mb-1", title = why,
        sprintf(t("This report's ARD: %s"), words),
        if (!is.null(go)) shiny::actionLink("ard_make_study", go, title = why,
                                            class = "btn btn-sm btn-link py-0"))
    }
    picked <- !is.na(pick) && pick %in% a$analysis_id
    prole <- if (picked) st_role(a[a$analysis_id == pick, , drop = FALSE][1L, ], a) else ""
    shiny::tagList(
      state_line,
      shiny::div(
        class = "d-flex flex-wrap gap-1 align-items-center mb-1",
        .btn("ard_an_new", t("New analysis"), class = "btn-sm btn-outline-primary py-0"),
        .btn("ard_an_copy", t("Copy"), class = "btn-sm btn-outline-secondary py-0",
             disabled = if (!picked) NA),
        # a stack asks what becomes of the analyses inside it
        .btn(if (identical(prole, "parent")) "ard_stack_delete" else "ard_an_delete",
             t("Delete..."), class = "btn-sm btn-outline-danger py-0",
             disabled = if (!picked) NA),
        if (identical(prole, "parent"))
          .btn("ard_stack_ungroup", t("Ungroup..."), class = "btn-sm btn-outline-secondary py-0"),
        shiny::span(class = "small text-muted ms-2",
                    if (picked) sprintf(t("%s chosen: the form below"), pick) else
                      t("A click on an analysis opens it below; another click closes it."))),
      shiny::div(class = "border rounded mb-1", rows),
      if (any(o$depth == 1L)) shiny::div(
        class = "small text-muted",
        with_tip(t("The order inside a stack"),
                 t("The analyses inside a stack are computed in this order, and so are their variables: a table whose variables have no order of their own shows them so."))))
  })
  # a click opens an analysis below the list; on the one open, closes it
  shiny::observeEvent(input$ard_ol_pick, {
    v <- input$ard_ol_pick
    an_pick(if (identical(shiny::isolate(an_pick()), v)) NULL else v)
  })
  # [Copy]: the analysis chosen, as a new one after it (a stack with the
  # analyses inside it)
  shiny::observeEvent(input$ard_an_copy, {
    tg <- ard_target()
    id <- an_pick()
    shiny::req(tg, id)
    p2 <- guarded(copy_analysis(rv$p, tg, id))
    if (is.null(p2)) return()
    cid <- attr(p2, "copied")
    attr(p2, "copied") <- NULL
    rv$p <- p2
    an_pick(cid)
    bump()
    notify(sprintf(t("%s is copied as %s."), id, cid))
  })
  shiny::observeEvent(input$ard_ol_move, {
    m <- input$ard_ol_move
    tg <- ard_target()
    shiny::req(tg, m$id)
    rv$p <- stack_move(rv$p, tg, m$id, as.integer(m$by))
    an_pick(m$id)
    bump()
  })

  # the groups an analysis can have, from the data it reads
  an_by_choices <- function(r, now = character()) {
    fd <- form_data(r)
    ds <- an_dataset(fd$dataset, fd$pop)
    d <- if (!is.na(ds %||% NA)) an_data(ds)
    if (is.null(d)) return(stats::setNames(now, now))
    cols <- as.list(d)
    rows <- .row_choices(cols, c(continuous = t("numbers"), categorical = t("counts")))
    kinds <- vapply(unname(rows), function(n) .column_kind(cols[[n]]), "")
    grp <- .group_choices(cols)
    ch <- c(grp, rows[kinds == "categorical" & !rows %in% grp])
    c(ch, stats::setNames(setdiff(now, ch), setdiff(now, ch)))
  }
  .stack_flag_words <- c(
    .by_stats = "The subjects per group (the column headers' N)",
    .total_n = "The subjects in all",
    .missing = "Rows for the missing values",
    .attributes = "The variables' labels and types")
  # a stack's own form: on what it runs the analyses inside, what it adds,
  # and those analyses (opened, moved, one added)
  stack_form_ui <- function(r, tg) shiny::isolate({
    # drawn once: the fields' own inputs must not draw it again
    blank_na <- function(x) if (is.na(x)) "" else x
    a <- shiny::isolate(st_rows())
    kids <- a[.stack_kids(a, r$analysis_id), , drop = FALSE]
    fl <- .stack_flags_of(r$args)
    # the report's own rows that count the subjects per group (GROUPN)
    groupn <- a$analysis_id[.stack_group_n(a, r)]
    js <- function(input, value) sprintf(
      "Shiny.setInputValue('%s', %s, {priority: 'event'});", input, value)
    shiny::tagList(
      shiny::h6(class = "mb-1", `data-an` = r$analysis_id, with_tip(
        sprintf(t("Analysis %s"), r$analysis_id),
        t("A stack: it runs the analyses inside it together (cards::ard_stack), in one call (the column headers' N too)."))),
      bslib::layout_columns(
        col_widths = c(4, 8),
        shiny::textInput(st_id("id"), with_tip(t("Analysis ID (a set of analyses)"), t("Several variables: in the ARD, one row per variable under the same ID.")), r$analysis_id),
        shiny::textInput(st_id("label"), t("Label"), blank_na(r$label), width = "100%")),
      shiny::selectInput(st_id("data"), with_tip(t("Data"), t("The rows the analysis reads, chosen from the definitions: a dataset (Data tab) kept to an analysis set (Populations tab), or an analysis data of 1-1 (the same, with more done to it). The population is part of the choice. The name is the one the program gives it.")),
                         data_choices(r),
                         selected = .an_data_value_row(r),
                         width = "100%"),
      shiny::tags$details(
        class = "mb-2", open = if (!is.na(r$where)) NA,
        shiny::tags$summary(class = "small", with_tip(argl("This analysis's own filter", "where"),
                                                      t("An R condition that filters this analysis's rows alone (e.g. AESER == \"Y\"). The data's own filter is 1-1's."))),
        shiny::textInput(st_id("where"), NULL, blank_na(r$where), width = "100%",
                         placeholder = "AESER == \"Y\""),
        .where_labels_note(sheet_rows(shiny::isolate(rv$p), "codelists",
                                      shiny::isolate(current()) %||% ""), t)),
      shiny::selectizeInput(
        st_id("by"), argl("Grouping variables", ".by"),
        an_by_choices(r, .split_bar(r$by)), .split_bar(r$by), multiple = TRUE,
        width = "100%", options = list(plugins = list("remove_button", "drag_drop"))),
      shiny::checkboxInput(
        st_id("overall"),
        with_tip(argl("Also over all subjects", "overall"),
                 t("The stack once more without its grouping variables (.overall = TRUE): the Total column of a table.")),
        .overall_on(r$overall), width = "100%"),
      shiny::h6(class = "small fw-bold mt-1", t("What it adds to the analyses inside")),
      lapply(names(.stack_flag_words), function(k) shiny::checkboxInput(
        st_id(paste0("fl", k)),
        paste0(t(.stack_flag_words[[k]]), " (", k, ")",
               if (k == ".by_stats" && length(groupn)) sprintf(t(" -- %s counts them now"),
                                                             paste(groupn, collapse = ", ")) else ""),
        isTRUE(fl$flags[[k]]), width = "100%")),
      shiny::uiOutput("ard_stack_n_note"),
      shiny::tags$details(
        class = "mb-2", open = if (nzchar(fl$other)) NA,
        shiny::tags$summary(class = "small", with_tip(argl("Other arguments (R)", "args"), t("cards' arguments, in R (e.g. conf.level = 0.9)."))),
        shiny::textInput(st_id("args_other"), NULL, fl$other, width = "100%")),
      shiny::h6(class = "small fw-bold mt-2", t("The analyses inside")),
      if (!nrow(kids)) shiny::p(class = "small text-muted", t("None yet: add one.")),
      lapply(seq_len(nrow(kids)), function(i) shiny::div(
        class = "d-flex gap-2 align-items-center small border-bottom py-1",
        shiny::strong(kids$analysis_id[i]),
        shiny::span(fn_label(kids$method[i])),
        shiny::span(class = "text-muted flex-grow-1",
                    paste(.split_bar(kids$variables[i]), collapse = ", ")),
        shiny::tags$button(type = "button", class = "btn btn-sm btn-link py-0",
                           onclick = js("ard_ol_pick", jsonlite::toJSON(kids$analysis_id[i], auto_unbox = TRUE)),
                           t("Open")))),
      shiny::div(
        class = "d-flex flex-wrap gap-2 align-items-center mt-2",
        .btn("ard_stack_add", t("Add an analysis inside"), class = "btn-sm btn-outline-primary"),
        .btn("ard_stat_apply", t("Apply to the analysis"), class = "btn-sm btn-primary")),
      shiny::tags$details(
        class = "mt-2", open = NA,
        shiny::tags$summary(class = "small", t("This analysis as code")),
        shiny::div(class = "rp-code", shiny::verbatimTextOutput("ard_an_code"))))
  })
  # the stack counting the subjects per group while GROUPN does: said at once
  output$ard_stack_n_note <- shiny::renderUI({
    st_drawn()
    r <- shiny::isolate(st_row())
    if (!identical(st_role(r), "parent")) return(NULL)
    on <- input[[st_id("fl.by_stats")]]
    if (!isTRUE(on)) return(NULL)
    a <- shiny::isolate(st_rows())
    groupn <- a$analysis_id[.stack_group_n(a, r)]
    if (!length(groupn)) return(NULL)
    shiny::div(
      class = "alert alert-danger py-1 small",
      sprintf(t(.n_twice_words), r$analysis_id, paste(groupn, collapse = ", ")),
      " ", .btn("ard_stack_del_groupn", sprintf(t("Delete %s"), paste(groupn, collapse = ", ")),
                class = "btn-sm btn-outline-danger py-0"))
  })
  shiny::observeEvent(input$ard_stack_del_groupn, {
    r <- st_row()
    tg <- ard_target()
    shiny::req(tg, st_role(r) == "parent")
    a <- st_rows()
    drop <- a$analysis_id[.stack_group_n(a, r)]
    a <- a[!a$analysis_id %in% drop, , drop = FALSE]
    a$output_id <- NULL
    # the stack counts them now
    i <- match(r$analysis_id, a$analysis_id)
    fl <- .stack_flags_of(a$args[i])
    fl$flags[[".by_stats"]] <- TRUE
    a$args[i] <- .stack_args(fl$flags, fl$other)
    rv$p <- set_ard_rows(rv$p, "analyses", tg, a)
    bump()
    notify(sprintf(t("%s deleted: %s counts the subjects per group now."),
                   paste(drop, collapse = ", "), r$analysis_id))
  })
  # one inside a stack: on what it runs, said; how to change that for it
  stack_inside_note <- function(r) {
    p <- st_parent_of(r)
    js <- sprintf("Shiny.setInputValue('ard_ol_pick', %s, {priority: 'event'});",
                  jsonlite::toJSON(p$analysis_id, auto_unbox = TRUE))
    shiny::div(
      class = "alert alert-secondary py-1 small",
      sprintf(t("Inside %s (%s): it runs on %s's data%s%s."), p$analysis_id, fn_label(p$method),
              p$analysis_id,
              sprintf(t(", %s"), data_words(p$dataset, p$population_id, p$data)),
              if (!.is_blank(p$by)) paste0(", ", t("by"), " ",
                                           gsub(" | ", ", ", p$by, fixed = TRUE)) else ""),
      shiny::div(class = "d-flex flex-wrap gap-2 mt-1",
                 shiny::tags$button(type = "button", class = "btn btn-sm btn-link p-0",
                                    onclick = js, sprintf(t("Open %s"), p$analysis_id)),
                 shiny::span(t("Another condition or other groups for this one alone:")),
                 .btn("ard_stack_out", sprintf(t("Take it out of %s"), p$analysis_id),
                      class = "btn-sm btn-link p-0")))
  }

  # [Add an analysis inside]
  shiny::observeEvent(input$ard_stack_add, {
    r <- st_row()
    tg <- ard_target()
    shiny::req(tg, st_role(r) == "parent")
    a <- st_rows()
    id <- .stack_free_id(a, "A")
    p2 <- guarded(stack_add_inside(rv$p, tg, r$analysis_id, id = id))
    if (is.null(p2)) return()
    rv$p <- p2
    an_pick(id)
    bump()
  })
  # [Take it out of STACK]: one analysis of its own again; the stack, left
  # with none, goes (keeping the N it gave, asked)
  shiny::observeEvent(input$ard_stack_out, {
    r <- st_row()
    tg <- ard_target()
    shiny::req(tg, st_role(r) == "inside")
    a <- st_rows()
    if (length(.stack_kids(a, r$parent)) > 1L) {
      rv$p <- stack_take_out(rv$p, tg, r$analysis_id)
      bump()
      return(notify(sprintf(t("%s is an analysis of its own again, on %s's data and groups."),
                            r$analysis_id, r$parent)))
    }
    stack_ask_n(sprintf(t("%s is the last analysis inside %s: %s goes."), r$analysis_id,
                        r$parent, r$parent), "ard_stack_out_ok")
  })
  shiny::observeEvent(input$ard_stack_out_ok, {
    r <- st_row()
    tg <- ard_target()
    shiny::req(tg)
    rv$p <- stack_take_out(rv$p, tg, r$analysis_id, keep_n = isTRUE(input$ard_stack_keep_n))
    shiny::removeModal()
    bump()
  })
  # ungrouping (and deleting a stack) asks whether to keep, as analyses of
  # their own, the subjects per group and the total N it gave
  stack_ask_n <- function(msg, ok, extra = NULL) {
    # the rows it would make: none when the report counts them already
    r <- st_row()
    if (identical(st_role(r), "inside")) r <- st_parent_of(r)
    a <- st_rows()
    a$output_id <- NULL
    n <- .stack_n_rows(a, r)
    shiny::showModal(shiny::modalDialog(
      title = t("Ungroup"), easyClose = TRUE,
      shiny::p(msg),
      extra,
      if (nrow(n)) shiny::checkboxInput(
        "ard_stack_keep_n",
        sprintf(t("Make %s: the subjects per group and the total N it gave, as analyses of their own (the column headers' N needs them)"),
                paste(n$analysis_id, collapse = ", ")),
        TRUE, width = "100%")
      else shiny::p(class = "small text-muted",
                    t("The report counts the subjects per group (and in all) already: no rows are made.")),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn(ok, t("OK"), class = "btn-primary"))))
  }
  shiny::observeEvent(input$ard_stack_ungroup, {
    r <- st_row()
    shiny::req(st_role(r) == "parent")
    stack_ask_n(sprintf(t("The analyses inside %s become analyses of their own, on its data, condition and groups; %s goes."),
                        r$analysis_id, r$analysis_id), "ard_stack_ungroup_ok")
  })
  shiny::observeEvent(input$ard_stack_ungroup_ok, {
    r <- st_row()
    tg <- ard_target()
    shiny::req(tg)
    a <- st_rows()
    first <- a$analysis_id[.stack_kids(a, r$analysis_id)][1L]
    rv$p <- stack_ungroup(rv$p, tg, r$analysis_id, keep_n = isTRUE(input$ard_stack_keep_n))
    shiny::removeModal()
    an_pick(first)
    bump()
  })
  # [Delete the analysis...]: asked first (a stack has its own Delete)
  shiny::observeEvent(input$ard_an_delete, {
    r <- st_row()
    shiny::req(r, st_role(r) %in% c("single", "inside"))
    lab <- if (!.is_blank(r$label)) paste0(" (", r$label, ")") else ""
    shiny::showModal(shiny::modalDialog(
      title = t("Delete the analysis"), easyClose = TRUE,
      shiny::p(sprintf(t("Delete %s%s from %s? Its statistics leave the report's ARD when the ARD is made again."),
                       r$analysis_id, lab, ard_target())),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("ard_an_delete_ok", t("Delete"), class = "btn-danger"))))
  })
  shiny::observeEvent(input$ard_an_delete_ok, {
    r <- st_row()
    tg <- ard_target()
    shiny::req(r, tg)
    p2 <- guarded(remove_analysis(rv$p, tg, r$analysis_id))
    if (is.null(p2)) return()
    rv$p <- p2
    shiny::removeModal()
    an_pick(NULL)
    bump()
    notify(sprintf(t("%s is deleted."), r$analysis_id))
  })
  shiny::observeEvent(input$ard_stack_delete, {
    r <- st_row()
    shiny::req(st_role(r) == "parent")
    stack_ask_n(sprintf(t("Delete %s."), r$analysis_id), "ard_stack_delete_ok",
                shiny::radioButtons("ard_stack_delete_how", NULL,
                                    stats::setNames(c("ungroup", "all"),
                                                    c(t("Keep the analyses inside, as analyses of their own"),
                                                      t("Delete them too"))),
                                    selected = "ungroup", width = "100%"))
  })
  shiny::observeEvent(input$ard_stack_delete_ok, {
    r <- st_row()
    tg <- ard_target()
    shiny::req(tg)
    rv$p <- stack_remove(rv$p, tg, r$analysis_id, how = input$ard_stack_delete_how %||% "ungroup",
                         keep_n = isTRUE(input$ard_stack_keep_n))
    shiny::removeModal()
    an_pick(NULL)
    bump()
  })
  # [Run together with other analyses...]: those that can, ticked; those
  # that cannot, listed faint with the reason
  stack_reason_words <- c(
    inside = "inside another already", method = "this kind cannot run inside one",
    data = "other data or population", where = "another condition",
    by = "other groups", strata = "other strata", denominator = "another denominator",
    variable = "a variable it has already",
    group_n = "the stack can count the subjects per group itself (this analysis counts them now)",
    total_n = "the stack can count the subjects in all itself (this analysis counts them now)")
  shiny::observeEvent(input$ard_stack_group, {
    r <- st_row()
    tg <- ard_target()
    shiny::req(tg, st_role(r) == "single")
    cand <- stack_candidates(rv$p, tg, r$analysis_id)
    ok <- cand$analysis_id[is.na(cand$reason)]
    no <- cand[!is.na(cand$reason), , drop = FALSE]
    desc <- rv$p$outputs$description[match(tg, rv$p$outputs$output_id)]
    shiny::showModal(shiny::modalDialog(
      title = t("Run analyses together (cards::ard_stack)"), easyClose = TRUE,
      shiny::p(class = "small", t("One call on the same data, population, condition and groups: it also counts the subjects per group (the column headers' N). Subjects with no group are not counted.")),
      shiny::checkboxGroupInput("ard_stack_pick", t("The analyses inside"), ok, selected = ok,
                                width = "100%"),
      if (nrow(no)) shiny::div(
        class = "small text-body-tertiary",
        shiny::div(t("Cannot be inside:")),
        lapply(seq_len(nrow(no)), function(i) shiny::div(
          class = "ms-2", sprintf("%s \u2014 %s", no$analysis_id[i],
                                  t(stack_reason_words[[no$reason[i]]]))))),
      shiny::textInput("ard_stack_label", t("Label"),
                       if (.is_blank(desc)) "" else desc, width = "100%"),
      shiny::p(class = "small text-muted mt-n2",
               sprintf(t("Its ID is %s (it can be changed on its form)."),
                       .stack_free_id(ard_rows(rv$p, "analyses", tg)))),
      footer = shiny::tagList(shiny::modalButton(t("Cancel")),
                              .btn("ard_stack_group_ok", t("Run them together"), class = "btn-primary"))))
  })
  shiny::observeEvent(input$ard_stack_group_ok, {
    tg <- ard_target()
    ids <- input$ard_stack_pick
    shiny::req(tg)
    if (!length(ids)) return(notify(t("Choose the analyses."), "warning"))
    lab <- trimws(input$ard_stack_label %||% "")
    p2 <- guarded(stack_group(rv$p, tg, ids, label = if (nzchar(lab)) lab else NA_character_))
    if (is.null(p2)) return()
    a <- ard_rows(p2, "analyses", tg)
    s <- a[a$method %in% .stack_fn & !a$analysis_id %in% ard_rows(rv$p, "analyses", tg)$analysis_id, ]
    rv$p <- p2
    shiny::removeModal()
    an_pick(s$analysis_id[1L])
    bump()
    if (isFALSE(.stack_flags_of(s$args[1L])$flags[[".by_stats"]])) {
      notify(sprintf(t("%s does not count the subjects per group: the report's GROUPN does already (two would break the column headers' N). Delete GROUPN and tick it on %s to let the stack count them."),
                     s$analysis_id[1L], s$analysis_id[1L]))
    }
  })

  ard_valid <- shiny::reactive({
    shiny::req(has_study())
    tryCatch({
      .ard_spec(rv$p$ard)
      NULL
    }, error = function(e) conditionMessage(e))
  })
  output$ard_check <- shiny::renderUI({
    msg <- ard_valid()
    # analyses of an output the report list does not have: they run, but
    # no table reads them until it is added (Reports > Add)
    ids <- unique(stats::na.omit(rv$p$ard$analyses$output_id))
    orphan <- setdiff(ids, rv$p$outputs$output_id)
    note <- if (length(orphan)) shiny::div(
      class = "alert alert-info py-1 small mt-2",
      sprintf(t("%s: analyses of no report yet. Add the report to the Report list to make its table."),
              paste(orphan, collapse = ", ")))
    if (is.null(msg)) {
      a <- rv$p$ard$analyses
      n <- nrow(a)
      tg <- ard_target()
      mine <- if (!is.null(tg)) sum(!is.na(a$output_id) & a$output_id == tg) else NA
      return(shiny::tagList(
        shiny::p(class = "small text-success mt-2",
                 if (is.na(mine)) sprintf(t("%d analyses; the definition reads without errors."), n) else
                   sprintf(t("This report's analyses: %d (the study's: %d). The definition reads without errors."),
                           mine, n)),
        note))
    }
    if (length(.old_format(rv$p))) {
      return(shiny::div(
        class = "alert alert-warning py-1 small mt-2",
        shiny::div(t(.old_format_line)),
        shiny::tags$details(
          shiny::tags$summary(t("The checks' messages")),
          shiny::tags$pre(class = "mb-0", msg))))
    }
    shiny::div(class = "alert alert-warning py-1 small mt-2",
               shiny::tags$pre(class = "mb-0", msg))
  })
  # the output the sidebar names: a report, or an output the ARD
  # definition has analyses for and the report list does not have yet
  ard_target <- shiny::reactive({
    tg <- target()
    if (is.na(tg) || !nzchar(tg)) NULL else tg
  })
  ard_scope_id <- shiny::reactive({
    if (identical(input$ard_scope, "study")) NULL else ard_target()
  })
  output$ard_code <- shiny::renderText({
    shiny::req(has_study())
    a <- rv$p$ard
    if (!nrow(a$analyses)) {
      return(t("No analyses yet: add rows to the analyses sheet."))
    }
    code <- ard_code_now()
    if (is.character(code) && length(code) == 1L && !is.null(attr(code, "msg"))) {
      return(code)
    }
    paste(code, collapse = "\n")
  })
  # SPEC | Code | Result of step 1 (R/result_tabs.R): SPEC the analyses
  # sheet (this report's, or every report's), Result this report's ARD
  shiny::observeEvent(an_pick(), {
    if (!is.null(an_pick())) adata_close()
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  output$ard_spec_pane <- shiny::renderUI({
    if (identical(step2_focus(), "adata")) {
      return(shiny::tagList(
        shiny::p(class = "small text-muted mb-1",
                 t("The rows of analysis_data this report reads, every column, as in the workbook: what the form does not show is written here. A new data's row comes when it is saved.")),
        rhandsontable::rHandsontableOutput("hot_adata_report")))
    }
    shiny::tagList(
      shiny::checkboxInput("ard_all", t("Every report's analyses (with output_id)"), FALSE),
      rhandsontable::rHandsontableOutput("hot_ard_analyses"))
  })
  output$ard_result_pane <- shiny::renderUI({
    if (identical(step2_focus(), "adata")) {
      return(shiny::tagList(
        shiny::div(
          class = "d-flex flex-wrap gap-2 align-items-center",
          .btn("adata_preview", t("Preview"), class = "btn-sm btn-primary"),
          shiny::span(class = "small text-muted",
                      t("Makes the analysis data of the form (as it is now, not saved yet) from the study's data: its rows and subjects, the first rows."))),
        shiny::uiOutput("adata_preview_out")))
    }
    shiny::tagList(
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center",
      preview_button("ard_preview", ard_target(), "btn-sm btn-primary"),
      help_tip(t(.preview_tip))),
    shiny::uiOutput("ard_run_info"),
    shiny::div(class = "rp-resize",
               DT::DTOutput("ard_table", height = "auto", fill = FALSE)))
  })
  # what the Code panel shows, as the save would write it
  ard_code_now <- shiny::reactive({
    a <- structure(rv$p$ard, class = "tfl_ard_spec")
    msg <- function(x) structure(x, msg = TRUE)
    scope <- input$ard_scope %||% "report"
    id <- ard_target()
    if (scope == "report") {
      if (is.null(id)) return(msg(t("Choose a report above, or in the list on the left (> opens it).")))
      if (!any(a$analyses$output_id %in% id)) {
        return(msg(sprintf(t("%s has no analyses in the ARD definition."), id)))
      }
    }
    tryCatch(switch(scope,
      report = ard_program_code(a, id, dir = rv$study$path,
                                codelists = .study_codelists(rv$p)),
      setup = ard_setup_code(a),
      autoexec = ard_autoexec_code(a)),
      error = function(e) msg(paste(
        if (length(.old_format(rv$p))) paste0(t(.old_format_line), "\n\n"),
        t("The code cannot be written yet:"), conditionMessage(e))))
  })
  output$ard_prog_state <- shiny::renderUI({
    shiny::req(has_study())
    code <- ard_code_now()
    if (!is.null(attr(code, "msg"))) return(NULL)
    rv$status_ver
    lay <- study_layout()
    name <- switch(input$ard_scope %||% "report",
                   report = .ard_prog_name(ard_target()),
                   setup = .ard_setup_file, autoexec = .ard_autoexec_file)
    f <- file.path(rv$study$path, lay[["programs_ard"]], name)
    st <- .ard_program_state(code, f)
    shiny::p(class = "small text-muted mb-1",
             shiny::code(file.path(lay[["programs_ard"]], name)), " ",
             t(switch(st,
               missing = "No file yet. Saving writes it.",
               current = "The saved program is the one below.",
               generated = "The definition has changed: saving rewrites the program as below.",
               edited = "The saved program was edited by hand. Saving writes it again from the definition (the edited one is kept in programs/.edited/).")))
  })
  shiny::observeEvent(input$ard_preview, {
    if (is.null(ard_target())) return(notify(t("Choose a report"), "warning"))
    if (isTRUE(do_preview(ard_target()))) bslib::nav_select("ard_right", "result")
  })
  # making the study ARD: always the saved programs (programs/ard/), so
  # what is built is what anyone rerunning autoexec_ard.R gets
  ard_log_ver <- shiny::reactiveVal(0L)
  ard_preview_out <- shiny::reactiveVal(NULL)
  ard_ready <- function() {
    if (!is.null(ard_valid())) {
      notify(t("Correct the ARD definition first."), "warning")
      return(FALSE)
    }
    !(dirty() && !do_save())
  }
  output$ard_run_info <- shiny::renderUI({
    r <- ard_res()
    if (is.null(r)) {
      return(shiny::p(class = "small text-muted mt-2",
                      t("Update preview to see the ARD this report's analyses make.")))
    }
    if (isTRUE(r$none)) {
      return(shiny::p(class = "small text-muted mt-2", t("This report uses no ARD.")))
    }
    if (!is.null(r$error)) {
      return(shiny::div(class = "alert alert-danger py-1 small mt-2",
                        shiny::tags$pre(class = "mb-0", r$error)))
    }
    a <- r$ard
    tab <- table(factor(a$analysis_id, levels = unique(a$analysis_id)))
    cn <- .ard_conditions_of(a)
    shiny::div(
      class = "small mt-2",
      shiny::p(sprintf(t("%s: %d rows in %.1f s"), r$scope, nrow(a), r$seconds),
               " ", paste(sprintf("%s %d", names(tab), as.integer(tab)),
                          collapse = ", ")),
      # what went wrong inside its analyses, this report's alone
      if (nrow(cn)) shiny::div(
        class = "alert alert-warning py-1 small",
        shiny::strong(t("Errors and warnings inside the analyses")),
        lapply(seq_len(nrow(cn)), function(i) shiny::div(
          shiny::span(class = if (cn$level[i] == "error") "text-danger" else "text-warning",
                      if (cn$level[i] == "error") t("error") else t("warning")),
          sprintf(" %s / %s: %s", cn$analysis_id[i] %||% "", cn$variable[i] %||% "", cn$message[i]),
          if (cn$level[i] == "error") shiny::span(
            class = "text-muted", " ", t("Its statistics are not in the ARD (blank in the table).")))))
    )
  })
  output$ard_table <- DT::renderDT({
    r <- ard_res()
    shiny::req(r, r$ard)
    # a height of its own, so the horizontal scroll bar stays in sight
    DT::datatable(ard_view(r$ard), rownames = FALSE, filter = "top",
                  selection = "none", class = "compact stripe nowrap", fillContainer = FALSE,
                  options = list(pageLength = 50, scrollX = TRUE,
                                 scrollY = "55vh", scrollCollapse = TRUE,
                                 dom = "tip"))
  })


  # -- the study ARD, output by output --------------------------------------
  ard_state_ver <- shiny::reactiveVal(0L)
  ard_state <- shiny::reactive({
    ard_state_ver()
    rv$status_ver
    # the ARD definitions and what their state is made from: an edit of a
    # table, a title or a listing does not hash every report's again
    p_ard()
    shiny::req(has_study())
    ard_status(shiny::isolate(current_study()))
  })
  # each step's mark for the report chosen (Make a report): its state at a
  # glance -- a step its kind does not use is faded instead
  step_mark <- function(sym, title, cls) {
    shiny::span(class = paste("ms-1", cls), title = title, sym)
  }
  # (steps 2 and 3 have no mark yet: drawn empty, not left waiting)
  output$step_mark_content <- shiny::renderUI(NULL)
  output$step_mark_page <- shiny::renderUI(NULL)
  output$step_mark_codelist <- shiny::renderUI({
    id <- current()
    if (is.null(id) || !has_study()) return(NULL)
    n <- nrow(sheet_rows(rv$p, "codelists", id) %||% data.frame())
    if (n) step_mark("\u25cf", sprintf(t("This report's own code list: %d rows"), n), "text-success") else
      step_mark("\u25cb", t("Optional"), "text-muted")
  })
  output$step_mark_ard <- shiny::renderUI({
    id <- current()
    if (is.null(id) || !has_study()) return(NULL)
    if (identical(report_kind(), "figure")) {
      n <- length(fig_design(rv$p, id)$data)
      return(if (n) step_mark("\u25cf", sprintf(t("Data steps: %d"), n), "text-success") else
        step_mark("\u25cb", t("No data step yet: start the figure in step 2"), "text-muted"))
    }
    if ("ard" %in% (.type_idle_tabs[[report_kind()]] %||% character())) {
      return(step_mark("\u2013", t("Not used"), "text-muted"))
    }
    st <- tryCatch(ard_state(), error = function(e) NULL)
    s0 <- if (!is.null(st)) st$state[match(id, st$output_id)] else NA
    switch(if (is.na(s0)) "none" else s0,
      built = step_mark("\u25cf", t("ARD: made"), "text-success"),
      outdated = step_mark("\u27f3", t("ARD: outdated (make it again)"), "text-warning"),
      error = step_mark("!", t("ARD: stopped with an error"), "text-danger"),
      step_mark("\u25cb", t("ARD: not made yet"), "text-muted"))
  })
  ard_state_view <- function() {
    d <- ard_state()
    src <- vapply(d$output_id, function(id) {
      f <- .ard_import_of(rv$p, id)
      if (is.null(f)) t("its ARD definition") else sprintf(t("taken in: %s"), f)
    }, "")
    v <- data.frame(a = d$output_id, b = d$analyses,
                    c = t(unname(.ard_state_labels[d$state])),
                    d = ifelse(is.na(d$rows), "", d$rows),
                    e = ifelse(is.na(d$built), "", d$built), f = d$error,
                    g = src, stringsAsFactors = FALSE)
    names(v) <- t(c("output_id", "Analyses", "State", "Rows", "Built",
                    "Error", "Source"))
    DT::formatStyle(
      .dt(v, selection = "single"), names(v)[3L],
      color = DT::styleEqual(t(unname(.ard_state_labels)),
                             c("#15803d", "#b45309", "#6b7280", "#b91c1c")))
  }
  output$ard_state <- DT::renderDT(ard_state_view())
  # the study ARD's errors and warnings, read again when the file changes
  ard_conds <- shiny::reactive({
    rv$status_ver
    ard_state_ver()
    shiny::req(has_study())
    s <- current_study()
    out <- .ard_study_value(s$planner$ard, "output", "output/ard/ard.rds")
    f <- file.path(s$path, out)
    k <- paste(f, if (file.exists(f)) file.mtime(f))
    if (!identical(ard_cols[["conds_key"]], k)) {
      ard_cols[["conds"]] <- study_ard_conditions(s)
      ard_cols[["conds_key"]] <- k
    }
    ard_cols[["conds"]]
  })
  ard_conds_shown <- shiny::reactive({
    d <- ard_conds()
    if (isTRUE(input$ard_cond_errors)) d[d$level %in% "error", , drop = FALSE] else d
  })
  output$ard_cond_badge <- shiny::renderUI({
    d <- ard_conds()
    if (!nrow(d)) return(NULL)
    ne <- sum(d$level %in% "error")
    nw <- sum(d$level %in% "warning")
    shiny::span(class = paste("badge ms-1", if (ne) "text-bg-danger" else "text-bg-warning"),
                sprintf(t("errors %d, warnings %d"), ne, nw))
  })
  # an analysis's errors and warnings as a table: an error's statistics
  # are not in the ARD (the table's cells are blank); a report whose ARD is
  # outdated, faint
  conds_table <- function(d, outdated = character()) {
    lv <- ifelse(d$level %in% "error", t("error"), t("warning"))
    eff <- ifelse(d$level %in% "error",
                  t("Its statistics are not in the ARD (blank in the table)."), "")
    src <- ifelse(is.na(d$source) | !nzchar(d$source), t("study ARD"),
                  sprintf(t("taken in: %s"), d$source))
    old <- ifelse(d$output_id %in% outdated, t("outdated"), "")
    v <- data.frame(d$output_id, d$analysis_id, d$variable, d$groups, lv, d$message,
                    eff, src, old, stringsAsFactors = FALSE)
    names(v) <- t(c("output_id", "Analysis ID", "Variable", "Groups", "Kind", "Message",
                    "In the table", "From", "ARD"))
    x <- .dt(v, selection = "single",
             language = list(emptyTable = t("Nothing went wrong (or the study ARD is not made yet).")))
    x <- DT::formatStyle(x, names(v)[5L],
                         color = DT::styleEqual(c(t("error"), t("warning")), c("#b91c1c", "#b45309")))
    DT::formatStyle(x, names(v)[9L], target = "row",
                    opacity = DT::styleEqual(t("outdated"), 0.5))
  }
  output$ard_conds <- DT::renderDT({
    d <- ard_conds_shown()
    st <- tryCatch(ard_state(), error = function(e) NULL)
    conds_table(d, if (!is.null(st)) st$output_id[st$state %in% "outdated"] else character())
  })
  .srv_keep(environment())
}
