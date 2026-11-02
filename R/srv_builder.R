# app_server(): builder -- moved from R/app.R as it was.  Run in
# app_server()'s environment (.srv_part()); what it makes is copied back
# there for the parts after it (.srv_keep()).
.srv_builder <- function() {

  # -- the table builder -------------------------------------------------
  # The form is drawn once per report (and whenever the builder tab is
  # opened), with input ids fresh each time: an input of the form before
  # can never be read as one of this form's.  Its edits write the sheets
  # (builder_write()), and the grids redraw when their tab is opened.
  bform <- new.env()
  # numbered from a different start in each session: a browser that
  # reconnects to a new session (the app restarted) still holds the inputs
  # of the session before -- b3_total_on = TRUE, drawn for another report
  # -- and sends them; with the same numbers they would be read as this
  # session's form (a Total column switched on for a report without one)
  bform$n <- sample.int(1e6L, 1L) * 10L
  session$userData$bform <- bform
  bform_drawn <- shiny::reactiveVal(0L)
  # the reports added since the study was last saved (their usual cards
  # are shown, empty: .builder_items_cards(fresh = ))
  bnew <- new.env()
  bnew$ids <- character()
  # a variable's own decimals, as the form edits them (bform$exc)
  exc_ver <- shiny::reactiveVal(0L)
  rv$bver <- 0L
  rv$btouched <- FALSE
  # the page shown: a Tables sub-tab counts as its own page
  active_page_now <- shiny::reactive({
    nav <- page()
    if (identical(nav, "tables")) "builder" else nav
  })
  # Passed on only when the page changes: page() reads the report's kind
  # from rv$p, which every edit writes -- as a reactive, each edit counted
  # as opening the builder again and redrew its form mid-typing.
  active_page <- shiny::reactiveVal("")
  shiny::observe(active_page(active_page_now()), priority = 100)
  shiny::observeEvent(list(rv$btouched, input$table_right), {
    if (identical(active_page(), "builder") && identical(input$table_right, "spec") &&
        isTRUE(rv$btouched)) {
      rv$btouched <- FALSE
      bump()
    }
  })
  shiny::observeEvent(active_page(), {
    if (identical(active_page(), "builder")) rv$bver <- rv$bver + 1L
    if (active_page() %in% c("table_spec", "report_spec") && rv$btouched) {
      rv$btouched <- FALSE
      bump()
    }
  })
  builder_case_now <- shiny::reactive({
    id <- current()
    if (is.null(id)) return("none")
    if (!identical(report_info(rv$p, id)$type, "table")) return("type")
    m <- meta_of(id)
    if (is.null(m)) return("meta")
    if (length(m$hierarchy)) return("hierarchy")
    "ok"
  })
  # Passed on only when it changes: the case reads rv$p, which every edit of
  # the form writes, and the form is drawn from the case -- a reactive would
  # redraw the form (closing it, losing the text being typed) at each edit.
  builder_case <- shiny::reactiveVal("none")
  shiny::observe(builder_case(builder_case_now()), priority = 100)
  # A table whose rows are in the study ARD (made by an official run, the
  # sample's for one) but not read for the builder yet: read them when its
  # builder is opened -- no new ARD, only the reading (once a report).
  auto_read <- new.env()
  shiny::observe({
    # (again when a run ends, or the study ARD's status changes in one: the
    # ARD it made is read then)
    rv$status_ver
    ard_state_ver()
    shiny::req(identical(active_page(), "builder"),
               identical(builder_case(), "meta"))
    id <- current()
    if (isTRUE(auto_read[[id]])) return()
    st <- tryCatch(ard_status(shiny::isolate(current_study())), error = function(e) NULL)
    if (is.null(st) || !identical(st$state[match(id, st$output_id)], "built")) return()
    auto_read[[id]] <- TRUE
    m <- NULL
    shiny::withProgress(message = sprintf(t("Reading the ARD of %s for the builder"), id),
                        m <- guarded(fetch_ard(shiny::isolate(current_study()), id)))
    if (!is.null(m)) rv$ard_ver <- shiny::isolate(rv$ard_ver) + 1L
  })
  output$builder_note <- shiny::renderUI({
    # (a template report: its card and the right pane say it -- #440)
    id <- current()
    if (!is.null(id) && !is.null(.tpl_of(rv$p, id))) {
      return(if (identical(builder_case(), "meta")) {
        items <- tryCatch(builder_items(rv$p, id), error = function(e) list())
        made <- vapply(items, function(it) it$fn, "")
        cards <- tryCatch(.builder_items_cards(items, template = TRUE,
                                               added = bform$added_by[[id]] %||% character(),
                                               code_fns = .tpl_code_fns(rv$p, id)),
                          error = function(e) list())
        shiny::tagList(
          shiny::p(class = "small text-muted", t("No ARD yet: Update preview makes it.")),
          .plan_add_menu_ui("b_add_pick", .plan_add_menu(cards, made = made, template = TRUE),
                            lang = lang))
      })
    }
    msg <- switch(builder_case(),
      none = t("Choose a report above, or in the list on the left (> opens it)."),
      type = t("The builder is for Tables; Listings and Figures are made in their data code."),
      meta = shiny::tagList(
        t("This table has no ARD yet: the builder is built from it."), " ",
        .btn("fetch3", t("Update preview"),
             class = "btn-sm btn-primary ms-1"), " ",
        help_tip(t(.preview_tip))),
      NULL)
    # (plan_cells()' variables of their own, read only, until the ARD is
    # there to edit them against -- #454)
    if (identical(builder_case(), "meta")) {
      pc <- tryCatch(.builder_args(builder_items(rv$p, id), "plan_cells"), error = function(e) NULL)
      if (length(.cells_var_names(pc))) msg <- shiny::tagList(msg, .cells_vars_ro(pc, lang))
    }
    if (is.null(msg)) return(pane_head(
      t("Table builder"),
      t("Drag to order, type to rename; every change is written to this report's SPEC (YAML) and shown in the code and the preview on the right.")))
    shiny::div(class = "alert alert-info py-2 small", msg)
  })
  bid <- function(x) paste0("b", bform$n, "_", x)
  output$builder_form <- shiny::renderUI({
    rv$bver
    rv$ard_ver
    shiny::req(builder_case() %in% c("ok", "hierarchy"))
    on.exit(.tpl_trace("builder cards drawn"), add = TRUE)
    id <- current()
    m <- meta_of(id)
    st <- builder_read(shiny::isolate(rv$p), id, m)
    # a table with no row of its own yet: the ARD's cards are put on once
    # (a table with no table_plan() of its own yet: a new one)
    bform$fresh <- {
      o <- tryCatch(.spec_output(shiny::isolate(rv$p), id), error = function(e) NULL)
      tp <- if (!is.null(o)) Filter(function(it) identical(it$fn, "table_plan"), o$content %||% list())
      if (!is.null(o)) !length(tp) || !length(tp[[1L]]$args) else
        !nrow(sheet_rows(shiny::isolate(rv$p), "tables", id))
    }
    bform$n <- bform$n + 1L
    bform_drawn(bform$n)
    # (conditions not said yet are the form's: another report, gone)
    if (!identical(bform$id, id)) bform$cv_edit <- list()
    bform$id <- id
    bform$st <- st
    bform$exc <- st$exceptions
    bform$own_tpl <- list()
    # what the sheets say, as the form last wrote or read it: an edit writes
    # only what differs from it
    bform$last <- st
    bform$meta <- m
    # the column header as lines of the form (NULL: the form cannot show
    # it), and whose {n} it prints: plan_col_header()'s header and values
    p0 <- shiny::isolate(rv$p)
    hd <- .builder_args(guarded(builder_items(p0, id, m)) %||% list(), "plan_col_header")
    bform$hdr <- hdr_uid(header_read(.header_df(hd$header), st$key))
    bform$hdr_n <- .header_n_text(hd$values)
    hdr_ver(shiny::isolate(hdr_ver()) + 1L)
    v <- st$variables
    bs <- builder_stats()
    keys <- unique(c(m$by, names(m$keys)))
    under_of <- function(i) {
      u <- v$under[i] %||% NA_character_
      if (is.na(u) || !nzchar(u)) NULL else u
    }
    var_items <- stats::setNames(lapply(seq_len(nrow(v)), function(i)
      shiny::span(class = "rp-b-var", shiny::strong(v$variable[i]),
                  shiny::span(class = "rp-b-kind", v$kind[i]),
                  if (!is.null(under_of(i))) shiny::span(
                    class = "rp-b-kind", paste0("\u2192 ", t("under"), " ", under_of(i))))),
      v$variable)
    hier <- length(st$hierarchy) > 0L
    al <- function(fn, arg) builder_label(.arg_meaning(fn, arg), arg, "arg", lang)
    # the cards, one an item of the table's content, in its order, the ARD's
    # normalization first (its hierarchy, step 1's, and the any-event row);
    # the required and the usual ones of the table's kind shown though the
    # content has none (off), and those added here waiting for a value
    items <- guarded(builder_items(p0, id, m)) %||% .builder_items_ard_first(list())
    # the variables not shown (plan_filter()'s condition the builder writes;
    # NULL: the condition is the user's own)
    hidden <- .hidden_vars_read(.filter_where_text(.builder_args(items, "plan_filter")$where) %||%
                                  NA_character_)
    bform$hidden <- hidden
    # the variables' rows as the items say them (headings, levels, nesting)
    vr_items <- .builder_vars_rows(items)
    # (a verb added or switched off here stays a card: kept in added_by
    # though the content has it now, so off -- its rows taken away -- it
    # is still shown; the menu's Remove drops it)
    added <- bform$added_by[[id]] %||% character()
    bform$total_kind <- ard_total_of(id, m)
    # (a template report: only the cards it overrides or adds; the cards
    # its pipe has are offered in "+ Add a card" first -- #440)
    tpl <- !is.null(.tpl_of(p0, id))
    bform$tpl <- tpl
    bform$made <- vapply(items, function(it) it$fn, "")
    items <- .builder_items_cards(items, hier, added, ard_total = bform$total_kind,
                                  off = bform$off_by[[id]] %||% character(),
                                  fresh = id %in% bnew$ids, template = tpl,
                                  code_fns = if (tpl) .tpl_code_fns(p0, id) else character())
    bform$items <- items
    new_card <- bform$just_added
    bform$just_added <- NULL
    has <- function(fn) any(vapply(items, function(it) identical(it$fn, fn), NA))
    cont <- any(st$variables$kind %in% "continuous")
    # the decimals: the plan_digits() card's when there is one
    digits_ui <- if (cont) shiny::conditionalPanel(
      sprintf("input['%s'] == 'stat'", bid("value")),
      shiny::uiOutput(bid("digits_ui")),
      shiny::uiOutput(bid("exc_ui")))
    # the row layers: a hierarchy's levels are its analysis's (step 1) --
    # fixed, never removed; its last level is the rows' text (label)
    lay_now <- if (hier) st$row_keys else st$layers
    lay_ch <- unique(c(lay_now, if (hier) st$row_choices else st$layer_choices))
    # (a summary table's block of variables is a layer like the keys: put
    # where it goes, or taken out -- a table of one variable)
    fixed <- if (hier) st$hierarchy else character()
    stack_keys <- .stack_keys(.rows_for(p0, "tables", id)$stack[1L],
                              .rows_for(p0, "tables", id)$stack_nest[1L])
    layer_names <- .layer_names(lay_ch, if (hier) st$hierarchy, lang, stack = stack_keys)
    # the rows' text (label): normalize_ard()'s .label, a key, or none
    lab_keys <- setdiff(lay_ch, c(".variable", lay_ch[.is_heading(lay_ch)]))
    row_label_ui <- if (hier) {
      shiny::div(class = "rp-b-sec small",
                 al("table_plan", "label"), " ",
                 shiny::tags$code(paste("label =", utils::tail(st$row_keys, 1L))),
                 shiny::span(class = "text-muted ms-1", t("(the last level)")))
    } else if (identical(st$row_label, "own")) {
      shiny::div(class = "rp-b-sec small text-muted",
                 al("table_plan", "label"), ": ",
                 t("the sheets' own (tables$label); edit it as R."))
    } else {
      cur <- if (is.na(st$row_label %||% ".label")) "NA" else st$row_label %||% ".label"
      shiny::div(
        class = "rp-b-sec",
        shiny::selectInput(
          bid("rowlabel"),
          with_tip(al("table_plan", "label"),
                   t("What the label column prints on each row: normalize_ard()'s row text (.label), the values of a key (that key is then no row layer), or nothing (NA).")),
          choices = c(stats::setNames(".label", t("The ARD's row text (.label)")),
                      stats::setNames(unique(c(lab_keys, setdiff(cur, c(".label", "NA")))),
                                      unique(c(lab_keys, setdiff(cur, c(".label", "NA"))))),
                      stats::setNames("NA", t("None (NA)"))),
          selected = cur, width = "22em"))
    }
    # table_plan(): the columns (cols) and the rows as layers (rows, label)
    tp_body <- shiny::tagList(
      shiny::div(
        class = "rp-b-sec",
        # one or more (a group, then a visit ...): outermost first, as the
        # rows -- dragged to order, each removed by its x (or Backspace)
        shiny::selectizeInput(bid("key"),
                              with_tip(al("table_plan", "cols"),
                                       t("The column keys, outermost first; drag to order. Their values are the table's columns (TRT01A: a column an arm). A key is in cols or in rows, not both.")),
                              .col_key_names(keys, lang),
                              selected = if (!anyNA(st$key)) st$key,
                              multiple = TRUE, width = "100%",
                              options = list(plugins = list("remove_button", "drag_drop")))),
      shiny::div(
        class = "rp-b-sec",
        # the table's rows: the ARD's keys (its groups, its hierarchy) and
        # the block of the analysis variables, outermost first -- the
        # layout is the table's; the ARD only offers them
        shiny::selectizeInput(
          bid("layers"),
          with_tip(al("table_plan", "rows"),
                   paste(if (hier) t("Outermost first; drag to order. The keys are the analysis's by (step 1).") else
                     t("Outermost first; drag to order. The keys are the analysis's by (step 1) and the stack's (normalize_ard()); \"...\" typed is a constant heading."),
                     t("What can be placed: the normalized ARD's columns (the keys, variable, a stack's subgroup). group1 is not there: normalizing makes it a column named after its group (TRT01A ...); the analysis variables AGE/SEX are values of variable, so they come as the block of variable. A column is in cols or in rows, not both."))),
          stats::setNames(lay_ch, layer_names), selected = lay_now,
          multiple = TRUE, width = "100%",
          options = c(list(plugins = list("remove_button", "drag_drop"),
                           onDelete = .fixed_items_js(fixed)),
                      if (!hier) list(create = I(.heading_create_js),
                                      createFilter = "^[\"']"))),
        .fixed_items_css(bid("layers"), fixed),
        # (the block taken out: said, in a line)
        if (!hier) shiny::conditionalPanel(
          sprintf("!(input['%1$s'] || []).includes('.variable')", bid("layers")),
          shiny::div(class = "small text-muted rp-b-fixed-note",
                     t("Analysis variables (variable) are no rows (a table of one variable: ORR ...)."))),
        row_label_ui,
        # (what it means, behind the (i): the way to step 1 alone shown)
        if (hier) shiny::div(
          class = "small text-muted rp-b-fixed-note",
          shiny::actionLink("b_rows_step1", t("Step 1's analysis")), " ",
          help_tip(paste(sprintf(t("The levels %s are the ARD analysis's (step 1)."),
                                 paste(st$hierarchy, collapse = " \u203a ")),
                         t("A table never drops a level of its hierarchy: a table of other levels is made from another ARD analysis. Rows the table should not show can be hidden in a later stage."))))),
      # a variable the ARD lacks is not offered: the ARD analysis adds it
      if (!hier) shiny::div(
        class = "small text-muted rp-b-fixed-note",
        shiny::actionLink("b_rows_step1", t("Step 1's analysis")), " ",
        help_tip(t("A variable not here is added in the ARD analysis."))))
    # (a summary table's variables each shown or not: plan_filter()'s card,
    # written as its row filter; the ARD stays as it is)
    shown_ui <- if (nrow(v) >= 2L && !is.null(hidden)) shiny::checkboxGroupInput(
      bid("shown"),
      with_tip(t("Shown"),
               t("Unticked: the table does not show the variable's rows (written as the row filter, plan_filter()); the ARD stays as it is.")),
      choices = v$variable, selected = setdiff(v$variable, hidden), inline = TRUE)
    # plan_cells(): what the cells print
    cells_body <- shiny::tagList(
      shiny::radioButtons(
        bid("value"),
        with_tip(al("plan_cells", "value"), t("Numbers: the ARD's values (stat), rounded here to the decimals below. Formatted text: the values as step 1 formatted them (stat_fmt), printed as they are, not rounded here; a row may still put two in one (Mean (SD)).")),
        stats::setNames(c("stat", "stat_fmt"),
                        t(c("Numbers (stat)", "Formatted text (stat_fmt)"))),
        selected = st$value, inline = TRUE),
      # the rows of a continuous variable, their decimals: only when the
      # table has one (left out, what the definition says is kept)
      if (cont) builder_rows_ui(bs, st, m$stats),
      if (!has("plan_digits")) digits_ui,
      shiny::radioButtons(
        bid("cat"),
        with_tip(al("plan_cells", "categorical"),
                 t("How a level of a categorical variable prints: its count and percentage in one of these formats (n (%) ...), or a template of one's own (Own), e.g. {n} ({p:.1f}%). Every categorical variable takes it.")),
        c(stats::setNames(.cat_formats()$key, .cat_formats()$label),
          stats::setNames("own", t("Own"))),
        selected = st$cat_format, inline = TRUE),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'own'", bid("cat")),
        shiny::textInput(bid("cat_own"), t("Template"),
                         # one's own starts from the format chosen now
                         if (is.na(st$cat_own %||% NA))
                           .cat_template(st$cat_format, st$pct_decimals,
                                         st$value %||% "stat") else st$cat_own,
                         placeholder = "{n} ({p:.1f%})", width = "100%"),
        shiny::p(class = "small text-muted",
                 t("{n} the count, {N} the denominator, {p} the proportion; a format after a colon: {p:.1f%} prints 16.3%, {p:.2f} 0.16. The table on the right shows it."))),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'stat' && input['%s'] != 'own'", bid("value"), bid("cat")),
        shiny::numericInput(bid("pct"), t("Decimals of the percent"),
                            value = st$pct_decimals, min = 0, max = 3,
                            width = "260px")),
      shiny::uiOutput(bid("warn")),
      # each variable: the common template or its own, its conditions
      # (a KM table's prob / time) -- #454
      shiny::uiOutput("b_cv_ui"))
    # plan_labels(): each variable's heading, levels and nesting (a summary
    # table; the values-and-labels card of the next step takes them over)
    vr_own <- vr_items
    label_from_ard <- function(var) {
      l <- vr_own$label[match(var, vr_own$variable)]
      length(l) != 1L || is.na(l) || !nzchar(l)
    }
    labels_body <- if (nrow(v) > 0L) shiny::tagList(
      # (a template report has no step 1-1: #440)
      if (!tpl) .btn("cl3_open", t("Code lists: in step 1 (1-1, column definitions)"),
                     class = "btn-sm btn-link py-0 px-0 mb-1"),
      # the headings and the order as one grid (the code list of the
      # variables), once: here
      shiny::uiOutput("b_vcl"),
      bslib::accordion(
        open = FALSE,
        lapply(seq_len(nrow(v)), function(i) {
          # blank: the code list's label of the variable (faint), what the
          # table prints unless one is written here
          hint <- v$hint[i] %||% NA_character_
          bslib::accordion_panel(
            # (where a nested one's rows go: plan_nest()'s card says it)
            title = paste0(v$variable[i],
                           if (!is.na(v$label[i])) paste0(": ", v$label[i]) else
                             if (!is.na(hint)) paste0(": ", hint)),
            value = v$variable[i],
            # nested under another's level: no heading of its own, so no
            # label to write (the field would mislead)
            if (!is.null(under_of(i))) shiny::p(
              class = "small text-muted mb-1",
              sprintf(t("No heading of its own: its rows go under %s."), under_of(i)))
            else shiny::textInput(bid(paste0("lab", i)),
                             shiny::tagList(
                               if (is.na(hint)) t("Label") else
                                 with_tip(t("Label"), t("Blank: the label of this report's code list of variable (faint). What is written here is the table's own.")),
                               # a label the sheets do not say: the ARD's
                               if (!is.na(v$label[i]) && label_from_ard(v$variable[i]))
                                 from_badge("ard", lang)),
                             value = if (is.na(v$label[i])) "" else
                               v$label[i], width = "100%",
                             placeholder = if (!is.na(hint)) hint),
            # what the table prints for it (the Statistics card's), faint
            # (its rows under a level of another variable: plan_nest()'s
            # card)
            shiny::uiOutput(bid(paste0("vdef", i))))
        })))
    # plan_col_header(): the header's lines
    hdr_body <- shiny::tagList(
      shiny::uiOutput(bid("hdr_ui")),
      if (!anyNA(st$key) &&
          !.has_group_n(p0, id, st$key[1L]))
        shiny::div(
          class = "small text-warning d-flex flex-wrap gap-2 align-items-center",
          shiny::span(sprintf(t("The ARD has no subjects per %s: a header's (N=n) prints NA."),
                              st$key[1L])),
          .btn("b_add_n", t("Count them (then Preview)"),
               class = "btn-sm btn-outline-primary py-0")))
    item_of <- function(fn) {
      at <- Filter(function(it) identical(it$fn, fn), items)
      if (length(at)) at[[1L]]
    }
    bodies <- list(
      table_plan = tp_body, plan_cells = cells_body, plan_col_header = hdr_body,
      plan_total = shiny::uiOutput(bid("total_ui")),
      # plan_digits(): the rounding (the study's: every report's) and the
      # statistics' decimals
      plan_digits = if (has("plan_digits")) shiny::tagList(
        .rounding_field(card_id("plan_digits"), shiny::isolate(rv$p$study[["rounding"]]), lang),
        if (cont) digits_ui),
      plan_labels = labels_body,
      # plan_levels(): each variable's values in order (dragged), its empty
      # values hidden or not
      # (the block of variables in order, the `variable` entry, and the
      # column variables' levels: the columns' order)
      plan_levels = if (has("plan_levels")) .levels_body(
        bid, st, sheet_rows(p0, "codelists", id),
        empty = vr_items, lang = lang,
        vars = if (!hier) var_items, cols = shiny::uiOutput(bid("arms_ui"))),
      plan_sort = if (hier && isTRUE(item_of("plan_sort")$present))
        hier_sort_ui(st, sheet_rows(p0, "codelists", id)$variable,
                     total = .rows_for(p0, "tables", id)$total[1L], stats = m$stats))
    # (a form of its own: the function's other arguments below it)
    for (fn in names(.special_covered)) {
      it <- item_of(fn)
      if (is.null(bodies[[fn]]) || is.null(it) || !(isTRUE(it$present) || isTRUE(it$added))) next
      bodies[[fn]] <- shiny::tagList(bodies[[fn]], .special_more(card_id(it$path), fn, it, lang))
    }
    # the layout cards with a form: their arguments as fields (a number, a
    # choice ...), the columns offered the table's keys and row layers
    lay_keys <- setdiff(st$layers %||% character(), ".variable")
    cols_offered <- unique(c(keys, st$row_keys, lay_keys[!.is_heading(lay_keys)]))
    # (plan_columns(): the columns printed -- the row label column, every
    # value column, each by name)
    stub_name <- item_of("plan_stub")$args$name %||% "row_label"
    print_cols <- unique(c(stub_name, ".values",
                           if (length(st$key) == 1L && !anyNA(st$key)) st$arms[[st$key]]))
    bform$form_cols <- list()
    # (each call its own card: plan_cell_style()'s second is
    # "plan_cell_style#2", its form read and written as the second)
    form_items <- Filter(function(it) it$fn %in% .plan_form_fns &&
                           (isTRUE(it$present) || isTRUE(it$added)), items)
    for (it in form_items) {
      fn <- it$fn
      # (plan_paginate_cols(), plan_hide(), plan_cell_style(): the printed
      # columns and the keys; plan_row_group(collapse = TRUE): every row key)
      ch <- switch(fn, plan_columns = print_cols,
                   plan_paginate_cols = , plan_hide = , plan_cell_style =
                     unique(c(print_cols, cols_offered)),
                   plan_row_group = c(stats::setNames("TRUE", t("Every row key (TRUE)")),
                                      stats::setNames(cols_offered, cols_offered)),
                   cols_offered)
      wc <- if (identical(fn, "plan_columns")) .widths_cols(it$args$widths, ch)
      bform$form_cols[[it$path]] <- wc
      # (normalize_ard(): its hierarchy the ARD analysis's, read only, the
      # way to step 1 beside it)
      body <- .plan_form_body(
        card_id(it$path), fn, it, choices = ch, lang = lang, cols = wc,
        ard = if (identical(fn, "normalize_ard")) list(hierarchy = st$hierarchy),
        fixed_link = if (identical(fn, "normalize_ard"))
          shiny::actionLink("b_norm_step1", t("Step 1's analysis"), class = "small ms-1"))
      # plan_filter(): its condition by the condition builder (the rows
      # kept: tables$keep_where), its switches as fields
      if (identical(fn, "plan_filter")) {
        bform$filter_where <- .filter_where_text(it$args$where)
        body <- shiny::tagList(
          shiny::div(class = "rp-field", `data-arg` = "where",
                     shiny::tags$label(class = "rp-field-label",
                                       with_tip(al("plan_filter", "where"),
                                                t("The rows kept: a condition over a row's statistics (n, p ...) and its keys, as a cells row's when. A SOC stays when one of its PTs does; the overall row always stays."))),
                     condition_builder_ui("b_filter_cond", lang)),
          body)
        if (!is.null(shown_ui)) body <- shiny::tagList(shown_ui, body)
      }
      bodies[[it$path]] <- body
    }
    # the cards of the names the user gives (a function's ...): the
    # comparison columns, the groups of statistics, the rows nested
    bform$dots_drawn <- list()
    for (fn in .plan_form_dots) {
      it <- item_of(fn)
      if (is.null(it) || !(isTRUE(it$present) || isTRUE(it$added))) next
      cid <- card_id(it$path)
      bodies[[fn]] <- switch(
        fn,
        plan_compare = {
          bform$dots_drawn[[fn]] <- .dots_args(fn, it$args %||% list())
          .compare_body(cid, it, comparisons_of(id, m), by_choices = m$by %||% keys, lang = lang)
        },
        plan_stat_groups = {
          bform$dots_drawn[[fn]] <- length(.dots_args(fn, it$args %||% list()))
          .stat_groups_body(cid, it, stats = m$stats,
                            variables = if (is.data.frame(m$variables)) m$variables$variable,
                            lang = lang)
        },
        plan_nest = {
          bform$dots_drawn[[fn]] <- .nest_vars(st)
          .nest_body(cid, it, st, lang = lang)
        })
    }
    # a part (plan_append()): its own fields, its items as YAML
    # (R/builder_parts.R)
    for (it in Filter(function(it) identical(it$fn, "plan_append") &&
                        (isTRUE(it$present) || isTRUE(it$added)), items)) {
      bodies[[it$path]] <- .part_body(card_id(it$path), it,
                                      datasets = rv$p$ard$datasets$dataset %||% character(), lang = lang)
    }
    bodies <- Filter(Negate(is.null), bodies)
    shiny::div(
      class = "rp-builder-cards", `data-back` = t("Back to the code"),
      .table_cards(items, bodies, id = card_id, lang = lang, new = new_card,
                   level_from = st$level_from),
      # the other verbs: "+ Add a card", in the pipe's order, below the last
      .plan_add_menu_ui("b_add_pick", .plan_add_menu(items, made = if (tpl) bform$made, template = tpl),
                        lang = lang))
  })
  # the kind of Total the report's ARD has (.ard_total_kind()); an older
  # saved meta without `overall`: read from the ARD itself
  ard_total_of <- function(id, m) {
    if (is.null(m)) return(NA_character_)
    if (is.null(m$overall)) {
      d <- tryCatch(ard_data(shiny::isolate(rv$study), id), error = function(e) NULL)
      if (!is.null(d)) m$overall <- .ard_has_overall(d, m$by)
    }
    .ard_total_kind(m)
  }
  # the comparison records of the report's ARD (ard_meta()'s
  # `comparisons`); an older saved meta without them: read from the ARD
  comparisons_of <- function(id, m) {
    if (is.null(m)) return(list())
    if (is.null(m$comparisons)) {
      d <- tryCatch(ard_data(shiny::isolate(rv$study), id), error = function(e) NULL)
      return(if (is.null(d)) list() else .ard_comparisons(d, m$by))
    }
    m$comparisons
  }
  # a card's input id: by its path (the verb; "#2" its second call)
  card_id <- function(path) bid(paste0("card_", gsub("[^A-Za-z0-9_]", "_", path)))
  # after a write: each card whose item now comes from elsewhere (a card
  # of the study's defaults edited is this table's own; Revert) gets its
  # mark and Revert in its head, the form not drawn again
  send_mark <- function(path, f) {
    session$sendCustomMessage("rp-card-mark", list(
      card = card_id(path), html = as.character(.card_mark(f, card_id(path), lang)),
      summary = .item_summary(f, lang = lang)))
  }
  refresh_marks <- function(p2) {
    fresh <- tryCatch(builder_items(p2, bform$id, bform$meta), error = function(e) list())
    for (i in seq_along(bform$items)) {
      it <- bform$items[[i]]
      f <- .builder_item_at(fresh, it$path)
      if (is.null(f) || !isTRUE(it$present) || !isTRUE(f$present)) next
      if (identical(f$from, it$from) && identical(isTRUE(f$revert), isTRUE(it$revert)) &&
          identical(f$args, it$args)) next
      bform$items[[i]][c("args", "own_args", "from", "arg_from", "below", "revert")] <-
        f[c("args", "own_args", "from", "arg_from", "below", "revert")]
      send_mark(it$path, f)
    }
  }
  # a part's card: Apply writes its fields; Start writes the usual part
  # (the ARD's summary statistics by the table's columns) into it
  shiny::observeEvent(input$b_part_act, {
    a <- input$b_part_act
    it <- .builder_item_at(bform$items, a$path)
    shiny::req(!is.null(it), identical(bform$id, current()))
    cid <- card_id(it$path)
    k <- if (grepl("#", it$path, fixed = TRUE)) as.integer(sub("^.*#", "", it$path)) else 1L
    args <- if (identical(a$act, "start")) {
      cols <- bform$st$key[!is.na(bform$st$key)]
      utils::modifyList(it$args %||% list(),
                        list(data = list(source = "ard"), content = .part_quickstart(cols)))
    } else guarded(.part_args_read(input, cid, it))
    if (is.null(args)) return()
    p2 <- guarded(.builder_item_write(rv$p, bform$id, "plan_append", args, k))
    if (is.null(p2)) return()
    rv$p <- p2
    rv$btouched <- TRUE
    rv$bver <- rv$bver + 1L
    notify(t("The part is written: update the preview to see its rows."))
  })
  # the Total column's switch: its card's
  total_on_id <- function() paste0(card_id("plan_total"), "__on")
  # a card's Revert: the report's own rows of its verb taken away, so the
  # study's defaults apply again
  shiny::observeEvent(input$b_card_revert, {
    it <- .builder_item_at(bform$items, input$b_card_revert$path)
    shiny::req(!is.null(it), identical(bform$id, current()))
    k <- if (grepl("#", it$path, fixed = TRUE)) as.integer(sub("^.*#", "", it$path)) else 1L
    p2 <- guarded(.builder_item_revert(rv$p, bform$id, it$fn, k))
    if (is.null(p2)) return()
    rv$p <- p2
    rv$btouched <- TRUE
    rv$bver <- rv$bver + 1L
  })
  # a card's "Edit as R": its arguments as one line of R, read back (and
  # written to the sheets through tflspec's bridge) on Apply
  # the dialog of a card's R: its text, and what tflspec's check found in
  # the text applied (`problems`, lines) -- shown before it is written
  r_dialog <- function(it, text, problems = character()) {
    shiny::modalDialog(
      title = shiny::tagList(t("Edit as R"), " ", shiny::tags$code(paste0(it$fn, "()"))),
      shiny::textAreaInput("b_r_text", NULL, text, rows = 6,
                           width = "100%", resize = "vertical"),
      shiny::p(class = "small text-muted",
               t("The arguments as R: name = value, separated by commas. What is no plain value (a call, a name) stays R.")),
      if (length(problems)) shiny::div(
        class = "alert alert-warning py-1 small rp-r-problems",
        shiny::tags$ul(class = "mb-1", lapply(problems, shiny::tags$li)),
        t("Apply again to write it as it is.")),
      footer = shiny::tagList(
        .btn("b_r_no", t("Cancel"), class = "btn-secondary"),
        .btn("b_r_ok", t("Apply"), class = "btn-primary")),
      size = "l", easyClose = TRUE)
  }
  shiny::observeEvent(input$b_card_r, {
    it <- .builder_item_at(bform$items, input$b_card_r$path)
    shiny::req(!is.null(it), isTRUE(it$present) || isTRUE(it$added))
    bform$r_item <- it
    bform$r_seen <- NULL
    shiny::showModal(r_dialog(it, args_to_r(it$fn, it$args)))
  })
  shiny::observeEvent(input$b_r_no, shiny::removeModal())
  shiny::observeEvent(input$b_r_ok, {
    it <- bform$r_item
    shiny::req(!is.null(it), identical(bform$id, current()))
    txt <- input$b_r_text %||% ""
    args <- tryCatch(args_from_r(it$fn, txt), error = function(e) {
      notify(conditionMessage(e), "error")
      NULL
    })
    shiny::req(!is.null(args))
    # the check's problems are shown in the dialog first (drawn again with
    # the text as typed); Apply again with the same text writes it as it is
    pr <- .args_problems(args)
    if (length(pr) && !identical(bform$r_seen, txt)) {
      bform$r_seen <- txt
      shiny::showModal(r_dialog(it, txt, pr))
      return()
    }
    args <- .args_plain(args)
    shiny::removeModal()
    if (identical(args_to_r(it$fn, args), args_to_r(it$fn, it$args))) return()
    k <- if (grepl("#", it$path, fixed = TRUE)) as.integer(sub("^.*#", "", it$path)) else 1L
    p2 <- guarded(.builder_item_write(rv$p, bform$id, it$fn, args, k))
    if (is.null(p2)) return()
    rv$p <- p2
    rv$btouched <- TRUE
    rv$bver <- rv$bver + 1L
  })
  # the Code tab's lines -> the cards that wrote them (code_jump())
  output$builder_code_jump <- shiny::renderUI({
    id <- current()
    shiny::req(!is.null(id), identical(report_info(rv$p, id)$type, "table"))
    items <- tryCatch(builder_items(rv$p, id, meta_of(id)), error = function(e) list())
    code_jump(builder_code_map(program_text(), items),
              "program_table")
  })
  # A hierarchy's order, one a term (outermost first): by name -- the code
  # list's order when the report has one for it -- or by a statistic of
  # the ARD (n, p, a comparison's estimate ...): largest or smallest
  # first, its sum over the columns or its largest / smallest in any one
  # (an outer term by its own row's), or a key of the user's, written as
  # it is.  The value counted in every column together or in one (the
  # Total, an arm).  The keys plan_sort() gets are shown under it.  An
  # order of the sheets' own is said, with a way back to the card's.
  hier_sort_ui <- function(st, coded, total = NA, stats = character()) {
    n <- length(st$row_keys)
    if (identical(st$sort, "own")) {
      return(shiny::div(
        class = "small",
        t("Order: as the sheets say (tables$sort)."), " ",
        shiny::actionLink("b_sort_reset", t("Use the card's order"))))
    }
    key <- st$key[1L]
    col_now <- attr(st$sort, "col") %||% NA_character_
    cols <- c(stats::setNames("", t("All columns together")),
              if (!is.na(total) && nzchar(total)) stats::setNames(total, total),
              stats::setNames(st$arms[[key]] %||% character(), st$arms[[key]] %||% character()))
    shiny::tagList(
      lapply(seq_len(n), function(i) {
        v <- st$row_keys[i]
        now <- st$sort[i]
        own <- startsWith(now, "=")
        cl <- v %in% coded
        choices <- .sort_key_choices(stats, cl, hier = v %in% st$hierarchy, lang = lang)
        # (a key of the builder's form that is not offered: offered too)
        if (!own && !now %in% choices) choices <- c(choices, stats::setNames(now, now))
        choices <- c(choices, stats::setNames("=", t("Another key (written as it is)")))
        opt <- function(value, label) shiny::tags$option(
          value = value, selected = if (identical(if (own) "=" else now, value)) NA, label)
        sid <- bid(paste0("sort", i))
        # (the values plan_levels() names first: levels(group1), offered
        # on every level -- where it names none, all tie, as if not there)
        g <- if (i < n) sprintf("group%d", i) else "label"
        first <- isTRUE((attr(st$sort, "first") %||% logical(n))[i])
        shiny::div(
          class = "d-flex flex-wrap align-items-center gap-2 mb-1",
          shiny::span(class = "rp-b-var", style = "min-width: 9em;", v),
          shiny::checkboxInput(
            bid(paste0("sort_first", i)),
            sprintf("%s (levels(%s))", t("Values in plan_levels' order first"), g),
            value = first),
          shiny::tags$select(
            id = sid, class = "form-select form-select-sm",
            style = "width: 22em; max-width: 100%;",
            unname(Map(opt, unname(choices), names(choices)))),
          shiny::conditionalPanel(
            sprintf("input['%s'] == '='", sid),
            shiny::textInput(bid(paste0("sort_own", i)), NULL,
                             value = if (own) substring(now, 2L) else "",
                             placeholder = "-max(n)@group1", width = "100%")))
      }),
      shiny::div(
        class = "d-flex align-items-center gap-2 mb-1",
        shiny::span(class = "small", style = "min-width: 9em;", t("Counted in")),
        shiny::selectInput(bid("sort_col"), NULL, cols,
                           selected = if (is.na(col_now)) "" else col_now, width = "22em")),
      shiny::div(class = "small text-muted", "plan_sort(): ",
                 shiny::tags$code(shiny::textOutput("b_sort_code", inline = TRUE))))
  }
  # The layout cards with a form (.plan_form_fns: the blank rows, the
  # pages, the row label column, the style): read back by verb -- none
  # until the card's fields are there -- and written to the sheets through
  # tflspec's bridge as the report's own: what the form changed from the
  # item (a field from the study's defaults left as it is stays theirs)
  form_args <- shiny::reactive({
    bform_drawn()
    shiny::req(builder_case() %in% c("ok", "hierarchy"))
    out <- list()
    # (by the card's path: a verb's second call its own)
    for (it in bform$items) {
      if (!it$fn %in% .plan_form_fns ||
          !(isTRUE(it$present) || isTRUE(it$added))) next
      r <- .plan_form_read(input, card_id(it$path), it$fn, cols = bform$form_cols[[it$path]])
      if (!is.null(r)) out[[it$path]] <- r
    }
    # (a form of its own: the other arguments' fields, the item's whole
    # arguments)
    for (fn in names(.special_covered)) {
      it <- .builder_item_at(bform$items, fn)
      if (is.null(it) || !(isTRUE(it$present) || isTRUE(it$added))) next
      r <- .special_read(input, card_id(it$path), fn)
      if (!is.null(r)) out[[fn]] <- structure(r, special = TRUE)
    }
    # (the cards of the names the user gives: their whole arguments)
    for (fn in .plan_form_dots) {
      it <- .builder_item_at(bform$items, fn)
      if (is.null(it) || !(isTRUE(it$present) || isTRUE(it$added))) next
      cid <- card_id(it$path)
      drawn <- bform$dots_drawn[[fn]]
      r <- switch(fn,
                  plan_compare = .compare_read(input, cid, drawn %||% list()),
                  plan_stat_groups = .stat_groups_read(input, cid, drawn %||% 0L),
                  plan_nest = .nest_read(input, cid, drawn, it))
      if (!is.null(r)) out[[fn]] <- structure(r, dots = TRUE)
    }
    out
  })
  form_args_d <- shiny::debounce(form_args, 250)
  shiny::observeEvent(form_args_d(), builder_guard({
    now <- form_args_d()
    for (path in names(now)) {
      at <- which(vapply(bform$items, function(it) identical(it$path, path), NA))
      if (!length(at)) next
      it <- bform$items[[at[1L]]]
      fn <- it$fn
      # (the verb's k-th call: "plan_cell_style#2" the second)
      k <- if (grepl("#", path, fixed = TRUE)) as.integer(sub("^.*#", "", path)) else 1L
      dots <- isTRUE(attr(now[[path]], "dots"))
      if (isTRUE(attr(now[[path]], "special"))) {
        # (a form of its own writes its item too: the fields below it put
        # into its arguments as they are now, not as drawn)
        cur <- .builder_item_at(tryCatch(builder_items(rv$p, bform$id, bform$meta),
                                         error = function(e) list()), path)
        if (is.null(cur)) next
        it$args <- cur$args
      }
      args <- if (isTRUE(attr(now[[path]], "special"))) .special_args_put(it$args, now[[path]]) else
        if (dots) .dots_form_args(.args_plain_attr(now[[path]]), it, fn) else
          .plan_form_args(now[[path]], it, fn)
      if (identical(args_to_r(fn, args), args_to_r(fn, it$args %||% list()))) next
      p2 <- guarded(.builder_item_write(rv$p, bform$id, fn, args, k))
      if (is.null(p2)) next
      rv$p <- p2
      rv$btouched <- TRUE
      # (what the card says now: the next edit is told from it)
      fresh <- .builder_item_at(tryCatch(builder_items(p2, bform$id, bform$meta), error = function(e) list()), path)
      if (!is.null(fresh)) {
        was_present <- isTRUE(it$present)
        bform$items[[at[1L]]][c("args", "own_args", "from", "arg_from", "below", "revert", "present")] <-
          fresh[c("args", "own_args", "from", "arg_from", "below", "revert", "present")]
        # a card added here that is now in the code, or of the defaults now
        # the report's own: drawn again (its mark, its Revert); a column or a
        # group more or less, a nest changed: drawn again (its fields; the
        # labels' card says where a variable's rows go)
        # (its mark and Revert only: put in its head, not drawn again)
        if (was_present) send_mark(path, fresh)
        if (!was_present ||
            # (normalize_ard()'s stack: the keys it adds offered to the rows)
            (identical(fn, "normalize_ard") &&
               !identical(.arg_to_r(fresh$args$stack), .arg_to_r(it$args$stack))) ||
            (dots && length(.dots_args(fn, fresh$args)) != length(.dots_args(fn, it$args))) ||
            identical(fn, "plan_nest")) rv$bver <- rv$bver + 1L
      }
    }
  }))
  # A card switched on (or added): written with its usual values, or --
  # a verb with none -- opened to be filled (its fields, or R); switched
  # off: the report's own rows of it taken away, the card kept, off (every
  # card stays; the menu's Remove takes one away).  The form is drawn
  # again either way.
  card_switch <- function(it, on) {
    id <- bform$id
    shiny::req(!is.null(id), identical(id, current()))
    added <- unique(c(bform$added_by[[id]] %||% character(), it$fn))
    bform$added_by[[id]] <- added
    off <- bform$off_by[[id]] %||% character()
    if (on) {
      bform$off_by[[id]] <- setdiff(off, it$fn)
      hier <- length(bform$st$hierarchy) > 0L
      args <- .plan_starter_args(it$fn, bform$st, hier, meta = bform$meta)
      if (!is.null(args)) {
        p2 <- guarded(.builder_item_write(rv$p, id, it$fn, args))
        if (is.null(p2)) return()
        rv$p <- p2
        rv$btouched <- TRUE
      }
      bform$just_added <- it$path %||% it$fn
    } else {
      bform$off_by[[id]] <- unique(c(off, it$fn))
      if (isTRUE(it$present)) {
        p2 <- guarded(.builder_item_revert(rv$p, id, it$fn, .path_k(it$path)))
        if (is.null(p2)) return()
        rv$p <- p2
        rv$btouched <- TRUE
      }
    }
    rv$bver <- rv$bver + 1L
  }
  # a card's Remove (its menu, any card not required): its rows taken away
  # when the code has it, the card dropped -- a usual card of the table's
  # kind comes back, off, when the form is drawn again
  # (#440: only the report's own items -- no study standard under them --
  # or a card shown empty; no question asked: "Undo" for 5 s puts the
  # report's YAML back as it was)
  shiny::observeEvent(input$b_card_remove, {
    it <- .builder_item_at(bform$items, input$b_card_remove$path)
    id <- bform$id
    shiny::req(!is.null(it), isTRUE(it$removable), identical(id, current()))
    undo <- list(id = id, fn = it$fn, added = bform$added_by[[id]], off = bform$off_by[[id]],
                 p = NULL)
    bform$added_by[[id]] <- setdiff(bform$added_by[[id]] %||% character(), it$fn)
    bform$off_by[[id]] <- unique(c(bform$off_by[[id]] %||% character(), it$fn))
    if (isTRUE(it$present) && identical(it$from, "own")) {
      undo$p <- rv$p
      p2 <- if (identical(it$fn, "plan_total")) {
        guarded(set_total_column(rv$p, id, NULL, "last"))
      } else guarded(.builder_item_revert(rv$p, id, it$fn, .path_k(it$path)))
      if (is.null(p2)) return()
      rv$p <- p2
      rv$btouched <- TRUE
    }
    bform$undo <- undo
    shiny::showNotification(
      shiny::span(sprintf(t("The card %s was removed."), it$fn), " ",
                  shiny::actionLink("b_card_undo", t("Undo"))),
      duration = 5, id = "b_card_undo_note")
    rv$bver <- rv$bver + 1L
  })
  shiny::observeEvent(input$b_card_undo, {
    u <- bform$undo
    bform$undo <- NULL
    shiny::removeNotification("b_card_undo_note")
    shiny::req(!is.null(u), identical(u$id, current()))
    # (the planner as it was: the report's YAML, its view and the ARD's
    # rows a Total column changed)
    if (!is.null(u$p)) {
      rv$p <- u$p
      rv$btouched <- TRUE
    }
    bform$added_by[[u$id]] <- u$added
    bform$off_by[[u$id]] <- u$off
    bform$just_added <- u$fn
    rv$bver <- rv$bver + 1L
  })
  # the cards' switches (the Total column's is its own, below)
  shiny::observe(builder_guard({
    bform_drawn()
    shiny::req(builder_case() %in% c("ok", "hierarchy"))
    for (it in bform$items) {
      if (!isTRUE(it$switch) || identical(it$fn, "plan_total")) next
      v <- input[[paste0(card_id(it$path), "__on")]]
      if (is.null(v) || identical(isTRUE(v), isTRUE(it$on))) next
      shiny::isolate(card_switch(it, isTRUE(v)))
      return(invisible())
    }
  }))
  # "Add": a verb of the plan not shown yet, its card in its place, on
  # (the form drawn again has new ids: read them again when it is)
  # (#440: "+ Add a card"; a template report's card of its pipe is shown
  # to be overridden -- nothing written till a value is changed)
  shiny::observeEvent(input$b_add_pick, {
    fn <- input$b_add_pick
    id <- bform$id
    # (a template report with no ARD yet: no form -- the card waits, shown
    # once the preview has made the ARD)
    cur <- current()
    if (!is.null(cur) && !identical(id, cur) && !is.null(.tpl_of(rv$p, cur))) {
      bform$added_by[[cur]] <- unique(c(bform$added_by[[cur]] %||% character(), fn))
      bform$off_by[[cur]] <- setdiff(bform$off_by[[cur]] %||% character(), fn)
      notify(sprintf(t("The card %s is shown once the ARD is made (Update preview)."), fn))
      return()
    }
    shiny::req(length(fn) == 1L, nzchar(fn), !is.null(id), identical(id, current()))
    tpl <- isTRUE(bform$tpl)
    if (!fn %in% .plan_addable(bform$items, template = tpl)) {
      return(notify(sprintf(t("%s cannot be added here: a card the table has already, or no verb of the plan."), fn)))
    }
    if (tpl && fn %in% bform$made) {
      bform$added_by[[id]] <- unique(c(bform$added_by[[id]] %||% character(), fn))
      bform$off_by[[id]] <- setdiff(bform$off_by[[id]] %||% character(), fn)
      bform$just_added <- fn
      rv$bver <- rv$bver + 1L
      return()
    }
    card_switch(list(fn = fn, path = fn, present = FALSE), TRUE)
  })
  # the columns changed: the rows' candidates made again (a key taken out
  # of cols offered to the rows; one put in cols no more -- one column is
  # in cols or in rows, not both)
  shiny::observe(builder_guard({
    bform_drawn()
    ks <- input[[bid("key")]]
    # (none before the field is drawn; none after: every column taken out)
    n <- shiny::isolate(bform$n)
    if (!is.null(ks)) bform$key_seen <- n
    shiny::req(builder_case() %in% c("ok", "hierarchy"),
               identical(shiny::isolate(bform$key_seen), n))
    ks <- ks %||% character()
    st <- shiny::isolate(bform$st)
    m <- shiny::isolate(bform$meta)
    shiny::req(!is.null(st), !is.null(m))
    hier <- length(st$hierarchy) > 0L
    tb <- .rows_for(shiny::isolate(rv$p), "tables", shiny::isolate(bform$id))
    sk <- .stack_keys(tb$stack[1L], tb$stack_nest[1L])
    base <- .layer_choices(names(m$keys), ks, st$variables$variable, hier,
                           tb$stack[1L], tb$stack_nest[1L])
    sel0 <- shiny::isolate(input[[bid("layers")]]) %||% (if (hier) st$row_keys else st$layers)
    # (a hierarchy's levels stay: they are the analysis's)
    sel <- setdiff(sel0, setdiff(ks, if (hier) st$hierarchy))
    ch <- unique(c(sel, base))
    now <- if (hier) st$row_choices else st$layer_choices
    if (identical(sel, sel0) && setequal(ch, unique(c(sel0, now)))) return()
    if (hier) bform$st$row_choices <- ch else bform$st$layer_choices <- ch
    shiny::updateSelectizeInput(
      session, bid("layers"),
      choices = stats::setNames(ch, .layer_names(ch, if (hier) st$hierarchy, lang, stack = sk)),
      selected = sel)
  }))
  # plan_filter()'s condition (the condition builder): written through the
  # bridge as the item's `where` (none: no condition), the card's other
  # values as they are now
  filter_cond <- condition_builder_server(
    "b_filter_cond",
    data = shiny::reactive({
      bform_drawn()
      .filter_data(bform$meta)
    }),
    value = shiny::reactive({
      bform_drawn()
      bform$filter_where %||% NA_character_
    }),
    lang = lang, key = bform_drawn)
  filter_cond_d <- shiny::debounce(shiny::reactive(filter_cond()$expr), 600)
  shiny::observeEvent(filter_cond_d(), builder_guard({
    e <- filter_cond_d()
    shiny::req(builder_case() %in% c("ok", "hierarchy"), identical(bform$id, current()))
    at <- which(vapply(bform$items, function(it) identical(it$path, "plan_filter"), NA))
    shiny::req(length(at) > 0L)
    it <- bform$items[[at[1L]]]
    shiny::req(isTRUE(it$present) || isTRUE(it$added))
    e <- if (length(e) != 1L || is.na(e) || !nzchar(trimws(e))) NA_character_ else trimws(e)
    # (the condition as the builder writes it: the same one read back,
    # written another way, is no change)
    old <- .filter_where_text(it$args$where) %||% NA_character_
    if (identical(e, old) || (!is.na(old) && identical(e, .cond_tidy(old)))) return()
    args <- it$args %||% list()
    args$where <- if (is.na(e)) NULL else tflspec::tfl_r(e)
    p2 <- guarded(.builder_item_write(rv$p, bform$id, "plan_filter", args))
    if (is.null(p2)) return()
    rv$p <- p2
    rv$btouched <- TRUE
    bform$filter_where <- e
    bform$hidden <- .hidden_vars_read(e)
    fresh <- .builder_item_at(tryCatch(builder_items(p2, bform$id, bform$meta), error = function(e) list()),
                              "plan_filter")
    if (!is.null(fresh)) {
      was_present <- isTRUE(it$present)
      bform$items[[at[1L]]][c("args", "own_args", "from", "arg_from", "below", "revert", "present")] <-
        fresh[c("args", "own_args", "from", "arg_from", "below", "revert", "present")]
      if (!was_present || !identical(fresh$from, it$from)) rv$bver <- rv$bver + 1L
    }
  }), ignoreInit = TRUE)
  # plan_digits()' rounding: the study's (every report's), written into
  # the study's defaults.yml
  shiny::observeEvent({
    bform_drawn()
    input[[paste0(card_id("plan_digits"), "__rounding")]]
  }, {
    v <- input[[paste0(card_id("plan_digits"), "__rounding")]]
    shiny::req(!is.null(v), nzchar(v), identical(bform$id, current()))
    if (identical(unname(rv$p$study[["rounding"]]), v)) return()
    p2 <- guarded(.spec_set_rounding(rv$p, v))
    if (is.null(p2)) return()
    rv$p <- p2
    rv$btouched <- TRUE
  }, ignoreInit = TRUE)
  # plan_levels(.drop_empty = ): a variable's values with no record hidden
  # (variables$empty_levels "hide") or shown
  shiny::observe(builder_guard({
    bform_drawn()
    shiny::req(builder_case() %in% c("ok", "hierarchy"))
    v <- bform$st$variables
    shiny::req(is.data.frame(v), nrow(v) > 0L)
    got <- lapply(seq_len(nrow(v)), function(i) input[[bid(paste0("lvdrop", i))]])
    shiny::req(any(!vapply(got, is.null, NA)))
    id <- shiny::isolate(bform$id)
    shiny::req(identical(id, shiny::isolate(current())))
    p <- shiny::isolate(rv$p)
    its <- builder_items(p, id)
    lv <- .builder_args(its, "plan_levels") %||% list()
    now <- as.character(unlist(lv$.drop_empty))
    want <- now
    for (i in seq_along(got)) {
      if (is.null(got[[i]])) next
      want <- if (isTRUE(got[[i]])) union(want, v$variable[i]) else setdiff(want, v$variable[i])
    }
    if (setequal(want, now)) return()
    if (length(want)) lv$.drop_empty <- want else lv$.drop_empty <- NULL
    p2 <- guarded(.spec_set_items(p, id, list(list(fn = "plan_levels", args = lv,
                                                   drop_empty = TRUE))))
    if (is.null(p2)) return()
    rv$p <- p2
    rv$btouched <- TRUE
  }))
  # The variables shown: those unticked are written as the row filter
  # (plan_filter(where = !variable %in% c(...)), its other arguments kept);
  # the ARD stays as it is.  At least one stays shown.
  shiny::observeEvent({
    bform_drawn()
    input[[bid("shown")]]
  }, {
    shiny::req(builder_case() %in% c("ok", "hierarchy"), identical(bform$id, current()),
               !is.null(bform$hidden))
    vars <- bform$st$variables$variable
    sh <- input[[bid("shown")]]
    if (is.null(sh)) {
      if (isTRUE(bform$shown_seen == bform$n)) notify(t("Keep at least one variable shown."))
      return()
    }
    bform$shown_seen <- bform$n
    hidden <- setdiff(vars, sh)
    if (setequal(hidden, bform$hidden)) return()
    id <- bform$id
    pf <- .builder_args(builder_items(rv$p, id), "plan_filter") %||% list()
    w <- .hidden_vars_write(vars[vars %in% hidden])
    if (is.na(w)) pf$where <- NULL else pf$where <- tflspec::tfl_r(w)
    p2 <- guarded(.spec_set_items(rv$p, id, list(list(fn = "plan_filter", args = pf,
                                                      drop_empty = TRUE))))
    if (is.null(p2)) return()
    rv$p <- p2
    rv$btouched <- TRUE
    rv$bver <- rv$bver + 1L
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$b_rows_step1, go("ard"))
  shiny::observeEvent(input$b_norm_step1, go("ard"))
  shiny::observeEvent(input$b_sort_reset, {
    id <- bform$id
    st <- bform$st
    shiny::req(!is.null(id), length(st$hierarchy) > 0L)
    n <- length(st$row_keys)
    ps <- .builder_args(builder_items(rv$p, id), "plan_sort") %||% list()
    ps$keys <- .split_list(.hier_sort(c(rep("alpha", n - 1L), "freq"),
                                      hier = st$row_keys %in% st$hierarchy))
    p2 <- guarded(.spec_set_items(rv$p, id, list(list(fn = "plan_sort", args = ps))))
    if (is.null(p2)) return()
    rv$p <- p2
    rv$btouched <- TRUE
    rv$bver <- rv$bver + 1L
  })
  .srv_keep(environment())
}
