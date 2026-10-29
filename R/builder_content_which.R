# The table builder's content, part 2: which cards (#440) -- the card kinds
# and which ones a table's plan shows, in the order the plan applies them.
# Split from R/builder_content.R (no code change).

# ---- which cards (#440, _archive/builder-card-layout-design-2026-10-20.md) ----

# A verb's tier, as tflspec's schema says it (tfl_schema(fn)$tier, "for
# the GUI"): "required" (always a card), "usual" (an empty card of a new
# report, before it is saved), "other" (through "+ Add a card").  The
# table below is the fallback for a schema with no tier.  One tier for
# every kind of table.
.plan_required <- c("normalize_ard", "table_plan", "plan_cells")

.plan_usual <- function(hier = FALSE) {
  c("plan_levels", "plan_labels", "plan_sort", "plan_digits", "plan_col_header")
}

.plan_study_cards <- c("plan_stub", "plan_blanks", "plan_paginate_rows", "plan_columns",
                       "plan_style")

.plan_tier_cache <- new.env()
.plan_tier <- function(fn) {
  hit <- .plan_tier_cache[[fn]]
  if (!is.null(hit)) return(hit)
  tr <- tryCatch(tflspec::tfl_schema(fn)$tier, error = function(e) NULL)
  if (!(length(tr) == 1L && tr %in% c("required", "usual", "other"))) {
    tr <- if (fn %in% .plan_required) "required" else
      if (fn %in% c(.plan_usual(), .plan_study_cards)) "usual" else "other"
  }
  .plan_tier_cache[[fn]] <- tr
  tr
}
.plan_tier_fns <- function(tier, fns = names(.plan_meaning)) {
  fns[vapply(fns, function(f) identical(.plan_tier(f), tier), NA)]
}

# The verbs a template report never shows a card of: they are in the
# template's call (the pipe after it starts with a plan)
.tpl_inner_fns <- c("normalize_ard", "table_plan")

# The items as cards -- one rule: the required ones, the items the report
# has (its YAML's, the study standard's in effect), and those added here
# (`added`); a new report not saved yet (`fresh`) also has the usual ones,
# empty (nothing is written till a value is given).  No on / off switch
# (the Total column's own excepted: it writes the ARD's overall rows too).
# `off`: added or usual cards taken away again (not shown).  Each told
# whether the menu's Remove takes it away (`removable`): a card of the
# report's own only (no study standard under it -- that one has Revert),
# or one shown empty.  A template report (`template`): its template's card
# above, then only the cards it overrides or adds -- no required card,
# none of the template's own call (.tpl_inner_fns).
.builder_items_cards <- function(items, hier = FALSE, added = character(), ard_total = NA,
                                off = character(), fresh = FALSE, template = FALSE,
                                code_fns = character()) {
  added <- setdiff(added, off)
  if (template) {
    # (one card a line of the program after the template's call --
    # `code_fns`, .tpl_code_fns(): the study standard's items and the
    # report's own -- and those added here)
    items <- Filter(function(it) !it$fn %in% .tpl_inner_fns &&
                      (identical(it$from, "own") || it$fn %in% c(added, code_fns)), items)
    items <- lapply(items, function(it) {
      if (!identical(it$from, "own") && it$fn %in% code_fns &&
          "defaults" %in% unlist(it$arg_from)) it$from <- "defaults"
      it
    })
    items <- .builder_items_shown(items, setdiff(added, .tpl_inner_fns))
  } else {
    usual <- if (fresh) setdiff(.plan_tier_fns("usual"), off) else character()
    items <- .builder_items_shown(items, unique(c(.plan_tier_fns("required"), usual, added)))
  }
  lapply(items, function(it) {
    req <- !template && (identical(.plan_tier(it$fn), "required") || isTRUE(it$required))
    it$required <- req
    it$added <- !isTRUE(it$present)
    own <- identical(it$from, "own") && !isTRUE(it$revert) && is.null(it$below)
    # (a template report's card of its pipe, shown to be overridden: taken
    # away again)
    it$removable <- !req && !isTRUE(it$fixed) &&
      (!isTRUE(it$present) || own ||
         (template && !identical(it$from, "own") && !it$fn %in% code_fns))
    it$switch <- FALSE
    if (identical(it$fn, "plan_total")) {
      it$switch <- TRUE
      if (!isTRUE(it$present) && identical(ard_total, "none")) {
        it$switch <- FALSE
        it$blocked <- "The ARD has no overall rows."
      }
    }
    it$on <- isTRUE(it$present) || req || it$added
    it
  })
}

# The ARD's Total: "overall" when it has rows over all subjects (no
# column key: ard_stack(.overall = TRUE)), which plan_total() makes the
# Total column; "none" when it has none (plan_total() would stop); NA not
# known (an older saved meta without `overall`).  `meta`: ard_meta().
.ard_total_kind <- function(meta) {
  if (is.null(meta) || is.null(meta$overall)) return(NA_character_)
  if (isTRUE(meta$overall)) "overall" else "none"
}

# The usual cards the ARD itself tells the values of -- on by default (a
# new table writes them): the Total column when the ARD has rows over all
# subjects, the values' order when its variables or its key columns keep a
# factor's levels, the labels when it carries the variables' labels.
# `meta`: ard_meta().
.ard_on_cards <- function(meta, hier = FALSE) {
  if (is.null(meta)) return(character())
  usual <- .plan_usual(hier)
  v <- meta$variables
  on <- c(if (identical(.ard_total_kind(meta), "overall")) "plan_total",
          if (length(intersect(meta$factor_vars, v$variable)) ||
                length(meta$factor_keys)) "plan_levels",
          if (is.data.frame(v) && any(!is.na(v$label) & nzchar(v$label %||% ""))) "plan_labels")
  # (the Total column too, though not a usual card: the ARD tells it)
  intersect(on, c(usual, "plan_total"))
}

# The verbs "Add" offers: the plan's, in the schema's order, less those
# shown (a verb is one card) and the required
.plan_addable <- function(items, template = FALSE) {
  shown <- vapply(items, function(it) it$fn, "")
  setdiff(names(.plan_meaning), c(if (template) .tpl_inner_fns else .plan_tier_fns("required"), shown))
}

# "+ Add a card": every verb of the plan in the pipe's order (the schema's
# rank), each with whether it can be added (a card shown already cannot);
# a template report's in two groups -- the cards the template's pipe has
# (`made`: overriding them), then the new ones -- and never its call's own
# verbs.  A list of list(fn, group, ok).
.plan_add_menu <- function(items, made = character(), template = FALSE) {
  shown <- vapply(items, function(it) it$fn, "")
  rank <- function(f) as.numeric(.schema_of(f)$rank %||% 500)
  fns <- names(.plan_meaning)
  fns <- setdiff(fns, if (template) .tpl_inner_fns else .plan_tier_fns("required"))
  fns <- fns[order(vapply(fns, rank, 0))]
  group <- if (template) ifelse(fns %in% made, "made", "new") else rep("new", length(fns))
  ord <- order(match(group, c("made", "new")))
  lapply(ord, function(i) list(fn = fns[[i]], group = group[[i]], ok = !fns[[i]] %in% shown))
}

# ... the schema of those verbs, for add_item_input() (NULL: none known)
.plan_add_schema <- function(items) {
  sch <- tryCatch(tflspec::tfl_schema(), error = function(e) NULL)
  if (is.null(sch)) return(NULL)
  sch[intersect(names(sch), .plan_addable(items))]
}

# What a card switched on (or added) is written with: its values from the
# ARD and the usual ones (`st`: builder_read()'s state); NULL for a verb
# with no such value (its card opens to be filled: its fields or R)
.plan_starter_args <- function(fn, st = NULL, hier = FALSE, meta = NULL) {
  v <- st$variables
  switch(
    fn,
    # the ARD's labels (none: the card opens to be filled)
    plan_labels = {
      if (!is.data.frame(v) || !nrow(v)) return(NULL)
      has <- !is.na(v$label) & nzchar(v$label)
      if (!any(has)) return(NULL)
      as.list(stats::setNames(v$label[has], v$variable[has]))
    },
    # the levels in their order (.level_order()): the variables' and the
    # key columns' (the columns' arms, a key's levels) -- those the ARD
    # keeps a factor of, when it keeps any
    plan_levels = {
      lv <- Filter(length, st$levels %||% list())
      fac <- c(meta$factor_vars, meta$factor_keys)
      if (length(fac)) {
        keys <- setdiff(meta$factor_keys, names(lv))
        kl <- stats::setNames(lapply(keys, function(k) st$arms[[k]] %||% meta$keys[[k]]), keys)
        lv <- Filter(length, c(lv, kl))
        lv <- lv[intersect(names(lv), fac)]
      }
      if (!length(lv)) NULL else lv
    },
    plan_digits = list(rounding = "sas"),
    plan_sort = if (hier && length(st$row_keys)) {
      list(keys = trimws(strsplit(.hier_sort(
        c(rep("alpha", length(st$row_keys) - 1L), "freq"),
        hier = st$row_keys %in% st$hierarchy), "|", fixed = TRUE)[[1L]]))
    },
    plan_col_header = {
      hp <- header_presets()
      ch <- .header_preset_choices(hier)
      if (!length(ch)) NULL else list(header = hp[[ch[[1L]]]])
    },
    plan_stub = list(name = "row_label"),
    plan_blanks = list(where = "between_groups"),
    plan_paginate_rows = list(max_rows = 30L, split = "group_safe"),
    plan_style = list(align_count_pct = TRUE),
    # (a part: the ARD's summary statistics under the table, by its columns)
    plan_append = list(name = "summary", data = list(source = "ard"),
                       content = .part_quickstart(st$key[!is.na(st$key)])),
    NULL)
}

# Where a variable's order comes from (.level_order()'s `source` and
# `differs`), in words: the same lines under the columns' order and on
# plan_levels()' card.  This table's own order says nothing; as first
# seen is a warning (check it).  A list: `text` (the sentences), `warn`.
.level_order_words <- c(
  ard = "The ARD's order (its factor levels).",
  codelist = "The code list's order (ARD step 1-1).",
  seen = "Check this order: the data do not give one (no factor, no numeric twin such as TRT01AN), so it is the ARD's.",
  differs = "The ARD's order differs from the code list's; the table uses the code list's.")

.level_order_text <- function(from, lang = "en") {
  src <- from$source %||% "seen"
  # (this table's own, or the plan's -- a stack's subgroups: nothing to say)
  if (src %in% c("table", "plan")) return(NULL)
  words <- .level_order_words[[if (src %in% names(.level_order_words)) src else "seen"]]
  txt <- tr(words, lang)
  if (isTRUE(from$differs)) txt <- paste(txt, tr(.level_order_words[["differs"]], lang))
  list(text = txt, warn = identical(src, "seen") || isTRUE(from$differs))
}

# ... as a line (NULL: none)
.level_order_note <- function(from, lang = "en") {
  w <- .level_order_text(from, lang)
  if (is.null(w)) return(NULL)
  shiny::div(class = paste("small rp-level-from", if (w$warn) "text-warning" else "text-muted"),
             w$text)
}

# A card's own line above its body, by verb (`r`: the card's Edit as R is
# offered beside it; `level_from`: the variables' orders' sources, for
# plan_levels()): what the card does not show yet, or where its values go
.plan_card_note <- function(fn, r = FALSE, lang = "en", level_from = NULL) {
  t <- function(x) tr(x, lang)
  switch(
    fn,
    # (said behind the (i): where the card writes -- plan_cells(), the
    # study's defaults.yml -- one card a function's exception)
    plan_digits = shiny::div(class = "rp-card-note",
                             help_tip(paste(t("The decimals are written into plan_cells()'s formats."),
                                            t("Applies to every report (the study's defaults.yml)")))),
    # (a line; what it does and one row before and after under the (i))
    normalize_ard = shiny::div(
      class = "rp-card-note",
      with_tip(t("Shapes the ARD for making the table."),
               t(paste("normalize_ard() shapes the cards ARD for making the table.",
                       "Before: group1 = TRT01A, group1_level = Drug X, variable = SEX,",
                       "variable_level = F, stat_name = n -> After: TRT01A = Drug X,",
                       "variable = SEX, .label = F, stat_name = n. The groups become columns",
                       "(TRT01A), the analysis variables stay values of variable, and a",
                       "category's value becomes .label (the row's text).")))),
    plan_levels = shiny::div(
      class = "rp-card-note",
      help_tip(t(paste("Sets the order of the levels of the column and row keys (the groups,",
                       "an analysis variable's values). The analysis variables' own order",
                       "(variable) is here too. Levels with no data can be hidden (.drop_empty).")))),
    plan_sort = shiny::div(
      class = "rp-card-note",
      with_tip(builder_label(.arg_meaning("plan_sort", "keys"), "keys", "arg", lang),
               t(paste("The keys, first to last: .overall the any-event row first;",
                       "group1, group2 ... the rows' outer levels, label the innermost",
                       "(the rows' text), each by name (a code list's order when it has one);",
                       ".depth each group's own row above the rows under it;",
                       "-n most frequent first (-n@group1: by the group's own count);",
                       "levels(group1) the values plan_levels() names first, in its order,",
                       "the others by the next keys;",
                       "-max(n) the most frequent in any column first, min(n) the least;",
                       "-p, -estimate by another statistic of the ARD. Another key is",
                       "written as it is.")))),
    NULL)
}

# A card's mark: where its item comes from (normalize_ard() with no
# argument -- a table of no hierarchy -- none: nothing of anyone's on it)
.card_from <- function(it) {
  if (identical(it$fn, "normalize_ard") && !length(it$args)) return(NULL)
  af <- unlist(it$arg_from)
  if (identical(it$from, "own") || (length(af) && any(af == "own")) || length(it$own_args)) {
    return("own")
  }
  if (startsWith(it$from %||% "", "template:") && length(af) && any(af == "defaults")) {
    return("defaults")
  }
  it$from
}
# ... its mark and Revert as a card's head has them (sent after a write:
# the page puts them in the card's head, .builder_cards_js)
.card_mark <- function(it, id, lang = "en") {
  shiny::tagList(
    from_badge(.card_from(it), lang,
               own = if (isTRUE(it$present) && identical(.card_from(it), "own")) "This table"),
    if (isTRUE(it$revert)) .card_revert(id, lang))
}

# The cards of a table's content, in its order: one builder_card() an
# item, with a thin line before the first item of each group (Mapping,
# Finishing; its one line of what it is for) -- the content's order is
# kept, the line only says where a group starts, and each card's head
# names its group (faint), so a group that comes back later is still told.
# normalize_ard() has no line: first, its head says ARD.  `bodies`: the
# forms the builder has for some (by path, or by verb); the rest show
# their arguments.  `id`: the card's input id of a path.  The required
# cards (.plan_required, and an item marked `required`) have the bar; the
# others a switch where the item says (`switch`, .builder_items_cards();
# else where `switches` names the verb), none elsewhere.  An item of the
# report's own is marked "This table".  The seldom changed cards
# (.plan_rare) are closed when drawn.  `new`: the path of a card just
# added (it takes the focus).  `level_from`: where each variable's order
# comes from (builder_read()'s), said on plan_levels()' card.
.table_cards <- function(items, bodies = list(), id = function(path) path,
                         switches = character(), lang = "en", new = NULL,
                         level_from = NULL) {
  t <- function(x) tr(x, lang)
  started <- character()
  out <- list()
  for (it in items) {
    body <- bodies[[it$path]] %||% bodies[[it$fn]]
    own_form <- !is.null(body)
    if (!own_form) body <- .item_args_view(it, lang)
    # (`required` as the item says -- none in a template report -- else by
    # the verb)
    req <- if (!is.null(it$required)) isTRUE(it$required) else it$fn %in% .plan_required
    sw <- if (!is.null(it$switch)) isTRUE(it$switch) else it$fn %in% switches
    on <- if (!is.null(it$on)) isTRUE(it$on) else (isTRUE(it$present) || req || !sw)
    # a card that cannot be on (the ARD lacks what it reads): off, its
    # reason where its body would be
    blocked <- !is.null(it$blocked)
    if (blocked) on <- FALSE
    pending <- !isTRUE(it$present) && on
    # an item the content has not (its form would add it): no mark of its
    # own -- "not in the code yet"
    # (#440: a card added or shown empty is the table's own card, drawn as
    # any -- its mark "This table"; nothing is written till a value is)
    absent <- !isTRUE(it$present) && !blocked && !isTRUE(it$added) && (pending || (!sw && !req))
    tools <- c(if ((isTRUE(it$present) || pending) && !isTRUE(it$fixed)) "r",
               if (isTRUE(it$removable) && !req) "remove")
    sec <- .plan_section(it$fn)
    if (!sec %in% started) {
      started <- c(started, sec)
      if (sec %in% names(.plan_section_notes)) {
        out[[length(out) + 1L]] <- shiny::div(
          class = "rp-card-sep", `data-section` = sec,
          shiny::tags$span(class = "rp-card-sep-label", t(.plan_section_labels[[sec]])),
          shiny::tags$span(class = "rp-card-sep-note", t(.plan_section_notes[[sec]])))
      }
    }
    rare <- it$fn %in% .plan_rare
    note <- .plan_card_note(it$fn, r = "r" %in% tools, lang = lang, level_from = level_from)
    from <- .card_from(it)
    out[[length(out) + 1L]] <- builder_card(
      id(it$path), it$fn, .verb_meaning(it$fn), from = from, required = req,
      on = on, switchable = sw, revert = isTRUE(it$revert),
      # (off: no body -- the summary says what turning it on does)
      body = if (!on) NULL else if (is.null(note)) body else shiny::tagList(note, body),
      # (off: what turning it on does, said where its body would be)
      summary = if (isTRUE(it$present)) .item_summary(it, lang = lang) else
        if (blocked) t(it$blocked) else if (!on) t("On: its usual values are written."),
      empty = if (!isTRUE(it$present) && isTRUE(it$added)) "Not set (not in the code)",
      tools = tools,
      # the study's defaults the report leaves as they are, the seldom
      # changed and the off: closed
      open = on && (pending || (!rare && (req || own_form || identical(it$from, "own")))),
      path = it$path, lang = lang,
      own_label = if (isTRUE(it$present) && identical(from, "own")) "This table",
      absent = absent, rare = rare, section = sec,
      tag = .plan_section_labels[[sec]],
      new = identical(it$path, new),
      also = if (identical(it$fn, "plan_digits")) "plan_cells")
  }
  out
}

# The bar above the cards: "Cards (plan)" -- every card in the content's
# order, by group, each a link that opens it and brings it into view (the
# seldom changed ones faint)
.table_cards_bar <- function(items, id = function(path) path, lang = "en") {
  t <- function(x) tr(x, lang)
  started <- character()
  entries <- list()
  for (it in items) {
    sec <- .plan_section(it$fn)
    if (!sec %in% started) {
      started <- c(started, sec)
      if (sec %in% names(.plan_section_notes)) {
        entries[[length(entries) + 1L]] <- shiny::tags$li(
          shiny::tags$h6(class = "dropdown-header", t(.plan_section_labels[[sec]])))
      }
    }
    entries[[length(entries) + 1L]] <- shiny::tags$li(shiny::tags$a(
      href = "#", class = paste("dropdown-item rp-layer-go",
                                if (it$fn %in% .plan_rare) "rp-layer-rare"),
      `data-target` = id(it$path),
      builder_label(.verb_meaning(it$fn), it$fn, "fn", lang)))
  }
  shiny::div(
    class = "rp-cards-bar",
    shiny::div(class = "dropdown",
               shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary dropdown-toggle rp-layers-btn",
                                  `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
                                  t("Cards (plan)")),
               shiny::tags$ul(class = "dropdown-menu", entries)))
}

# "+ Add a card" below the last card: a menu of every verb in the pipe's
# order (.plan_add_menu()), those shown already greyed; a click sends the
# verb (`input_id`, an event)
.plan_add_menu_ui <- function(input_id, menu, lang = "en") {
  t <- function(x) tr(x, lang)
  heads <- c(made = "The template's cards (override)", new = "New cards")
  groups <- unique(vapply(menu, function(m) m$group, ""))
  entries <- list()
  for (g in groups) {
    if (length(groups) > 1L) {
      entries[[length(entries) + 1L]] <- shiny::tags$li(shiny::tags$h6(class = "dropdown-header", t(heads[[g]])))
    }
    for (m in Filter(function(m) identical(m$group, g), menu)) {
      entries[[length(entries) + 1L]] <- shiny::tags$li(shiny::tags$a(
        href = "#", class = paste("dropdown-item", if (!m$ok) "disabled"),
        `aria-disabled` = if (!m$ok) "true",
        onclick = if (m$ok) sprintf(
          "event.preventDefault(); Shiny.setInputValue('%s', '%s', {priority: 'event'})", input_id, m$fn),
        builder_label(.verb_meaning(m$fn), m$fn, "fn", lang)))
    }
  }
  shiny::div(
    class = "dropdown rp-add-menu mt-2",
    shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-primary dropdown-toggle",
                       `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
                       paste0("+ ", t("Add a card"))),
    shiny::tags$ul(class = "dropdown-menu", entries))
}
