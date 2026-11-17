# The table field of an argument whose def's input is "table" (tflspec's
# tfl_arg_defs()): a map<K,V> a table of two columns (key, value), a
# rows<map<string,V>> a table whose columns are the record's keys.  Rows
# are added, removed and (a rows<...>, an ordered map) moved; the rows given
# as fixed (the template's ALL row) show on top, read only, and are not
# read back.  The whole table is one input (`<card id>__<arg>`): its state as
# JSON, the binding in .builder_cards_js; builder_card_read() reads it back
# as the type says (.tbl_read()).
#
#   .tbl_spec(type, def, ctx, value, lang, choices)  the table's columns, NULL
#                                                    when the type is no table
#   .field_table(fid, spec, value, fixed_rows, lang) the field
#   .tbl_state(spec, value)                          the state the field holds
#   .tbl_read(json)                                  the state -> the value
#
# A cell's kind: "text" (a text field, the choices offered as it is typed),
# "pick" (a select of the choices), "r" (R, monospace, the columns offered),
# "number_text" (a number, else text), "list" (several values, one a line,
# in order; a select beside it adds one of the choices).

# ---- the type -> the table's columns -------------------------------------------

# "map<column,r>" -> list(shape = "map", key = "column", value = "r");
# "rows<map<string,string|r>>" -> list(shape = "rows", key = "string",
# value = "string|r"); "rows<subgroup, subgroup_lvl, where>" (a data frame
# of those fields, tflspec's schema notation) -> list(shape = "rows", key =
# "string", value = "string|r", fields = c("subgroup", ...)); of a type of
# alternatives ("rows<...>|call<...>") the first that is a table; NULL for
# any other type
.tbl_type <- function(type) {
  if (is.null(type) || length(type) != 1L || is.na(type)) return(NULL)
  alts <- trimws(.type_split(trimws(type), "|"))
  ty <- alts[grepl("^(rows|map)<.*>$|^list<map<.*>>$", alts)][1L]
  if (is.na(ty)) return(NULL)
  shape <- "map"
  # (a list<map<..>>: its maps in order, a rows<map<..>>'s table)
  if (grepl("^(rows|list)<.*>$", ty)) {
    shape <- "rows"
    ty <- trimws(sub("^(rows|list)<(.*)>$", "\\2", ty))
    if (!grepl("^map<.*>$", ty)) {
      f <- trimws(.type_split(ty, ","))
      f <- f[nzchar(f)]
      if (!length(f) || !all(grepl("^[A-Za-z.][A-Za-z0-9._]*$", f))) return(NULL)
      return(list(shape = "rows", key = "string", value = "string|r", fields = f))
    }
  }
  if (!grepl("^map<.*>$", ty)) return(NULL)
  kv <- .type_split(sub("^map<(.*)>$", "\\1", ty), ",")
  if (length(kv) != 2L) return(NULL)
  # (a map of maps, map<K,map<K2,V>>: a row an inner entry, its two keys
  # and its value -- tflspec_summary's nest and value_labels)
  if (identical(shape, "map") && grepl("^map<.*>$", kv[[2L]])) {
    kv2 <- .type_split(sub("^map<(.*)>$", "\\1", kv[[2L]]), ",")
    if (length(kv2) != 2L || grepl("^(map|rows|list)<", kv2[[2L]])) return(NULL)
    return(list(shape = "map2", key = kv[[1L]], key2 = kv2[[1L]], value = kv2[[2L]]))
  }
  list(shape = shape, key = kv[[1L]], value = kv[[2L]])
}

# a rows<...> value as its records: a data frame by row, a list as it is
.tbl_records <- function(value) {
  if (!is.data.frame(value)) return(value)
  lapply(seq_len(nrow(value)), function(i) {
    r <- lapply(value, function(col) if (is.list(col)) col[[i]] else col[i])
    r[!vapply(r, function(x) length(x) == 1L && is.atomic(x) && is.na(x), NA)]
  })
}

# the kind of a cell of type `ty` (its first alternative decides, r last)
.tbl_cell_kind <- function(ty) {
  alts <- .type_split(trimws(ty %||% "string"), "|")
  alts <- c(setdiff(alts, "r"), intersect(alts, "r"))
  first <- alts[[1L]]
  if (identical(first, "r")) return("r")
  if (grepl("^(list|vec)<", first)) return("list")
  if (first %in% c("number", "integer")) return("number_text")
  "text"
}

# a column's choices from its source words; nothing when the ctx cannot say
.tbl_choices <- function(sources, ctx, lang, values = NULL) {
  if (!length(sources)) return(character())
  tryCatch(.choice_values(sources, ctx, lang, values = values), error = function(e) character())
}

# The table's columns: `cols` their names, `heads` their headings, `kinds`
# their cells' kinds, `choices` each one's choices, `ordered` whether rows
# move.  A map: the key (picked from the def's choices -- typed when the
# def is `free`, or a new column's name when the value is R, the choices
# then offered in the R) and the value.  A rows<...>: the def's `columns`
# ("params": the template's params by name; a list by name: each column's
# own type, input and choices), else the keys of the value.  NULL when the
# type is no table, or a rows<...> has no columns to show.
.tbl_spec <- function(type, def = NULL, ctx = NULL, value = NULL, lang = "en", choices = NULL) {
  tt <- .tbl_type(type)
  if (is.null(tt)) return(NULL)
  ctx <- ctx %||% list()
  if (is.null(choices)) choices <- .tbl_choices(def$choices, ctx, lang, values = def$values)
  if (identical(tt$shape, "map2")) {
    # (a key that is a column: picked from the def's choices, else typed)
    key_kind <- function(k) if (grepl("^column", k) && length(choices) && !isTRUE(def$free)) "pick" else "text"
    key_ch <- function(k) if (grepl("^column", k)) choices else character()
    vk <- .tbl_cell_kind(tt$value)
    return(list(shape = "map2", cols = c("key", "key2", "value"),
                heads = c(tr(if (grepl("^column", tt$key)) "Column" else "Key", lang),
                          tr(if (grepl("^column", tt$key2)) "Second column" else "Key", lang),
                          tr(if (identical(vk, "list")) "Values" else "Value", lang)),
                kinds = c(key_kind(tt$key), key_kind(tt$key2), vk),
                choices = list(key_ch(tt$key), key_ch(tt$key2), character()),
                ordered = isTRUE(def$ordered)))
  }
  if (identical(tt$shape, "map")) {
    vk <- .tbl_cell_kind(tt$value)
    to_value <- identical(vk, "r")
    kk <- if (!to_value && length(choices)) (if (isTRUE(def$free)) "text" else "pick") else "text"
    key_head <- if (to_value && grepl("^column", tt$key)) "New column" else
      if (grepl("^column", tt$key)) "Column" else "Key"
    val_head <- switch(vk, r = "R", list = "Values", "Value")
    return(list(shape = "map", cols = c("key", "value"),
                heads = c(tr(key_head, lang), if (identical(val_head, "R")) "R" else tr(val_head, lang)),
                kinds = c(kk, vk),
                choices = list(if (!to_value) choices else character(),
                               if (to_value) choices else character()),
                ordered = isTRUE(def$ordered)))
  }
  vk <- .tbl_cell_kind(tt$value)
  cdefs <- def$columns
  value <- .tbl_records(value)
  # (the columns' words: "params" the template's params; "name?" a column
  # the table may have, shown when a row has it, else added by its button
  # -- e.g. "params, analysis_id?" (canon b5db722); a list by name: each
  # column's def, `required = FALSE` one the table may have (tflspec#822:
  # analysis.conditions' where and analysis_id); an entry that is the word
  # "params" the params' columns there; the value's own keys after them)
  # ("params" of a rows<f1, f2, ...>: its fields, a template's param's own)
  params <- function() {
    if (length(tt$fields)) return(tt$fields)
    p <- ctx$params
    if (is.list(p) && !is.null(names(p))) names(p) else as.character(unlist(p))
  }
  opt <- character()
  cols <- if (is.list(cdefs) && length(names(cdefs))) {
    is_col <- vapply(cdefs, is.list, NA)
    opt <- names(cdefs)[is_col][vapply(cdefs[is_col], function(x) {
      identical(x$required, FALSE) || isTRUE(x$optional)
    }, NA)]
    nm <- unlist(lapply(seq_along(cdefs), function(i) {
      if (is_col[[i]]) names(cdefs)[[i]] else if (identical(cdefs[[i]], "params")) params()
    }))
    c(nm, unlist(lapply(value, names)))
  } else if (length(cdefs)) {
    w <- .choice_words(cdefs)
    opt <- sub("[?]$", "", w[grepl("[?]$", w)])
    unlist(lapply(sub("[?]$", "", w), function(x) if (identical(x, "params")) params() else x))
  } else {
    unique(c(tt$fields, unlist(lapply(value, names))))
  }
  cols <- unique(cols[!is.na(cols) & nzchar(cols)])
  # (an optional column no row has: not shown, its button adds it)
  in_value <- unique(unlist(lapply(value, function(r) if (is.list(r)) names(r))))
  hidden <- setdiff(opt, in_value)
  all_cols <- cols
  cols <- setdiff(cols, hidden)
  if (!length(cols)) return(NULL)
  cd <- if (is.list(cdefs) && length(names(cdefs))) cdefs else list()
  # (the columns a condition's R takes, for a where-like cell)
  cond_choices <- function(src) {
    x <- if (length(src)) .tbl_choices(src, ctx, lang) else choices
    if (!length(x)) x <- .tbl_choices("vars", ctx, lang)
    x
  }
  kind_of <- function(k) {
    c1 <- if (is.list(cd[[k]])) cd[[k]] else list()
    ck <- if (!is.null(c1$type)) .tbl_cell_kind(c1$type) else vk
    has_r <- any(vapply(value, function(r) inherits(r[[k]], "tfl_r"), NA))
    is_cond <- identical(k, "where") || identical(c1$input, "condition")
    if (is_cond || (has_r && grepl("(^|\\|)\\s*r\\s*($|\\|)", c1$type %||% tt$value))) ck <- "r"
    ch <- if (identical(ck, "r")) cond_choices(c1$choices) else .tbl_choices(c1$choices, ctx, lang)
    if (identical(ck, "text") && length(ch) && identical(c1$input, "pick") && !isTRUE(c1$free)) ck <- "pick"
    list(kind = ck, choices = ch)
  }
  ks <- lapply(cols, kind_of)
  # (the optional columns not shown, in the columns' order, each with its kind)
  extra <- lapply(intersect(all_cols, hidden), function(k) c(list(col = k), kind_of(k)))
  list(shape = "rows", cols = cols, heads = cols,
       kinds = vapply(ks, function(x) x$kind, ""), choices = lapply(ks, function(x) x$choices),
       ordered = TRUE, extra = extra)
}

# ---- the value <-> the state -------------------------------------------------------

# a value as its cell's text
.tbl_cell_text <- function(v, kind) {
  if (is.null(v) || inherits(v, "tfl_unset")) return("")
  if (inherits(v, "tfl_r")) return(as.character(unclass(v)))
  if (identical(kind, "r")) return(.arg_to_r(v))
  v <- as.character(unlist(v))
  if (identical(kind, "list")) return(paste(v, collapse = "\n"))
  paste(v, collapse = ", ")
}

# The rows of a value as cell texts (a list of character vectors, one a
# row): a map's entries in order, a rows<...>'s records
.tbl_rows <- function(spec, value) {
  if (identical(spec$shape, "rows")) value <- .tbl_records(value)
  if (is.null(value) || inherits(value, c("tfl_r", "tfl_unset")) || !length(value)) return(list())
  if (identical(spec$shape, "map2")) {
    nm <- names(value)
    if (is.null(nm)) return(list())
    out <- list()
    for (i in seq_along(value)) {
      inner <- as.list(value[[i]])
      nm2 <- names(inner)
      if (is.null(nm2)) next
      for (j in seq_along(inner)) {
        out[[length(out) + 1L]] <- c(nm[[i]], nm2[[j]], .tbl_cell_text(inner[[j]], spec$kinds[[3L]]))
      }
    }
    return(out)
  }
  if (identical(spec$shape, "map")) {
    nm <- names(value)
    if (is.null(nm)) return(list())
    value <- as.list(value)
    return(lapply(seq_along(value), function(i) {
      c(nm[[i]], .tbl_cell_text(value[[i]], spec$kinds[[2L]]))
    }))
  }
  lapply(value, function(r) {
    vapply(seq_along(spec$cols), function(j) .tbl_cell_text(r[[spec$cols[[j]]]], spec$kinds[[j]]), "")
  })
}

# The state the field holds, as its input sends it: the shape, the
# columns, their kinds and the rows' cell texts
.tbl_state <- function(spec, value) {
  list(shape = spec$shape, cols = spec$cols, kinds = spec$kinds, rows = .tbl_rows(spec, value))
}

# a cell's text as its value: NULL when blank
.tbl_cell_value <- function(txt, kind) {
  txt <- trimws(paste(as.character(unlist(txt)), collapse = "\n"))
  if (!nzchar(txt)) return(NULL)
  switch(
    kind,
    r = tflspec::tfl_r(txt),
    list = {
      v <- trimws(strsplit(txt, "\n", fixed = TRUE)[[1L]])
      v <- v[nzchar(v)]
      if (length(v)) v
    },
    number_text = if (grepl("^[-+]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?$", txt)) as.numeric(txt) else txt,
    txt)
}

# The field's input (its state as JSON, or the state as a list) read back
# as the type says: a map a named list (a row of no key or no value left
# out), a rows<...> a list of named lists (a blank cell left out, a blank
# row too); NULL when nothing is left
.tbl_read <- function(x) {
  if (is.null(x)) return(NULL)
  st <- if (is.character(x)) {
    if (!length(x) || !nzchar(x[[1L]])) return(NULL)
    tryCatch(jsonlite::fromJSON(x[[1L]], simplifyVector = FALSE), error = function(e) NULL)
  } else x
  if (!is.list(st) || !length(st$rows)) return(NULL)
  kinds <- as.character(unlist(st$kinds))
  cols <- as.character(unlist(st$cols))
  rows <- lapply(st$rows, function(r) as.character(unlist(lapply(r, function(z) if (is.null(z)) "" else z))))
  shape <- as.character(unlist(st$shape))[1L]
  if (identical(shape, "map2")) {
    # (the rows by their first key, in the order first seen)
    out <- list()
    for (r in rows) {
      k <- trimws(r[1L] %||% "")
      k2 <- trimws(if (length(r) >= 2L) r[[2L]] else "")
      v <- .tbl_cell_value(if (length(r) >= 3L) r[[3L]] else "", kinds[[3L]])
      if (is.na(k) || !nzchar(k) || is.na(k2) || !nzchar(k2) || is.null(v)) next
      if (is.null(out[[k]])) out[[k]] <- list()
      out[[k]][[k2]] <- v
    }
    return(if (length(out)) out)
  }
  if (identical(shape, "map")) {
    out <- list()
    for (r in rows) {
      k <- trimws(r[1L] %||% "")
      v <- .tbl_cell_value(if (length(r) >= 2L) r[[2L]] else "", kinds[[2L]])
      if (is.na(k) || !nzchar(k) || is.null(v)) next
      out[[k]] <- v
    }
    return(if (length(out)) out)
  }
  out <- list()
  for (r in rows) {
    rec <- list()
    for (j in seq_along(cols)) {
      v <- .tbl_cell_value(if (j <= length(r)) r[[j]] else "", kinds[[j]])
      if (!is.null(v)) rec[[cols[[j]]]] <- v
    }
    if (length(rec)) out[[length(out) + 1L]] <- rec
  }
  if (length(out)) out
}

# whether an input is a table field's state (JSON of its kinds, or the list)
.tbl_is_state <- function(x) {
  if (is.list(x)) return(!is.null(x$kinds))
  is.character(x) && length(x) == 1L && !is.na(x) && grepl("^\\s*[{].*\"kinds\"", x)
}

# ---- the field --------------------------------------------------------------------

# a select's options: the blank, then the choices (groups as optgroups),
# the value kept when the choices lack it
.tbl_options <- function(choices, selected = "", blank = "") {
  opt <- function(v, lab) shiny::tags$option(value = v, selected = if (identical(v, selected)) NA, lab)
  flat <- as.character(unlist(choices))
  items <- if (is.list(choices) && !is.null(names(choices))) {
    lapply(names(choices), function(g) {
      x <- .choice_vec(choices[[g]])
      shiny::tags$optgroup(label = g, Map(opt, unname(x), names(x)))
    })
  } else {
    x <- .choice_vec(choices)
    Map(opt, unname(x), names(x))
  }
  keep <- if (nzchar(selected) && !selected %in% flat) opt(selected, selected)
  shiny::tagList(shiny::tags$option(value = "", blank), items, keep)
}

# one cell's control (no id: the table is the one input)
.tbl_cell <- function(kind, text, dl = NULL, choices = NULL, lang = "en") {
  base <- "form-control form-control-sm rp-tbl-cell"
  switch(
    kind,
    pick = shiny::tags$select(class = "form-select form-select-sm rp-tbl-cell",
                              .tbl_options(choices, text)),
    r = shiny::tags$input(type = "text", class = paste(base, "rp-mono rp-tbl-r"), value = text,
                          placeholder = "R", spellcheck = "false", list = dl),
    list = shiny::tagList(
      shiny::tags$textarea(class = paste(base, "rp-tbl-list"), rows = "2",
                           placeholder = tr("One per line", lang), spellcheck = "false", text),
      if (length(choices)) {
        shiny::tags$select(class = "form-select form-select-sm rp-tbl-add", title = tr("Add a value", lang),
                           .tbl_options(choices, "", blank = paste0("+ ", tr("Add a value", lang))))
      }),
    number_text = shiny::tags$input(type = "text", class = paste(base, "rp-tbl-num"), value = text,
                                    inputmode = "decimal"),
    shiny::tags$input(type = "text", class = base, value = text, list = dl))
}

# one row: its cells and its buttons (up, down when the rows move; remove)
.tbl_row <- function(spec, cells, dls, lang = "en") {
  t <- function(x) tr(x, lang)
  shiny::tags$tr(
    class = "rp-tbl-row",
    lapply(seq_along(spec$cols), function(j) {
      shiny::tags$td(class = paste0("rp-tbl-td rp-tbl-k-", spec$kinds[[j]]),
                     .tbl_cell(spec$kinds[[j]], cells[[j]] %||% "", dls[[j]], spec$choices[[j]], lang))
    }),
    shiny::tags$td(
      class = "rp-tbl-tools",
      if (isTRUE(spec$ordered)) shiny::tagList(
        shiny::tags$button(type = "button", class = "btn btn-link btn-sm rp-tbl-up", title = t("Move up"),
                           `aria-label` = t("Move up"), "\u2191"),
        shiny::tags$button(type = "button", class = "btn btn-link btn-sm rp-tbl-down", title = t("Move down"),
                           `aria-label` = t("Move down"), "\u2193")),
      shiny::tags$button(type = "button", class = "btn btn-link btn-sm rp-tbl-del", title = t("Remove the row"),
                         `aria-label` = t("Remove the row"), "\u00d7")))
}

# The table field: a header of the columns, the fixed rows (read only, on
# top), the rows of the value, a blank row's template and Add a row.  Its
# state as it is drawn rides in data-state (the input's first value is the
# same, read from the cells).
.field_table <- function(fid, spec, value, fixed_rows = NULL, lang = "en") {
  t <- function(x) tr(x, lang)
  n <- length(spec$cols)
  # (a column of text or R with choices: its datalist, offered as it is typed)
  dls <- lapply(seq_len(n), function(j) {
    if (spec$kinds[[j]] %in% c("text", "r") && length(spec$choices[[j]])) paste0(fid, "__dl", j)
  })
  lists <- lapply(seq_len(n), function(j) {
    if (is.null(dls[[j]])) return(NULL)
    shiny::tags$datalist(id = dls[[j]], lapply(unique(as.character(unlist(spec$choices[[j]]))),
                                               function(x) shiny::tags$option(value = x)))
  })
  rows <- .tbl_rows(spec, value)
  fixed <- .tbl_rows(spec, fixed_rows)
  st <- .tbl_state(spec, value)
  shiny::div(
    class = paste("rp-tbl", paste0("rp-tbl-", spec$shape)), id = fid,
    `data-shape` = spec$shape,
    `data-cols` = as.character(jsonlite::toJSON(spec$cols)),
    `data-kinds` = as.character(jsonlite::toJSON(spec$kinds)),
    `data-state` = as.character(jsonlite::toJSON(st)),
    shiny::tags$table(
      class = "table table-sm rp-tbl-table",
      shiny::tags$thead(shiny::tags$tr(
        lapply(seq_len(n), function(j) {
          shiny::tags$th(class = if (identical(spec$shape, "rows") || identical(spec$kinds[[j]], "r")) "rp-mono",
                         spec$heads[[j]])
        }),
        shiny::tags$th(class = "rp-tbl-tools"))),
      if (length(fixed)) shiny::tags$tbody(
        class = "rp-tbl-fixed",
        lapply(fixed, function(cells) {
          shiny::tags$tr(class = "rp-tbl-ro", title = t("From the template"),
                         lapply(seq_len(n), function(j) {
                           shiny::tags$td(class = paste("rp-tbl-td", if (spec$kinds[[j]] == "r") "rp-mono"),
                                          cells[[j]])
                         }),
                         shiny::tags$td(class = "rp-tbl-tools",
                                        shiny::tags$span(class = "rp-badge rp-badge-template",
                                                         t("From the template"))))
        })),
      shiny::tags$tbody(class = "rp-tbl-body",
                        lapply(rows, function(cells) .tbl_row(spec, cells, dls, lang)))),
    shiny::tags$template(class = "rp-tbl-tpl", .tbl_row(spec, rep("", n), dls, lang)),
    lists,
    shiny::tags$button(type = "button", class = "btn btn-link btn-sm rp-tbl-addrow",
                       paste0("+ ", t("Add a row"))),
    # (an optional column no row has: its cell's template and the button
    # that adds it to every row)
    lapply(seq_along(spec$extra), function(i) {
      x <- spec$extra[[i]]
      dl <- if (x$kind %in% c("text", "r") && length(x$choices)) paste0(fid, "__dlx", i)
      shiny::tagList(
        if (!is.null(dl)) shiny::tags$datalist(id = dl, lapply(unique(as.character(unlist(x$choices))),
                                                               function(v) shiny::tags$option(value = v))),
        shiny::tags$template(class = "rp-tbl-coltpl", `data-col` = x$col,
                             shiny::tags$td(class = paste0("rp-tbl-td rp-tbl-k-", x$kind),
                                            .tbl_cell(x$kind, "", dl, x$choices, lang))),
        shiny::tags$button(type = "button", class = "btn btn-link btn-sm rp-tbl-addcol",
                           `data-col` = x$col, `data-kind` = x$kind,
                           paste0("+ ", t("Add the column:"), " ", x$col)))
    }))
}
