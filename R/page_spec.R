# Step 3 on the YAML spec (#438, step B4 of the "YAML native" plan): a
# page's values read layer by layer from the `page:` maps -- the report's
# own (spec/outputs/<id>.yml), its style, the patterns it extends
# (spec/patterns/<name>.yml), the study's defaults (spec/defaults.yml) --
# each with where it comes from, and written into the one layer's file,
# only the key changed.  The sheets' view of that layer is made again from
# the spec, so the screens that read the sheets see the same.
#
# A target is a report's id, a pattern ("@<name>") or NA (Standard: the
# study's defaults).  The keys are rtfreporter's: a function, then its
# argument (rtf_page: paper_size), or a band and its line number
# (footer: '99': left), or tokens and a name.  `!unset` (tflspec::tfl_unset())
# takes away what a layer below says: a line or a token left out, a value
# none.

# ---- the fields: rtfreporter's functions and the arguments tflspec writes

# One card a function; its fields in the function's order (formals).  kind:
# choice, number, pt (half points shown as points), text, logical, inches
# (twips shown as inches, or fit).  `arg` is the YAML key.
.pgs_fields <- list(
  rtf_page = list(
    list(arg = "paper_size", label = "Paper", kind = "choice",
         choices = c("letter", "legal", "A4", "A3", "A5")),
    list(arg = "orientation", label = "Orientation", kind = "choice",
         choices = c("landscape", "portrait")),
    list(arg = "width_in", label = "Width (in)", kind = "number"),
    list(arg = "height_in", label = "Height (in)", kind = "number"),
    list(arg = "margin_top_in", label = "Top (in)", kind = "number"),
    list(arg = "margin_bottom_in", label = "Bottom (in)", kind = "number"),
    list(arg = "margin_left_in", label = "Left (in)", kind = "number"),
    list(arg = "margin_right_in", label = "Right (in)", kind = "number"),
    list(arg = "header_dist_in", label = "Header distance (in)", kind = "number"),
    list(arg = "footer_dist_in", label = "Footer distance (in)", kind = "number")),
  rtf_default_format = list(
    list(arg = "font_size_half_points", label = "Size (pt)", kind = "pt"),
    list(arg = "row_height_twips", label = "Row height (twips)", kind = "number"),
    list(arg = "cell_padding_left_twips", label = "Cell padding, left (twips)", kind = "number"),
    list(arg = "cell_padding_right_twips", label = "Cell padding, right (twips)", kind = "number"),
    list(arg = "markup", label = "Markup", kind = "choice",
         choices = c("script", "relational", "all", "none")),
    list(arg = "title_format", label = "Titles as", kind = "choice", choices = c("text", "table")),
    list(arg = "footnote_format", label = "Footnotes as", kind = "choice",
         choices = c("table", "text")),
    list(arg = "title_width", label = "Titles' width", kind = "text",
         choices = c("content", "page")),
    list(arg = "footnote_width", label = "Footnotes' width", kind = "text",
         choices = c("content", "page"))),
  rtf_document = list(
    list(arg = "font_table", label = "Font", kind = "font",
         choices = c("Courier", "Courier New")),
    list(arg = "watermark", label = "Watermark", kind = "text")),
  rtf_tables = list(
    list(arg = "auto_section", label = "Section from the table's name", kind = "logical"),
    list(arg = "section_label_align", label = "Section's alignment", kind = "choice",
         choices = c("left", "center", "right")),
    list(arg = "auto_title", label = "Title from the table's name", kind = "logical"),
    list(arg = "title_label_align", label = "Title's alignment", kind = "choice",
         choices = c("left", "center", "right"))),
  rtf_figures = list(
    list(arg = "width_twips", label = "Width (in)", kind = "inches"),
    list(arg = "height_twips", label = "Height (in)", kind = "inches")),
  rtf_titles = list(
    list(arg = "font_size_half_points", label = "Size (pt)", kind = "pt")),
  rtf_footnotes = list(
    list(arg = "font_size_half_points", label = "Size (pt)", kind = "pt")))

# a card's meaning, its title "meaning (function())"
.pgs_cards <- c(rtf_page = "Paper and margins", rtf_default_format = "Text defaults",
                rtf_document = "Document", rtf_tables = "Table on the page",
                rtf_figures = "Figure size")

# the call each card is in the program (tflspec writes rtf_page()'s
# arguments as rtf_document(page = list(...)))
.pgs_call_names <- c(rtf_page = "(rtf_document(page =))", rtf_default_format = "(rtf_default_format())",
                     rtf_document = "(rtf_document())", rtf_tables = "(rtf_tables())",
                     rtf_figures = "(rtf_figures())", rtf_titles = "(rtf_titles())",
                     rtf_footnotes = "(rtf_footnotes())")

.pgs_field <- function(fn, arg) {
  for (f in .pgs_fields[[fn]]) if (identical(f$arg, arg)) return(f)
  NULL
}

# ---- reading ---------------------------------------------------------------

# The spec with the view's edits in it, made when the planner has none
.pgs_spec <- function(p) {
  if (is.null(p)) return(NULL)
  sp <- tryCatch(.spec_now(.spec_ensure(p)), error = function(e) NULL)
  if (is.null(sp) || !inherits(sp$study, "tfl_study_spec")) NULL else sp
}

# The page maps a target's page is put together from, nearest first, named
# by where they are: own, style:<name>, pattern:<name>, defaults
.pgs_layers <- function(sp, target) {
  st <- sp$study
  if (is.na(target)) return(list(own = st$defaults$page %||% list()))
  if (.is_pattern_id(target)) {
    return(list(own = st$patterns[[substring(target, 2L)]]$page %||% list(),
                defaults = st$defaults$page %||% list()))
  }
  o <- sp$outputs[[target]]
  out <- list(own = o$page %||% list())
  sn <- o$style %||% "standard"
  if (is.character(sn) && length(sn) == 1L && !is.null(st$styles[[sn]])) {
    out[[paste0("style:", sn)]] <- st$styles[[sn]]$page %||% list()
  }
  chain <- o$extends
  if (identical(chain, "none")) chain <- character()
  for (nm in chain) {
    if (identical(nm, "defaults")) {
      out$defaults <- st$defaults$page %||% list()
    } else if (startsWith(nm, "patterns/")) {
      out[[paste0("pattern:", substring(nm, 10L))]] <- st$patterns[[substring(nm, 10L)]]$page %||% list()
    }
  }
  out
}

.pgs_unset <- function(v) inherits(v, "tfl_unset")

# A value at `path` (keys) in the layers, nearest first: list(value, from,
# unset).  A layer that unsets the key or a map above it says none.
.pgs_find <- function(layers, path) {
  for (k in names(layers)) {
    v <- layers[[k]]
    for (key in path) {
      if (.pgs_unset(v)) break
      v <- if (is.list(v)) v[[key]] else NULL
      if (is.null(v)) break
    }
    if (.pgs_unset(v)) return(list(value = NULL, from = k, unset = TRUE))
    if (!is.null(v)) return(list(value = v, from = k, unset = FALSE))
  }
  list(value = NULL, from = NA_character_, unset = FALSE)
}

# One argument of a target's page: the value and where it comes from, and
# what the layers below its own say (`below`)
.pgs_arg <- function(sp, target, fn, arg) {
  layers <- .pgs_layers(sp, target)
  now <- .pgs_find(layers, c(fn, arg))
  now$below <- .pgs_find(layers[-1L], c(fn, arg))
  now
}

# A value as the form shows it (text)
.pgs_show <- function(v, kind) {
  if (is.null(v)) return("")
  if (identical(kind, "font")) {
    v <- if (is.list(v) && length(v)) v[[1L]]$name %||% "" else as.character(v)
    return(as.character(v %||% ""))
  }
  if (identical(kind, "pt")) return(.points(as.character(v)))
  if (identical(kind, "inches")) {
    n <- suppressWarnings(as.numeric(v))
    return(if (is.na(n)) as.character(v) else format(round(n / 1440, 3)))
  }
  if (is.logical(v)) return(if (isTRUE(v)) "TRUE" else "FALSE")
  as.character(v)[1L]
}

# A text typed into a field as the YAML value (NULL: blank; "(none)":
# !unset) -- the sheets' cells as tflspec reads them: TRUE / FALSE,
# numbers, else the text
.pgs_value <- function(v, kind) {
  v <- trimws(v %||% "")
  if (!nzchar(v)) return(NULL)
  if (identical(v, "(none)")) return(tflspec::tfl_unset())
  if (identical(kind, "font")) return(list(list(name = v)))
  if (identical(kind, "pt")) {
    hp <- .half_points(v)
    if (is.na(hp)) stop("The size is a number of points (9, 10 ...).", call. = FALSE)
    return(as.integer(hp))
  }
  if (identical(kind, "inches")) {
    if (identical(tolower(v), "fit")) return("fit")
    n <- suppressWarnings(as.numeric(v))
    if (is.na(n) || n <= 0) stop("A size is inches (5.5) or fit.", call. = FALSE)
    return(as.integer(round(n * 1440)))
  }
  if (v %in% c("TRUE", "FALSE")) return(identical(v, "TRUE"))
  n <- suppressWarnings(as.numeric(v))
  if (!is.na(n) && grepl("^-?[0-9.]+$", v)) {
    return(if (n == round(n) && !grepl("[.]", v)) as.integer(n) else n)
  }
  v
}

# a map over a map, as tflspec's resolve puts the layers together
# (.merge_map(): by key; !unset takes the key away; maps recurse)
.pgs_merge <- function(base, over) {
  is_map <- function(x) is.list(x) && !.pgs_unset(x) && length(x) > 0L && !is.null(names(x))
  if (is.null(over) || (is.list(over) && !length(over) && !.pgs_unset(over))) return(base)
  if (!is_map(base) || !is_map(over)) return(over)
  for (k in names(over)) {
    v <- over[[k]]
    if (.pgs_unset(v)) base[k] <- list(v) else
      base[k] <- list(if (is_map(v) && is_map(base[[k]])) .pgs_merge(base[[k]], v) else v)
  }
  base
}

.pgs_text <- function(v) {
  if (is.null(v) || .pgs_unset(v)) return(NA_character_)
  as.character(v)[1L]
}

# A band's lines (header, footer, titles, footnotes) as a target's page
# prints them: line, left, center, right, from (the nearest layer with the
# line), omitted (a line left out: !unset), in the lines' order
.pgs_lines <- function(sp, target, band) {
  layers <- .pgs_layers(sp, target)
  keys <- unique(unlist(lapply(layers, function(l) {
    m <- l[[band]]
    if (is.list(m) && !.pgs_unset(m)) names(m)
  })))
  out <- data.frame(line = character(), left = character(), center = character(),
                    right = character(), from = character(), omitted = logical(),
                    stringsAsFactors = FALSE)
  for (ln in keys) {
    f <- .pgs_find(layers, c(band, ln))
    if (is.na(f$from)) next
    # (a band unset above the line: the line is not printed, nothing to say)
    if (f$unset && .pgs_unset(layers[[f$from]][[band]])) next
    cells <- .pgs_resolved_list(lapply(layers, function(l) {
      m <- l[[band]]
      if (.pgs_unset(m)) m else if (is.list(m)) m[[ln]]
    }))
    cell <- function(k) .pgs_text(if (is.list(cells) && !.pgs_unset(cells)) cells[[k]])
    out[nrow(out) + 1L, ] <- list(ln, cell("left"), cell("center"), cell("right"), f$from, f$unset)
  }
  n <- suppressWarnings(as.integer(out$line))
  out <- out[order(is.na(n), n, out$line), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# a line's values in the layers (nearest first) put together, as resolved
.pgs_resolved_list <- function(vals) {
  acc <- NULL
  for (v in rev(vals)) {
    acc <- if (.pgs_unset(v)) NULL else if (is.null(v)) acc else
      if (is.list(v) && !length(v)) list() else if (is.null(acc)) v else .pgs_merge(acc, v)
  }
  acc
}

# The tokens of a target's page: name, value, from, omitted
.pgs_tokens <- function(sp, target) {
  layers <- .pgs_layers(sp, target)
  keys <- unique(unlist(lapply(layers, function(l) {
    m <- l$tokens
    if (is.list(m) && !.pgs_unset(m)) names(m)
  })))
  out <- data.frame(name = character(), value = character(), from = character(),
                    omitted = logical(), stringsAsFactors = FALSE)
  for (k in keys) {
    f <- .pgs_find(layers, c("tokens", k))
    if (is.na(f$from)) next
    out[nrow(out) + 1L, ] <- list(k, .pgs_text(f$value), f$from, f$unset)
  }
  out
}

# Every page of a spec, and the reports' extends: what step 3's form is
# drawn from (a value that changes when they do)
.pgs_pages <- function(sp) {
  if (is.null(sp)) return(NULL)
  list(sp$study$defaults$page, lapply(sp$study$patterns, `[[`, "page"),
       lapply(sp$outputs, function(o) list(o$page, o$extends)))
}

# the patterns of the study (its spec/patterns/ files)
.pgs_patterns <- function(sp) sort(names(sp$study$patterns %||% list()))

# a report's pattern: the first patterns/<name> it extends (NA: Standard)
.pgs_pattern_of <- function(sp, id) {
  ch <- sp$outputs[[id]]$extends
  pat <- grep("^patterns/", ch, value = TRUE)
  if (length(pat)) substring(pat[1L], 10L) else NA_character_
}

# ---- writing ---------------------------------------------------------------

# a map with the value at `path` set (NULL: taken away); maps left empty on
# the way up go too (a line's empty map is a blank line: kept)
.pgs_put <- function(m, path, value) {
  if (!is.list(m) || .pgs_unset(m)) m <- list()
  k <- path[1L]
  if (length(path) == 1L) {
    if (is.null(value)) m[[k]] <- NULL else m[k] <- list(value)
    return(m)
  }
  sub <- .pgs_put(m[[k]], path[-1L], value)
  if (!length(sub)) m[[k]] <- NULL else m[k] <- list(sub)
  m
}

# A target's own page written: the report's file, the defaults' or a
# pattern's, and the view's rows of it made again
.pgs_write <- function(p, target, page) {
  p <- .spec_ensure(p)
  p <- .spec_sync(p, p$spec$meta)
  attr(p, "changed") <- NULL
  if (!length(page)) page <- NULL
  if (!is.na(target) && !.is_pattern_id(target)) {
    o <- p$spec$outputs[[target]]
    if (is.null(o)) stop("The spec has no report ", target, ".", call. = FALSE)
    if (identical(o$page, page)) return(p)
    o["page"] <- list(page)
    return(.spec_set_output(p, target, o, page_only = TRUE))
  }
  sp <- p$spec
  if (is.na(target)) {
    if (identical(sp$study$defaults$page, page)) return(p)
    sp$study$defaults["page"] <- list(page %||% list())
    p$spec <- sp
    return(.spec_set_study_part(p, "defaults"))
  }
  nm <- substring(target, 2L)
  if (is.null(sp$study$patterns[[nm]])) stop("No page pattern ", nm, ".", call. = FALSE)
  if (identical(sp$study$patterns[[nm]]$page, page)) return(p)
  sp$study$patterns[[nm]]["page"] <- list(page %||% list())
  p$spec <- sp
  .spec_set_study_part(p, "patterns", nm)
}

# One study part's file written from the spec (tflspec::tfl_write_study_part()),
# and the view's study rows (blank and @<pattern>) of the report sheets
# made again from it
.spec_set_study_part <- function(p, part, name = NULL) {
  sp <- p$spec
  rel <- if (identical(part, "patterns")) {
    file.path(study_layout()[["spec"]], "patterns", paste0(name, ".yml"))
  } else file.path(study_layout()[["spec"]], paste0(part, ".yml"))
  f <- tempfile("tflplanner-part", fileext = ".yml")
  on.exit(unlink(f), add = TRUE)
  tflspec::tfl_write_study_part(sp$study, part, f, name = name)
  sp$text[[rel]] <- readLines(f, encoding = "UTF-8", warn = FALSE)
  sp$text <- sp$text[sort(names(sp$text), method = "radix")]
  p$spec <- sp
  .spec_view_study_page(p)
}

# The view's study rows of the report sheets (the defaults' page: blank
# output_id; a pattern's: @<name>) made again from the spec
.spec_view_study_page <- function(p) {
  st <- p$spec$study
  lean <- st
  if (!is.null(lean$defaults)) lean$defaults["content"] <- list(NULL)
  lean$patterns <- lapply(lean$patterns, function(l) { l["content"] <- list(NULL); l })
  lean$styles <- list()
  v <- .planner_from_spec(lean, list())
  no_id <- function(d) { d$output_id <- NULL; rownames(d) <- NULL; d }
  ids <- c(NA_character_, paste0("@", names(st$patterns)))
  for (s in report_sheets()) {
    have <- unique(p$sheets[[s]]$output_id)
    ids_s <- union(ids, have[.is_pattern_id(have)])
    for (id in ids_s) {
      new <- sheet_rows(v, s, id)
      old <- sheet_rows(p, s, id)
      if (!identical(no_id(old), no_id(new))) p <- set_sheet_rows(p, s, id, no_id(new))
    }
  }
  if (!is.null(p$spec$base)) p$spec$base <- .planner_body(p)
  p
}

# A target's argument set: `value` the YAML value (NULL: back to the
# layers below; !unset: none).  A value the layers below say already is
# no difference: the key goes instead.
.pgs_set_arg <- function(p, target, fn, arg, value) {
  sp <- .pgs_spec(p)
  layers <- .pgs_layers(sp, target)
  below <- .pgs_find(layers[-1L], c(fn, arg))
  if (!is.null(value)) {
    if (.pgs_unset(value) && is.null(below$value)) value <- NULL
    else if (!.pgs_unset(value) && !is.null(below$value) && !below$unset &&
             identical(.pgs_show(value, "text"), .pgs_show(below$value, "text"))) value <- NULL
  }
  page <- .pgs_put(layers$own, c(fn, arg), value)
  .pgs_write(p, target, page)
}

# A target's own line of a band: its cells (NA or blank: none, a cell the
# layers below say taken away), left out (`omit`), or gone (`drop`: back to
# the layers' line of that number)
.pgs_set_line <- function(p, target, band, line, left = NA, center = NA, right = NA,
                          omit = FALSE, drop = FALSE) {
  sp <- .pgs_spec(p)
  layers <- .pgs_layers(sp, target)
  line <- as.character(line)
  value <- if (drop) NULL else if (omit) tflspec::tfl_unset() else {
    below <- .pgs_resolved_list(lapply(layers[-1L], function(l) {
      m <- l[[band]]
      if (.pgs_unset(m)) m else if (is.list(m)) m[[line]]
    }))
    parts <- list(left = left, center = center, right = right)
    if (any(vapply(parts, function(v) identical(trimws(v %||% ""), "(none)"), NA))) {
      tflspec::tfl_unset()
    } else {
      cells <- list()
      for (k in names(parts)) {
        v <- parts[[k]]
        if (!is.null(v) && !is.na(v) && nzchar(v)) {
          cells[[k]] <- gsub("\r\n", "\n", v, fixed = TRUE)
        } else if (is.list(below) && !is.null(below[[k]]) && !.pgs_unset(below[[k]])) {
          cells[k] <- list(tflspec::tfl_unset())
        }
      }
      cells
    }
  }
  own <- layers$own
  m <- own[[band]]
  if (!is.list(m) || .pgs_unset(m)) m <- list()
  if (is.null(value)) m[[line]] <- NULL else m[line] <- list(value)
  own <- if (length(m)) {
    o2 <- own
    o2[band] <- list(m)
    o2
  } else {
    own[[band]] <- NULL
    own
  }
  .pgs_write(p, target, own)
}

# A target's own token (NULL: back to the layers'; !unset: left out)
.pgs_set_token <- function(p, target, name, value) {
  sp <- .pgs_spec(p)
  page <- .pgs_put(.pgs_layers(sp, target)$own, c("tokens", name), value)
  .pgs_write(p, target, page)
}

# ---- extends and the patterns ---------------------------------------------

# A report's page pattern (NA or "Standard": none): its `extends:` with the
# pattern put first, in place of the one it had
.pgs_set_pattern <- function(p, id, pattern) {
  if (!is.na(pattern) && identical(pattern, "Standard")) pattern <- NA_character_
  sp <- .pgs_spec(p)
  o <- sp$outputs[[id]]
  if (is.null(o)) stop("The spec has no report ", id, ".", call. = FALSE)
  if (!is.na(pattern) && !pattern %in% .pgs_patterns(sp)) {
    stop("No page pattern ", pattern, ".", call. = FALSE)
  }
  ch <- o$extends
  if (identical(ch, "none")) ch <- character()
  had <- grep("^patterns/", ch)
  rest <- if (length(had)) ch[-had[1L]] else ch
  new <- c(if (!is.na(pattern)) paste0("patterns/", pattern), rest)
  if (!length(new)) new <- if (identical(o$extends, "none")) "none" else character()
  if (identical(as.character(o$extends %||% character()), new)) return(p)
  o["extends"] <- list(if (length(new)) new)
  p <- .spec_ensure(p)
  p <- .spec_sync(p, p$spec$meta)
  .spec_set_output(p, id, o, page_only = TRUE)
}

# The reports whose extends names a pattern
.pgs_pattern_reports <- function(sp, name) {
  ids <- names(sp$outputs)
  ids[vapply(sp$outputs, function(o) paste0("patterns/", name) %in% o$extends, NA)]
}

# A new pattern: Standard's page, no difference (an empty file)
.pgs_add_pattern <- function(p, name) {
  p <- .spec_ensure(p)
  p <- .spec_sync(p, p$spec$meta)
  sp <- p$spec
  name <- trimws(sub("^@", "", name %||% ""))
  if (!grepl(.pattern_rx, name)) {
    stop("A page pattern's name is a letter, then letters, digits or _ (Compact): not '",
         name, "'.", call. = FALSE)
  }
  if (identical(tolower(name), "standard")) {
    stop("Standard is the study's defaults: give the pattern another name.", call. = FALSE)
  }
  if (name %in% .pgs_patterns(sp)) stop("There is a page pattern '", name, "' already.", call. = FALSE)
  sp$study$patterns[[name]] <- list(data = list(), codelists = list(), content = list(),
                                    page = list())
  p$spec <- sp
  p <- .spec_set_study_part(p, "patterns", name)
  # (the view: the pattern's empty report row, as the sheets have it)
  if (!name %in% page_patterns(p)) {
    p <- add_page_pattern(p, name)
    if (!is.null(p$spec$base)) p$spec$base <- .planner_body(p)
  }
  p
}

# A pattern renamed: its file and the reports that extend it
.pgs_rename_pattern <- function(p, name, new) {
  p <- .spec_ensure(p)
  p <- .spec_sync(p, p$spec$meta)
  sp <- p$spec
  new <- trimws(sub("^@", "", new %||% ""))
  if (!name %in% .pgs_patterns(sp)) stop("No page pattern ", name, ".", call. = FALSE)
  if (!grepl(.pattern_rx, new) || identical(tolower(new), "standard")) {
    stop("A page pattern's name is a letter, then letters, digits or _ (Compact): not '",
         new, "'.", call. = FALSE)
  }
  if (new %in% .pgs_patterns(sp)) stop("There is a page pattern '", new, "' already.", call. = FALSE)
  used <- .pgs_pattern_reports(sp, name)
  pats <- sp$study$patterns
  names(pats)[names(pats) == name] <- new
  sp$study$patterns <- pats[sort(names(pats), method = "radix")]
  dir <- file.path(study_layout()[["spec"]], "patterns")
  sp$text[[file.path(dir, paste0(name, ".yml"))]] <- NULL
  p$spec <- sp
  p <- .spec_set_study_part(p, "patterns", new)
  for (id in used) {
    o <- p$spec$outputs[[id]]
    o$extends[o$extends == paste0("patterns/", name)] <- paste0("patterns/", new)
    p <- .spec_set_output(p, id, o, page_only = TRUE)
  }
  # (the view: its rows of every sheet under the new name)
  if (name %in% page_patterns(p)) p <- rename_page_pattern(p, name, new)
  p <- .spec_view_study_page(p)
  if (!is.null(p$spec$base)) p$spec$base <- .planner_body(p)
  p
}

# A pattern no report extends taken away (its file goes on save)
.pgs_remove_pattern <- function(p, name) {
  p <- .spec_ensure(p)
  p <- .spec_sync(p, p$spec$meta)
  sp <- p$spec
  used <- .pgs_pattern_reports(sp, name)
  if (length(used)) {
    stop("The page pattern '", name, "' is used by ", paste(used, collapse = ", "),
         ": choose another for them first.", call. = FALSE)
  }
  sp$study$patterns[[name]] <- NULL
  sp$text[[file.path(study_layout()[["spec"]], "patterns", paste0(name, ".yml"))]] <- NULL
  p$spec <- sp
  if (name %in% page_patterns(p)) p <- remove_page_pattern(p, name)
  p <- .spec_view_study_page(p)
  if (!is.null(p$spec$base)) p$spec$base <- .planner_body(p)
  p
}
