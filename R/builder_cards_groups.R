# Field groups, the folded arguments, the source badge, code jump, nav
# hints and the "add an item" input.
# Split from R/builder_cards.R by section; no code changed.

# ---- groups and the folded arguments -------------------------------------------

# Several fields on one row: the schema's `groups:` (a label, the arguments,
# `as` -- how they are laid out, a word a field: "min max by").
#' @noRd
arg_group <- function(label = NULL, fields, as = NULL, lang = "en") {
  caps <- if (!is.null(as)) strsplit(trimws(as), "[[:space:]]+")[[1L]]
  fields <- lapply(seq_along(fields), function(i) {
    f <- fields[[i]]
    if (!is.null(caps) && length(caps) == length(fields)) {
      shiny::div(class = "rp-group-cell", `data-as` = caps[[i]], f)
    } else {
      shiny::div(class = "rp-group-cell", f)
    }
  })
  shiny::div(class = "rp-group", `data-as` = as,
             if (!is.null(label)) shiny::div(class = "rp-group-label", .label_text(label, lang)),
             shiny::div(class = "rp-group-row", fields))
}

# The arguments not shown: the function's formals less the visible ones
# and the ones in use, folded under "Other arguments (n)" -- a search
# field, the names in the function's order, each a field labelled
# "meaning (name)" (the name alone when no meaning is known), its (i)
# what the argument means (.arg_help()) or a link to the package's help.
# A field given a value stays shown
# when the fold closes (it is promoted; the next render passes it as used).
#' @noRd
arg_more <- function(id, fn, used = character(), schema = NULL, lang = "en",
                     values = list(), choices = NULL, rest = NULL, fields = NULL,
                     label = "Other arguments") {
  t <- function(x) tr(x, lang)
  schema <- schema %||% .schema_of(fn)
  all <- .fn_formals(fn, schema)
  # (in the function's order, as its formals; `rest` and their `fields`
  # given: those)
  rest <- rest %||% setdiff(all, c(schema$visible, used, "..."))
  if (!length(rest)) return(NULL)
  pkg <- schema$package %||% if (grepl("::", fn, fixed = TRUE)) sub("::.*$", "", fn)
  fields <- fields %||% lapply(rest, function(a) {
    help <- .arg_help(fn, a, schema, lang)
    link <- if (is.null(help) && !is.null(pkg)) {
      shiny::tags$a(class = "rp-help-link", href = .help_url(pkg, fn), target = "_blank",
                    rel = "noopener", title = t("The package's help"),
                    paste0("?", pkg, "::", sub("^.*::", "", fn)))
    }
    arg_field(id, a, schema$args[[a]]$type, label = .arg_meaning(fn, a), value = values[[a]],
              default = schema$args[[a]]$default, help = help, help_link = link,
              choices = choices, lang = lang)
  })
  shiny::div(
    class = "rp-more", `data-card` = id,
    shiny::tags$button(type = "button", class = "rp-more-toggle",
                       shiny::tags$span(class = "rp-more-arrow", "\u25b8"), " ",
                       sprintf("%s (%d)", t(label), length(rest))),
    shiny::div(class = "rp-more-body",
               shiny::tags$input(type = "search", class = "form-control form-control-sm rp-more-search",
                                 id = paste0(id, "__more_q"), placeholder = t("Search arguments")),
               fields))
}

# the function's arguments: the schema's `formals`, else the installed
# function's, else the schema's `args`
.fn_formals <- function(fn, schema = NULL) {
  f <- schema$formals
  if (!is.null(f)) return(if (is.list(f)) names(f) else as.character(f))
  pkg <- if (grepl("::", fn, fixed = TRUE)) sub("::.*$", "", fn) else schema$package
  nm <- sub("^.*::", "", fn)
  if (!is.null(pkg) && requireNamespace(pkg, quietly = TRUE)) {
    ns <- asNamespace(pkg)
    if (exists(nm, envir = ns, inherits = FALSE)) {
      fo <- formals(get(nm, envir = ns))
      if (!is.null(fo)) return(names(fo))
    }
  }
  names(schema$args) %||% character()
}

.help_url <- function(pkg, fn) {
  sprintf("https://rdrr.io/cran/%s/man/%s.html", pkg, sub("^.*::", "", fn))
}

# The arguments `args` in the order of the function's arguments
# (`formals`: .fn_formals()'s), those not among them after, as they came.
# Every card's fields are in this order.
.arg_order <- function(args, formals) {
  args[order(match(args, formals, nomatch = length(formals) + 1L), seq_along(args))]
}

# An argument's help in the (i): what it means, when it is used, an
# example -- tflplanner's own text (.plan_arg_help), else the schema's,
# else tflspec's -- in the language; NULL for none
.arg_help <- function(fn, arg, schema = NULL, lang = "en") {
  own <- .plan_arg_help[[fn]]
  h <- if (!is.null(own) && arg %in% names(own)) own[[arg]]
  h <- h %||% schema$args[[arg]]$description %||% .schema_help(fn, arg)
  if (is.null(h)) NULL else tr(h, lang)
}

# the schema of a function, when tflspec has one
.schema_of <- function(fn) {
  if (is.null(fn) || !requireNamespace("tflspec", quietly = TRUE)) return(NULL)
  tryCatch(tflspec::tfl_schema(fn), error = function(e) NULL)
}

# an argument's help text, NULL for none
.schema_help <- function(fn, arg) {
  if (!requireNamespace("tflspec", quietly = TRUE)) return(NULL)
  h <- tryCatch(tflspec::tfl_schema_help(fn, arg), error = function(e) NA_character_)
  if (is.null(h) || length(h) != 1L || is.na(h) || !nzchar(h)) NULL else h
}

# ---- where a value came from ---------------------------------------------------

# The mark of an item's or a value's source: own has none; `defaults` the
# study's defaults, `standards` the company standards, `pattern:<name>` a
# pattern, `template:<name>` a template, `ard` taken from the ARD (changed,
# it is the report's own, and Revert takes it back to the ARD's).
#' @noRd
from_badge <- function(from, lang = "en", own = NULL) {
  if (is.null(from) || !length(from) || !nzchar(from)) return(NULL)
  t <- function(x) tr(x, lang)
  # the report's own: its mark when the caller names it ("This table"),
  # none otherwise
  if (identical(from, "own")) {
    if (is.null(own)) return(NULL)
    return(shiny::tags$span(class = "rp-badge rp-badge-own", `data-from` = "own", t(own)))
  }
  kind <- sub(":.*$", "", from)
  name <- if (grepl(":", from, fixed = TRUE)) sub("^[^:]*:", "", from) else NULL
  text <- switch(kind,
                 defaults = t("Study standard"),
                 standards = t("Company standard"),
                 pattern = paste(t("Pattern"), name),
                 template = t("Template"),
                 ard = t("From the ARD"),
                 from)
  shiny::tags$span(class = paste("rp-badge", paste0("rp-badge-", kind)), `data-from` = from,
                   title = if (!is.null(name)) name else NULL, text)
}

# ---- Code line <-> card -----------------------------------------------------------

# The table of the code's lines to the items that wrote them (what
# tfl_content_code() returns: a line number -> an item's path, "<bundle>/
# <n>" or an id), for the Code tab's view `output_id`: a click on a line
# selects and opens its card, a card's field in focus lights its lines.
# `lines_map`: a data frame (`line`, `path`), or a vector of paths named
# by line.
#' @noRd
code_jump <- function(lines_map, output_id) {
  if (is.data.frame(lines_map)) {
    m <- stats::setNames(as.list(as.character(lines_map$path)), as.character(lines_map$line))
  } else {
    m <- as.list(stats::setNames(as.character(lines_map), names(lines_map)))
  }
  shiny::tags$script(type = "application/json", class = "rp-code-jump", `data-for` = output_id,
                     shiny::HTML(jsonlite::toJSON(m, auto_unbox = TRUE)))
}

# ---- nav mode -----------------------------------------------------------------------

# A sentence of the guided (nav) mode: in the HTML in both modes, shown
# only when <html> has the nav class (no redraw).  At a group's head, or
# under a field.
#' @noRd
nav_hint <- function(text, where = c("group", "field"), lang = "en") {
  where <- match.arg(where)
  if (is.null(text) || !length(text)) return(NULL)
  if (is.character(text)) text <- tr(text, lang)
  shiny::div(class = paste("rp-nav-hint", paste0("rp-nav-", where)), text)
}

# ---- adding an item --------------------------------------------------------------------

# The "add an item" field: a selectize of the schema's functions, narrowed
# to `kinds` (a figure entry's `figure:`, or "plan" for a table's verbs),
# each labelled "meaning (fn)", grouped by kind; `pkg::fn` typed is taken
# as it is.  Enter adds (the button `<id>__add`), and the first field of
# the new card gets the focus.
#' @noRd
add_item_input <- function(id, kinds = NULL, schema = NULL, lang = "en", placeholder = NULL) {
  t <- function(x) tr(x, lang)
  schema <- schema %||% (if (requireNamespace("tflspec", quietly = TRUE)) {
    tryCatch(tflspec::tfl_schema(), error = function(e) NULL)
  })
  entries <- .item_choices(schema, kinds, lang)
  shiny::div(
    class = "rp-add-item", `data-add` = id,
    shiny::selectizeInput(
      id, NULL, choices = c(stats::setNames("", ""), entries), selected = "", width = "100%",
      options = list(create = TRUE, createOnBlur = FALSE, maxItems = 1,
                     placeholder = placeholder %||% t("Add an item (type a function name)"),
                     optgroupField = "kind", labelField = "label", searchField = list("label", "value"))),
    shiny::tags$button(type = "button", id = paste0(id, "__add"),
                       class = "btn btn-sm btn-outline-secondary action-button rp-add-btn", t("Add")))
}

# the functions offered: a named vector (value fn, name "meaning (fn)"),
# in the schema's rank
.item_choices <- function(schema, kinds = NULL, lang = "en") {
  if (!length(schema)) return(character())
  nms <- names(schema)
  keep <- vapply(nms, function(n) {
    e <- schema[[n]]
    if (is.null(kinds)) return(TRUE)
    if ("plan" %in% kinds && grepl("^(table_plan$|plan_)", n)) return(TRUE)
    fig <- e$figure
    !is.null(fig) && any(fig %in% kinds)
  }, NA)
  nms <- nms[keep]
  if (!length(nms)) return(character())
  rank <- vapply(nms, function(n) as.numeric(schema[[n]]$rank %||% 9999), 0)
  nms <- nms[order(rank, nms)]
  labels <- vapply(nms, function(n) {
    m <- .label_text(schema[[n]]$label, lang)
    if (is.null(m)) paste0(n, "()") else paste0(m, " (", n, "())")
  }, "")
  stats::setNames(nms, labels)
}
