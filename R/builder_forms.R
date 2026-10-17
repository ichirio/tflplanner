# The forms of the cards whose arguments are named by the user -- a
# function's `...`: plan_compare() (a comparison column a name and its
# template), plan_stat_groups() (a group a name and its statistics) and
# plan_nest() (a variable and the level of another it goes under).  Each
# has a body (its fields) and a reader (the fields back as the item's
# whole arguments, NULL while they are not drawn).  What a form read says
# is written as the report's own when it differs from the item
# (.dots_form_args()).

# the verbs with such a form
.plan_form_dots <- c("plan_compare", "plan_stat_groups", "plan_nest")

# an item's arguments of its `...` (the names the user gives): those not
# of the function's own
.dots_args <- function(fn, args) {
  sch <- .plan_form_schema(fn)
  args[setdiff(names(args) %||% character(), sch$formals)]
}

# The item's whole arguments after the form's edit: the form's reading
# (its `...` as the user names them), else the item's as they are
.dots_form_args <- function(read, it, fn) {
  if (is.null(read)) return(it$args %||% list())
  read
}

# a form's reading without the mark it is carried with (attribute `dots`)
.args_plain_attr <- function(x) {
  attr(x, "dots") <- NULL
  x
}

# the label of a field, as the other fields' ("meaning (name)"), with the
# mark of where the value came from
.dots_label <- function(fn, arg, it, lang = "en", help = NULL) {
  shiny::tags$label(class = "rp-field-label",
                    builder_label(.arg_meaning(fn, arg), arg, "arg", lang),
                    from_badge(if (length(it$args)) .arg_from(it, arg) else "own", lang),
                    help_tip(help))
}

# a template as text in a field; a template of more than one text (a map
# by context, guards, cell_rows()) is R, shown as it is and kept
.template_text <- function(v) {
  if (is.character(v) && length(v) == 1L && is.null(names(v))) v else NULL
}

# ---- plan_compare() ---------------------------------------------------------------

# The columns offered: the item's own (by name), then each statistic of
# the ARD's comparison records (`comparisons`: context -> statistics,
# ard_meta()), labelled with the analyses that have it
.compare_choices <- function(cols = character(), comparisons = list()) {
  st <- setdiff(as.character(unique(unlist(comparisons, use.names = FALSE))), cols)
  lab <- vapply(st, function(s) {
    ctx <- names(comparisons)[vapply(comparisons, function(z) s %in% z, NA)]
    paste0(s, " (", paste(ctx, collapse = ", "), ")")
  }, "", USE.NAMES = FALSE)
  c(stats::setNames(cols, cols), stats::setNames(st, lab))
}

# The plan_compare() card's body: its columns (a choice of the ARD's
# comparison statistics, or a name typed), each column's template, the
# group compared (`by`) and the place of the columns.  `by_choices`: the
# column keys.  The columns drawn are `cols` (the order of the
# templates' fields, read back by .compare_read()).
.compare_body <- function(cid, it, comparisons = list(), by_choices = NULL, lang = "en") {
  t <- function(x) tr(x, lang)
  fn <- "plan_compare"
  sch <- .plan_form_schema(fn)
  a <- it$args %||% list()
  dots <- .dots_args(fn, a)
  cols <- names(dots)
  rows <- lapply(seq_along(cols), function(i) {
    txt <- .template_text(dots[[i]])
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$code(cols[[i]])),
      shiny::tags$td(
        if (!is.null(txt)) shiny::tags$input(
          type = "text", id = paste0(cid, "__tpl__", i),
          class = "form-control form-control-sm rp-mono", value = txt, spellcheck = "false")
        else shiny::tagList(shiny::tags$code(class = "rp-mono", .arg_to_r(dots[[i]])),
                            shiny::div(class = "small text-muted", t("As R: Edit as R changes it.")))))
  })
  st <- unique(unlist(comparisons, use.names = FALSE))
  shiny::tagList(
    shiny::div(
      class = "rp-field rp-field-dots", `data-arg` = "...",
      .dots_label(fn, "...", it, lang,
                  help = t("A comparison column a name: a statistic of the ARD's comparison records (a test's p.value, a difference's estimate ...), or a name typed. Each column's text is its template.")),
      shiny::selectizeInput(paste0(cid, "__cols"), NULL,
                            choices = .compare_choices(cols, comparisons), selected = cols,
                            multiple = TRUE, width = "100%",
                            options = list(create = TRUE, plugins = list("remove_button"),
                                           placeholder = t("Choose or type a column's name"))),
      if (!length(st)) shiny::div(class = "small text-warning",
                                  t("The ARD has no comparison records (a test, a difference): add one in step 1, then read the ARD again.")),
      if (length(rows)) shiny::tags$table(
        class = "rp-widths rp-dots-table",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th(t("Column")),
                                         shiny::tags$th(t("Template")))),
        shiny::tags$tbody(rows))),
    arg_group(NULL, list(
      arg_field(cid, "by", sch$args$by$type, label = .arg_meaning(fn, "by"), value = a$by,
                choices = by_choices, from = if (!is.null(a$by)) .arg_from(it, "by") else "own",
                help = .schema_help(fn, "by"), lang = lang),
      arg_field(cid, "position", sch$args$position$type, label = .arg_meaning(fn, "position"),
                value = a$position, default = "last",
                from = if (!is.null(a$position)) .arg_from(it, "position") else "own",
                help = .schema_help(fn, "position"), lang = lang)), lang = lang))
}

# ... read back: the columns chosen in their order, each its template (as
# typed; a new column `{name}`; one of R as it was), then by and position.
# `drawn`: the item's columns as drawn (the templates' fields' order), and
# their values; NULL while the fields are not there.
.compare_read <- function(input, cid, drawn = list()) {
  cols <- .input_value(input, paste0(cid, "__cols"))
  pos <- .input_value(input, paste0(cid, "__position"))
  if (is.null(cols) && is.null(pos)) return(NULL)
  cols <- as.character(cols %||% character())
  cols <- unique(trimws(cols[nzchar(trimws(cols))]))
  out <- list()
  dn <- names(drawn) %||% character()
  for (k in cols) {
    i <- match(k, dn)
    v <- if (!is.na(i)) {
      txt <- .input_value(input, paste0(cid, "__tpl__", i))
      if (!is.null(txt) && nzchar(trimws(txt))) trimws(txt) else drawn[[i]]
    } else paste0("{", k, "}")
    out[[k]] <- v
  }
  sch <- .plan_form_schema("plan_compare")
  rest <- builder_card_read(input, cid, sch, args = c("by", "position"))
  c(out, rest)
}

# ---- plan_stat_groups() -------------------------------------------------------------

# The plan_stat_groups() card's body: a group a row -- its name and its
# statistics (a choice of the ARD's) -- and one more row for a new group;
# the variables whose records are split (.variables) and the group of the
# others (.others).  `stats`: the ARD's statistics; `variables`: its
# analysis variables.
.stat_groups_body <- function(cid, it, stats = NULL, variables = NULL, lang = "en") {
  t <- function(x) tr(x, lang)
  fn <- "plan_stat_groups"
  sch <- .plan_form_schema(fn)
  a <- it$args %||% list()
  g <- .dots_args(fn, a)
  nm <- c(names(g), "")
  rows <- lapply(seq_along(nm), function(i) {
    v <- if (i <= length(g)) as.character(unlist(g[[i]], use.names = FALSE)) else character()
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$input(
        type = "text", id = paste0(cid, "__g__", i, "__name"),
        class = "form-control form-control-sm rp-mono", value = nm[[i]],
        placeholder = if (i > length(g)) t("A new group"), spellcheck = "false",
        style = "width: 9em;")),
      shiny::tags$td(.field_select(paste0(cid, "__g__", i, "__stats"),
                                   .choices_with(stats::setNames(stats %||% character(), stats %||% character()), v),
                                   v, multiple = TRUE, lang = lang)))
  })
  shiny::tagList(
    shiny::div(
      class = "rp-field rp-field-dots", `data-arg` = "...",
      .dots_label(fn, "...", it, lang,
                  help = t("A group a row: its name (a key of .stat_group, as table_plan()'s cols or rows name it, and plan_cells()' template's) and its statistics. A statistic in no group is not read.")),
      shiny::tags$table(
        class = "rp-widths rp-dots-table",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th(t("Group name")),
                                         shiny::tags$th(t("Statistics")))),
        shiny::tags$tbody(rows))),
    arg_group(NULL, list(
      arg_field(cid, ".variables", sch$args$.variables$type, label = .arg_meaning(fn, ".variables"),
                value = a$.variables, choices = variables,
                from = if (!is.null(a$.variables)) .arg_from(it, ".variables") else "own",
                help = .schema_help(fn, ".variables"), lang = lang),
      arg_field(cid, ".others", sch$args$.others$type, label = .arg_meaning(fn, ".others"),
                value = a$.others,
                from = if (!is.null(a$.others)) .arg_from(it, ".others") else "own",
                help = .schema_help(fn, ".others"), lang = lang)), lang = lang))
}

# ... read back: the groups with a name and a statistic, in their order,
# then .variables and .others.  `n`: the groups drawn (the new row is n +
# 1); NULL while the fields are not there.
.stat_groups_read <- function(input, cid, n = 0L) {
  first <- .input_value(input, paste0(cid, "__g__1__name"))
  if (is.null(first)) return(NULL)
  out <- list()
  for (i in seq_len(n + 1L)) {
    nm <- .input_value(input, paste0(cid, "__g__", i, "__name"))
    st <- .input_value(input, paste0(cid, "__g__", i, "__stats"))
    if (is.null(nm) || !nzchar(trimws(nm))) next
    st <- as.character(st %||% character())
    st <- st[nzchar(st)]
    if (!length(st)) next
    out[[trimws(nm)]] <- st
  }
  sch <- .plan_form_schema("plan_stat_groups")
  c(out, builder_card_read(input, cid, sch, args = c(".variables", ".others")))
}

# ---- plan_nest() ------------------------------------------------------------------

# the choices of where a variable's rows go: its own block, or under a
# level of another categorical variable ("RACE: Asian")
.nest_choices <- function(var, st, cur = NULL, lang = "en") {
  v <- st$variables
  others <- v$variable[v$kind == "categorical" & v$variable != var]
  ch <- unlist(lapply(others, function(o) paste0(o, ": ", st$levels[[o]])), use.names = FALSE)
  c(stats::setNames("", tr("(its own block)", lang)), unique(c(ch, cur)))
}

# a nest's value ("RACE: Asian") as plan_nest() takes it (RACE = "Asian"),
# and back
.nest_value <- function(txt) {
  if (is.null(txt) || !length(txt) || is.na(txt) || !nzchar(trimws(txt))) return(NULL)
  at <- regexpr(": ", txt, fixed = TRUE)
  if (at < 1L) return(NULL)
  stats::setNames(list(substring(txt, at + 2L)), trimws(substr(txt, 1L, at - 1L)))
}
.nest_text <- function(v) {
  if (is.null(v) || !length(v) || is.null(names(v))) return(NULL)
  paste0(names(v)[[1L]], ": ", as.character(unlist(v, use.names = FALSE))[[1L]])
}

# The plan_nest() card's body: each categorical variable a row, the level
# of another variable its rows go under (blank: its own block).  `st`:
# builder_read()'s (its variables and levels).  The variables drawn are
# its categorical ones, in order (.nest_vars()).
.nest_vars <- function(st) {
  v <- st$variables
  if (!is.data.frame(v) || !nrow(v)) return(character())
  v$variable[v$kind == "categorical"]
}

.nest_body <- function(cid, it, st, lang = "en") {
  t <- function(x) tr(x, lang)
  a <- .dots_args("plan_nest", it$args %||% list())
  vars <- .nest_vars(st)
  if (!length(vars)) {
    return(shiny::div(class = "small text-muted", t("No categorical variable to nest.")))
  }
  rows <- lapply(seq_along(vars), function(i) {
    cur <- .nest_text(a[[vars[[i]]]])
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$code(vars[[i]])),
      shiny::tags$td(shiny::selectInput(paste0(cid, "__nest__", i), NULL,
                                        choices = .nest_choices(vars[[i]], st, cur, lang),
                                        selected = cur %||% "", width = "100%")))
  })
  shiny::div(
    class = "rp-field rp-field-dots", `data-arg` = "...",
    .dots_label("plan_nest", "...", it, lang,
                help = t("Its rows go right after that level's row, one step deeper, without its own heading: Chinese / Japanese / Korean under Race's Asian. Blank: a block of its own.")),
    shiny::tags$table(
      class = "rp-widths rp-dots-table",
      shiny::tags$thead(shiny::tags$tr(shiny::tags$th(t("Variable")),
                                       shiny::tags$th(t("Rows under a level of another variable")))),
      shiny::tags$tbody(rows)))
}

# ... read back: a variable under a level (VAR = list(RACE = "Asian")),
# the item's nests of variables not drawn kept; NULL while the fields
# are not there
.nest_read <- function(input, cid, vars, it = NULL) {
  if (!length(vars) || is.null(.input_value(input, paste0(cid, "__nest__1")))) return(NULL)
  a <- .dots_args("plan_nest", it$args %||% list())
  out <- a[setdiff(names(a), vars)]
  for (i in seq_along(vars)) {
    v <- .nest_value(.input_value(input, paste0(cid, "__nest__", i)))
    if (!is.null(v)) out[[vars[[i]]]] <- v
  }
  out
}
