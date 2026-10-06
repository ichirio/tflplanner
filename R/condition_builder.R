# A condition built from rows -- a variable, an operator, its values --
# instead of written as R: for the analysis data's and the analysis sets'
# `where`.  Rows are ANDed; groups of rows ("or") are ORed.  It writes the
# R the definition keeps (SAFFL == "Y" & PARAMCD %in% c("ALT", "AST")) and
# reads it back; what it cannot read stays as R, in a field of its own.
# The values offered are the data's, with their counts, searched as the
# function search searches (case, full / half width, kana).  The variables
# are offered in groups, as ADaM names them: the population flags first
# (SAFFL, ITTFL ...), then the analysis flags, the treatment, the
# parameter, the timing, and the others.  A flag's blank is a value too:
# "(blank)", written as NA and "" (a blank is either, as the data was read).
#
# The rows: a data frame, one row a condition: `group` (1, 2, ... the
# "or" groups), `var`, `op` (one of .cond_ops), `values` (a list column of
# character vectors), `type` ("chr", "num", "date").

.cond_ops <- c("==", "!=", "<", "<=", ">", ">=", "is.na", "!is.na")
.cond_op_labels <- c("==" = "= (any of)", "!=" = "\u2260 (none of)", "<" = "<", "<=" = "\u2264",
                     ">" = ">", ">=" = "\u2265", "is.na" = "is blank",
                     "!is.na" = "is not blank")

.cond_empty <- function() {
  d <- data.frame(group = integer(), var = character(), op = character(),
                  type = character(), stringsAsFactors = FALSE)
  d$values <- list()
  d$id <- integer()
  d
}

.cond_row <- function(group, var, op, values = character(), type = "chr") {
  d <- data.frame(group = as.integer(group), var = var, op = op, type = type,
                  stringsAsFactors = FALSE)
  d$values <- list(as.character(values))
  d$id <- NA_integer_
  d
}

# a variable as R writes it (`a b` when it is not a plain name)
.cond_name <- function(x) if (identical(make.names(x), x)) x else paste0("`", x, "`")

# a flag's blank, as a value of the rows (written NA, "")
.cond_blank <- "(blank)"

# The variables of the data as ADaM names them, in groups (a named list of
# named vectors: label = name), the population flags first; each group
# only when the data has one of its columns.  A flag that is not a
# population's is an analysis flag in data of several rows a subject, one
# of the others in a subject-level data (ADSL).
.cond_pop_flags <- c("SAFFL", "ITTFL", "FASFL", "PPROTFL", "RANDFL", "ENRLFL",
                     "COMPLFL")

# Which of `vars` are population flags (ADaM: SAFFL, ITTFL, FASFL, PPROTFL,
# RANDFL, ENRLFL, COMPLFL first in that order, then PPSFL, MITTFL, PKFL ...
# as they come): the names, in that order.  For a form that offers the
# analysis sets of a data (2-1).
.cond_population_flags <- function(vars) {
  vars <- unique(as.character(vars))
  pop <- vars %in% .cond_pop_flags |
    grepl("^(PP|PPS|MITT|EFF|EVAL|PK|PKAS|PKPD|SCRN|COMP[0-9]+|RAND[0-9]*)FL$", vars)
  v <- vars[pop]
  v[order(match(v, .cond_pop_flags), seq_along(v))]
}

# Each variable's kind, as ADaM names it: "population", "analysis" (an
# analysis flag), "treatment", "parameter", "timing" or "other" -- a named
# character vector.  `subject_level`: one row a subject (ADSL), where a
# flag that is not a population's is one of the others; given `data`, it
# is read from it (USUBJID once a row).
.cond_var_kind <- function(vars, data = NULL, subject_level = NULL) {
  vars <- as.character(vars)
  if (is.null(subject_level)) {
    subject_level <- !is.null(data) && "USUBJID" %in% names(data) &&
      !anyDuplicated(data$USUBJID)
  }
  pop <- vars %in% .cond_population_flags(vars)
  flag <- grepl("FL$", vars) & !pop
  anl <- grepl("^ANL[0-9]{2}FL$", vars) | (flag & !subject_level)
  trt <- grepl("^(TRT[0-9]{2}[PA]N?|TRT[PA]N?|TRTSEQ[PA]N?|ARM|ARMCD|ACTARM|ACTARMCD)$", vars)
  par <- grepl("^(PARAMCD|PARAM|PARAMN|PARCAT[0-9]+N?)$", vars)
  tim <- grepl("^(AVISITN?|ATPTN?|APHASEN?|APERIOD|APERIODC|VISIT|VISITNUM)$", vars)
  stats::setNames(ifelse(pop, "population", ifelse(anl, "analysis", ifelse(trt, "treatment",
                  ifelse(par, "parameter", ifelse(tim, "timing", "other"))))), vars)
}

.cond_var_groups <- function(vars, labels = NULL, subject_level = FALSE) {
  lab <- vapply(vars, function(v) {
    x <- if (!is.null(labels) && v %in% names(labels)) labels[[v]] else NA
    if (is.na(x) || !nzchar(x)) v else paste0(v, " \u2014 ", x)
  }, "")
  kind <- unname(.cond_var_kind(vars, subject_level = subject_level))
  ord <- order(match(vars, .cond_pop_flags), seq_along(vars))
  groups <- c(population = "Population flags", analysis = "Analysis flags",
              treatment = "Treatment", parameter = "Parameter", timing = "Timing",
              other = "Other")
  out <- list()
  for (k in names(groups)) {
    i <- ord[kind[ord] == k]
    if (length(i)) out[[groups[[k]]]] <- stats::setNames(vars[i], lab[i])
  }
  out
}

.cond_value <- function(v, type) {
  switch(type,
         num = v,
         date = sprintf("as.Date(%s)", .r_string(v)),
         .r_string(v))
}

# The R of the rows: NA for none.  Not equal is written !(x %in% ...), so a
# blank value counts as "not Y" (x != "Y" would drop it)
.cond_write <- function(rows) {
  if (is.null(rows) || !nrow(rows)) return(NA_character_)
  one <- function(i) {
    v <- rows$var[i]
    nm <- .cond_name(v)
    op <- rows$op[i]
    vals <- rows$values[[i]]
    vals <- unique(vals[!is.na(vals) & nzchar(vals)])
    if (op == "is.na") return(sprintf("is.na(%s)", nm))
    if (op == "!is.na") return(sprintf("!is.na(%s)", nm))
    if (!length(vals)) return(NA_character_)
    blank <- .cond_blank %in% vals
    vals <- setdiff(vals, .cond_blank)
    q <- c(vapply(vals, .cond_value, "", type = rows$type[i], USE.NAMES = FALSE),
           if (blank) c("NA", '""'))
    set <- if (length(q) == 1L) q else sprintf("c(%s)", paste(q, collapse = ", "))
    switch(op,
           "==" = if (length(q) == 1L) sprintf("%s == %s", nm, q) else
             sprintf("%s %%in%% %s", nm, set),
           "!=" = sprintf("!(%s %%in%% %s)", nm, set),
           sprintf("%s %s %s", nm, op, q[[1L]]))
  }
  parts <- vapply(seq_len(nrow(rows)), one, "")
  ok <- !is.na(parts)
  if (!any(ok)) return(NA_character_)
  g <- rows$group[ok]
  parts <- parts[ok]
  groups <- lapply(unique(g), function(k) parts[g == k])
  if (length(groups) == 1L) return(paste(groups[[1L]], collapse = " & "))
  paste(vapply(groups, function(p)
    if (length(p) > 1L) paste0("(", paste(p, collapse = " & "), ")") else p, ""),
    collapse = " | ")
}

# The rows of an R condition, or NULL when it is not one they can hold
# (any other call, a negation of several, "and" over "or" ...)
.cond_parse <- function(expr) {
  if (is.null(expr) || length(expr) != 1L || is.na(expr) || !nzchar(trimws(expr))) {
    return(.cond_empty())
  }
  e <- tryCatch(parse(text = expr, keep.source = FALSE), error = function(e) NULL)
  if (length(e) != 1L) return(NULL)
  e <- e[[1L]]
  unparen <- function(x) {
    while (is.call(x) && identical(x[[1L]], as.name("("))) x <- x[[2L]]
    x
  }
  flatten <- function(x, op) {
    x <- unparen(x)
    if (is.call(x) && (identical(x[[1L]], as.name(op)) ||
                         identical(x[[1L]], as.name(paste0(op, op))))) {
      c(flatten(x[[2L]], op), flatten(x[[3L]], op))
    } else list(x)
  }
  # a constant: its text and type; NULL if not one
  const <- function(x) {
    x <- unparen(x)
    # a blank: NA, or "" (a flag's third value)
    if (identical(x, NA) || identical(x, NA_character_) || identical(x, "")) {
      return(list(v = .cond_blank, type = NA_character_))
    }
    if (is.character(x) && length(x) == 1L) return(list(v = x, type = "chr"))
    if (is.numeric(x) && length(x) == 1L) return(list(v = format(x, digits = 15), type = "num"))
    if (is.call(x) && identical(x[[1L]], as.name("-")) && length(x) == 2L &&
        is.numeric(x[[2L]])) return(list(v = format(-x[[2L]], digits = 15), type = "num"))
    if (is.call(x) && identical(x[[1L]], as.name("as.Date")) && length(x) == 2L &&
        is.character(x[[2L]])) return(list(v = x[[2L]], type = "date"))
    NULL
  }
  consts <- function(x) {
    x <- unparen(x)
    if (is.call(x) && identical(x[[1L]], as.name("c"))) {
      vs <- lapply(as.list(x)[-1L], const)
    } else vs <- list(const(x))
    if (!length(vs) || any(vapply(vs, is.null, NA))) return(NULL)
    types <- unique(stats::na.omit(vapply(vs, `[[`, "", "type")))
    if (length(types) > 1L) return(NULL)
    if (!length(types)) types <- "chr"
    if (types != "chr" && any(is.na(vapply(vs, `[[`, "", "type")))) return(NULL)
    list(v = unique(vapply(vs, `[[`, "", "v")), type = types)
  }
  var_of <- function(x) {
    x <- unparen(x)
    if (is.name(x)) as.character(x) else NULL
  }
  atom <- function(x, group) {
    x <- unparen(x)
    if (!is.call(x)) return(NULL)
    f <- as.character(x[[1L]])[1L]
    if (f == "!" && length(x) == 2L) {
      y <- unparen(x[[2L]])
      if (is.call(y) && identical(y[[1L]], as.name("%in%"))) {
        r <- atom(y, group)
        if (is.null(r)) return(NULL)
        r$op <- "!="
        return(r)
      }
      if (is.call(y) && identical(y[[1L]], as.name("is.na")) && length(y) == 2L) {
        v <- var_of(y[[2L]])
        return(if (is.null(v)) NULL else .cond_row(group, v, "!is.na"))
      }
      return(NULL)
    }
    if (f == "is.na" && length(x) == 2L) {
      v <- var_of(x[[2L]])
      return(if (is.null(v)) NULL else .cond_row(group, v, "is.na"))
    }
    if (f == "%in%" && length(x) == 3L) {
      v <- var_of(x[[2L]])
      k <- consts(x[[3L]])
      if (is.null(v) || is.null(k)) return(NULL)
      return(.cond_row(group, v, "==", k$v, k$type))
    }
    if (f %in% c("==", "!=", "<", "<=", ">", ">=") && length(x) == 3L) {
      v <- var_of(x[[2L]])
      k <- const(x[[3L]])
      if (is.null(v) || is.null(k)) return(NULL)
      if (is.na(k$type)) {
        if (!f %in% c("==", "!=")) return(NULL)
        k$type <- "chr"
      }
      return(.cond_row(group, v, f, k$v, k$type))
    }
    NULL
  }
  groups <- flatten(e, "|")
  rows <- list()
  for (g in seq_along(groups)) {
    for (a in flatten(groups[[g]], "&")) {
      r <- atom(a, g)
      if (is.null(r)) return(NULL)
      rows[[length(rows) + 1L]] <- r
    }
  }
  do.call(rbind, rows)
}

# a condition's R, tidied: the same condition written the same way
.cond_tidy <- function(expr) {
  r <- .cond_parse(expr)
  if (is.null(r)) return(expr)
  .cond_write(r)
}

# A column's values to choose from: the most frequent first, at most `max`;
# NULL for a number or a date (typed in)
.cond_choices <- function(x, max = 200L) {
  if (is.numeric(x) || inherits(x, c("Date", "POSIXt"))) return(NULL)
  x <- as.character(x)
  blank <- sum(is.na(x) | !nzchar(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(character())
  tab <- sort(table(x), decreasing = TRUE)
  tab <- utils::head(tab, max)
  out <- stats::setNames(names(tab), sprintf("%s (%d)", names(tab), as.integer(tab)))
  # a flag (Y / N): its blank is a value to choose too
  if (blank && all(names(tab) %in% c("Y", "N"))) {
    out <- c(out, stats::setNames(.cond_blank, sprintf("%s (%d)", .cond_blank, blank)))
  }
  out
}

.cond_type <- function(x) {
  if (is.numeric(x)) "num" else if (inherits(x, c("Date", "POSIXt"))) "date" else "chr"
}

# the value search of a row: normalized as the function search's (a copy
# in JavaScript), the words ANDed
.cond_js <- "
window.tflCondNorm = window.tflCondNorm || function(s) {
  s = String(s == null ? '' : s).normalize('NFKC').toLowerCase();
  var out = '';
  for (var i = 0; i < s.length; i++) {
    var c = s.charCodeAt(i);
    if (c >= 0x3041 && c <= 0x3096) c += 0x60;
    var small = [0x30A1, 0x30A3, 0x30A5, 0x30A7, 0x30A9, 0x30C3, 0x30E3, 0x30E5, 0x30E7, 0x30EE, 0x30F5, 0x30F6];
    var big = [0x30A2, 0x30A4, 0x30A6, 0x30A8, 0x30AA, 0x30C4, 0x30E4, 0x30E6, 0x30E8, 0x30EF, 0x30AB, 0x30B1];
    var k = small.indexOf(c);
    if (k >= 0) c = big[k];
    if (c === 0x30FC || c === 0x30FB || c === 0x3D) continue;
    var ch = String.fromCharCode(c);
    out += /[!-/:-@[-`{-~]/.test(ch) || c === 0x3001 || c === 0x3002 ? ' ' : ch;
  }
  return out.replace(/ +/g, ' ').trim();
};
window.tflCondScore = function(search) {
  var words = tflCondNorm(search).split(' ').filter(function(w) { return w.length; });
  return function(item) {
    var t = tflCondNorm(item.label);
    var tq = t.replace(/ /g, '');
    for (var i = 0; i < words.length; i++) {
      if (t.indexOf(words[i]) < 0 && tq.indexOf(words[i]) < 0) return 0;
    }
    return 1;
  };
};
"

#' @noRd
condition_builder_ui <- function(id, lang = "en") {
  ns <- shiny::NS(id)
  shiny::div(
    class = "tfl-cond",
    shiny::tags$script(shiny::HTML(.cond_js)),
    shiny::uiOutput(ns("body")))
}

# `data` a reactive of the data the values come from (NULL: no data, the
# variables and values typed in); `value` a reactive of the condition now
# (R, NA for none); `labels` a reactive of the variables' labels (named),
# else the data's label attributes; `key` a reactive whose change reads
# `value` again (the row chosen).  Returns a reactive:
# list(expr = the R (NA for none), ok = it reads as R, n_rows = the rows),
# changed only when the condition changes.
condition_builder_server <- function(id, data, value, labels = NULL, lang = "en",
                                     key = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    t <- function(x) tr(x, lang)
    st <- shiny::reactiveValues(rows = .cond_empty(), raw = NULL, ver = 0L)
    out <- shiny::reactiveVal(list(expr = NA_character_, ok = TRUE, n_rows = 0L))
    # the condition last read or sent: the definition's value coming back
    # is not read again
    seen <- new.env()
    seen$v <- list("unset")
    set_out <- function(v) {
      seen$v <- v$expr
      if (!identical(shiny::isolate(out()), v)) out(v)
    }
    redraw <- function() st$ver <- shiny::isolate(st$ver) + 1L
    # each row's fields are named by an id never used again (a field of a
    # row taken out keeps its value in the browser for a while)
    ids <- new.env()
    ids$n <- 0L
    with_ids <- function(r) {
      r$id <- ids$n + seq_len(nrow(r))
      ids$n <- ids$n + nrow(r)
      r
    }
    # read the condition again: from the definition, for another row
    load <- function(expr) {
      seen$v <- expr
      r <- .cond_parse(expr)
      if (is.null(r)) {
        st$raw <- expr
        st$rows <- with_ids(.cond_empty())
      } else {
        st$raw <- NULL
        st$rows <- with_ids(r)
      }
      out(list(expr = if (is.null(r)) expr else .cond_write(r),
               ok = TRUE, n_rows = if (is.null(r)) 0L else nrow(r)))
      redraw()
    }
    same <- function(a, b) identical(a, b) ||
      (length(a) == 1L && length(b) == 1L && is.atomic(a) && is.atomic(b) &&
         is.na(a) && is.na(b))
    shiny::observeEvent(value(), {
      v <- value()
      # its own change coming back: nothing to read
      if (same(v, seen$v)) return()
      load(v)
    }, ignoreNULL = FALSE)
    if (!is.null(key)) shiny::observeEvent(key(), load(value()), ignoreInit = TRUE)

    d <- shiny::reactive(tryCatch(data(), error = function(e) NULL))
    labs <- shiny::reactive({
      l <- if (!is.null(labels)) tryCatch(labels(), error = function(e) NULL)
      dd <- d()
      if (is.null(l) && !is.null(dd)) {
        l <- vapply(dd, function(x) attr(x, "label") %||% NA_character_, "")
      }
      l
    })
    var_choices <- function() {
      dd <- d()
      if (is.null(dd)) return(NULL)
      # one row a subject: ADSL (its flags other than the populations' are
      # not analysis flags)
      subj <- "USUBJID" %in% names(dd) && !anyDuplicated(dd$USUBJID)
      g <- .cond_var_groups(names(dd), labs(), subject_level = subj)
      stats::setNames(g, t(names(g)))
    }
    # the values of a column, kept per data and column
    cache <- new.env()
    shiny::observeEvent(d(), rm(list = ls(cache), envir = cache), ignoreNULL = FALSE)
    choices_of <- function(v) {
      dd <- d()
      if (is.null(dd) || !v %in% names(dd)) return(NULL)
      if (is.null(cache[[v]])) cache[[v]] <- list(ch = .cond_choices(dd[[v]]),
                                                  type = .cond_type(dd[[v]]))
      cache[[v]]
    }
    js <- function(name, value) sprintf("Shiny.setInputValue(%s, %s, {priority: 'event'});",
                                       .r_string(ns(name)), value)

    output$body <- shiny::renderUI({
      st$ver
      raw <- shiny::isolate(st$raw)
      rows <- shiny::isolate(st$rows)
      if (!is.null(raw)) {
        return(shiny::tagList(
          shiny::p(class = "small text-muted mb-1",
                   t("Written as R (the rows cannot hold it): edit it here.")),
          shiny::textAreaInput(ns("raw"), NULL, raw, width = "100%", rows = 2),
          shiny::div(class = "d-flex gap-2",
                     shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary",
                                        onclick = js("to_rows", "Math.random()"),
                                        t("As rows")))))
      }
      vc <- var_choices()
      groups <- if (nrow(rows)) unique(rows$group) else 1L
      row_ui <- function(i) {
        id <- rows$id[i]
        v <- rows$var[i]
        op <- rows$op[i]
        ch <- if (nzchar(v)) choices_of(v)
        type <- ch$type %||% rows$type[i]
        vals <- rows$values[[i]]
        var_in <- if (is.null(vc)) {
          shiny::textInput(ns(paste0("var_", id)), NULL, v, width = "100%",
                           placeholder = t("Variable"))
        } else {
          shiny::selectizeInput(ns(paste0("var_", id)), NULL, width = "100%",
                                choices = c(stats::setNames("", t("Variable")), vc,
                                            if (nzchar(v) && !v %in% unlist(vc)) stats::setNames(v, v)),
                                selected = v,
                                options = list(score = I("tflCondScore")))
        }
        ops <- stats::setNames(.cond_ops, t(unname(.cond_op_labels[.cond_ops])))
        if (identical(type, "chr")) ops <- ops[!.cond_ops %in% c("<", "<=", ">", ">=")]
        op_in <- shiny::selectInput(ns(paste0("op_", id)), NULL, ops, selected = op,
                                    width = "100%", selectize = FALSE)
        val_in <- if (op %in% c("is.na", "!is.na")) NULL else
          if (identical(type, "chr") && !is.null(ch$ch)) {
            shiny::selectizeInput(
              ns(paste0("val_", id)), NULL, width = "100%", multiple = op %in% c("==", "!="),
              choices = c(ch$ch, stats::setNames(setdiff(vals, ch$ch), setdiff(vals, ch$ch))),
              selected = vals,
              options = list(placeholder = t("Values (search)"), create = TRUE,
                             score = I("tflCondScore")))
          } else {
            shiny::textInput(ns(paste0("val_", id)), NULL, paste(vals, collapse = ", "),
                             width = "100%",
                             placeholder = switch(type, date = "2024-01-31", num = "65",
                                                  t("Values, comma between")))
          }
        shiny::div(
          class = "d-flex gap-1 align-items-start",
          shiny::div(style = "flex: 3 1 0", var_in),
          shiny::div(style = "flex: 0 0 11rem", op_in),
          shiny::div(style = "flex: 4 1 0", val_in),
          shiny::tags$button(type = "button", class = "btn btn-sm btn-link text-muted px-1",
                             title = t("Remove"), onclick = js("del", id), "\u00d7"))
      }
      body <- lapply(seq_along(groups), function(k) {
        idx <- which(rows$group == groups[k])
        shiny::tagList(
          if (k > 1L) shiny::div(class = "small fw-semibold text-muted my-1", t("or")),
          shiny::div(class = "border rounded p-1 mb-1",
                     lapply(idx, row_ui),
                     shiny::tags$button(type = "button", class = "btn btn-sm btn-link py-0",
                                        onclick = js("add", groups[k]), t("+ and"))))
      })
      shiny::tagList(
        if (!nrow(rows)) shiny::p(class = "small text-muted mb-1", t("No condition: every row.")),
        body,
        shiny::div(
          class = "d-flex flex-wrap gap-2 small",
          shiny::tags$button(type = "button", class = "btn btn-sm btn-link py-0",
                             onclick = js("add_or", "Math.random()"), t("+ or (another group)")),
          shiny::tags$button(type = "button", class = "btn btn-sm btn-link py-0 text-muted",
                             onclick = js("to_raw", "Math.random()"), t("Write as R"))))
    })

    # the rows as the fields have them now
    rows_now <- function() {
      r <- shiny::isolate(st$rows)
      if (!nrow(r)) return(r)
      for (i in seq_len(nrow(r))) {
        id <- r$id[i]
        v <- shiny::isolate(input[[paste0("var_", id)]])
        if (!is.null(v)) r$var[i] <- v
        op <- shiny::isolate(input[[paste0("op_", id)]])
        if (!is.null(op)) r$op[i] <- op
        ch <- if (nzchar(r$var[i])) choices_of(r$var[i])
        if (!is.null(ch$type)) r$type[i] <- ch$type
        val <- shiny::isolate(input[[paste0("val_", id)]])
        if (!is.null(val)) {
          r$values[[i]] <- if (identical(r$type[i], "chr") && !is.null(ch$ch)) as.character(val) else
            trimws(strsplit(paste(val, collapse = ","), ",", fixed = TRUE)[[1L]])
        }
      }
      r
    }
    # a field changed: the condition again; a variable or an operator
    # changed: the row's fields again too
    shiny::observe({
      rows <- st$rows
      n <- nrow(rows)
      if (!is.null(st$raw)) return()
      vals <- lapply(rows$id, function(id) list(
        input[[paste0("var_", id)]], input[[paste0("op_", id)]], input[[paste0("val_", id)]]))
      r <- rows_now()
      shape <- function(x) paste(x$var, x$op, x$type)
      if (!identical(shape(r), shape(rows))) {
        # the values of another column are not this one's; a flag (*FL)
        # starts as = Y, to change from there
        changed <- r$var != rows$var
        r$values[changed] <- list(character())
        fl <- changed & grepl("FL$", r$var)
        r$op[fl] <- "=="
        r$values[fl] <- list("Y")
        st$rows <- r
        redraw()
      }
      expr <- .cond_write(r)
      set_out(list(expr = expr, ok = TRUE, n_rows = nrow(r)))
    })
    shiny::observeEvent(input$raw, {
      txt <- input$raw
      ok <- !nzchar(trimws(txt)) || !is.null(tryCatch(parse(text = txt), error = function(e) NULL))
      set_out(list(expr = if (nzchar(trimws(txt))) txt else NA_character_, ok = ok,
                    n_rows = 0L))
    })
    add_row <- function(group) {
      r <- rows_now()
      st$rows <- rbind(r, with_ids(.cond_row(group, "", "==")))
      redraw()
    }
    shiny::observeEvent(input$add, add_row(as.integer(input$add)))
    shiny::observeEvent(input$add_or, {
      r <- rows_now()
      add_row(if (nrow(r)) max(r$group) + 1L else 1L)
    })
    shiny::observeEvent(input$del, {
      r <- rows_now()
      r <- r[r$id != as.integer(input$del), , drop = FALSE]
      rownames(r) <- NULL
      st$rows <- r
      redraw()
    })
    shiny::observeEvent(input$to_raw, {
      e <- .cond_write(rows_now())
      st$raw <- if (is.na(e)) "" else e
      redraw()
    })
    shiny::observeEvent(input$to_rows, {
      r <- .cond_parse(input$raw %||% "")
      if (is.null(r)) {
        shiny::showNotification(t("The rows cannot hold this condition: it stays as R."),
                                type = "warning")
        return()
      }
      st$raw <- NULL
      st$rows <- with_ids(r)
      redraw()
    })
    out
  })
}
