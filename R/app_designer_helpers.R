# The Plot Designer's helpers: where a piece goes, the script lines a piece makes,
# and the column choosers of the template form.
# Split from R/app_designer.R (no code change).

# Where a piece added goes (after its index; 0 = first): after the piece
# chosen in its section; else a layer last (a KM figure's curves first), a
# step that makes an object of its own last, a step on df before the first
# such step (#293)
.pd_add_at <- function(x, k, sec, s) {
  at <- if (identical(s$sec, sec) && isTRUE(s$i >= 1L && s$i <= length(x))) s$i
  if (sec == "layers") return(at %||% if (identical(k, "km_curve")) 0L else length(x))
  named <- which(vapply(x, function(p) isTRUE(p$step %in% names(.pd_named_steps)), NA))
  df_end <- if (length(named)) named[1L] - 1L else length(x)
  # a step that reads or keeps ADaM rows is a step on df, whatever is
  # chosen; a step that makes an object goes among those; the others
  # (derive, filter ...) after the step chosen, so on its object
  if (k %in% .pd_source_steps) return(if (isTRUE(at <= df_end)) at else df_end)
  if (k %in% names(.pd_named_steps)) return(if (isTRUE(at > df_end)) at else length(x))
  at %||% df_end
}
.pd_source_steps <- c("read", "join", "param", "flag", "time_unit")

# A join's fields as its card shows them: its how named for what it does;
# a semi join (only the subjects the dataset has) adds no variables, so
# they are not asked (shown, not starred, while it still has some)
.pd_join_fields <- function(f, p) {
  f$label[f$field == "how"] <- "How to join (how)"
  if (identical(p$how, "semi")) {
    v <- f$field == "vars"
    f$required[v] <- FALSE
    if (!length(p$vars) || all(!nzchar(trimws(p$vars)))) f <- f[!v, , drop = FALSE]
  }
  f
}

# A choice field's choices (tflspec's "a | b"), named by their words where
# the app has them (a join's how: left / semi)
.pd_choice_words <- function(r, t = identity) {
  if (is.na(r$choices)) return(NULL)
  ch <- strsplit(r$choices, " | ", fixed = TRUE)[[1L]]
  words <- c(left = "Left join (left)", semi = "Keep its subjects only (semi)")
  if (!identical(r$field, "how") || !all(ch %in% names(words))) return(ch)
  stats::setNames(ch, t(unname(words[ch])))
}

# The lines of a design's script that one piece makes: the lines the
# script has with it and not without it (`make(d)` writes a design's
# script; for the code lists, `make(d, codelists = FALSE)`), its parts
# apart by "# ..."; the figure settings: the plot section; a whole-script
# layer: the script.
.piece_code <- function(code, d, s, make) {
  # (the piece itself: `d` loses it below, to make the code without it)
  p <- if (identical(s$sec, "data") && s$i <= length(d$data)) d$data[[s$i]]
  whole <- identical(s$sec, "layers") &&
    identical(d$layers[[s$i]]$layer %||% "", "figure")
  out <- if (whole) code else if (identical(s$sec, "plot")) {
    from <- grep("^# ---- plot ", code)
    if (length(from)) code[seq(from[1L], length(code))] else character()
  } else {
    other <- tryCatch(switch(s$sec,
      layers = {
        num <- which(vapply(d$layers, function(l) identical(l$layer, "ard_number"), NA))
        d$layers <- d$layers[-s$i]
        out <- make(d)
        # an ARD number's label is ard_label_<k>, k its place among them:
        # the code without it numbers the later ones back as they were
        r <- match(s$i, num)
        if (!is.na(r)) {
          for (j in rev(seq_len(length(num) - r) + r - 1L)) {
            out <- gsub(paste0("\\bard_label_", j, "\\b"), paste0("ard_label_", j + 1L), out,
                        perl = TRUE)
          }
        }
        out
      },
      data = { d$data <- d$data[-s$i]; make(d) },
      codelists = make(d, codelists = FALSE),
      NULL), error = function(e) NULL)
    if (is.null(other)) character() else .lines_added(code, other)
  }
  while (length(out) && !nzchar(trimws(out[length(out)]))) out <- out[-length(out)]
  # a time already in days: nothing to change (ADTTE's AVAL is in days)
  if (!length(out) && identical(p$step, "time_unit") && identical(p$unit %||% "days", "days")) {
    return(t_static("(no line: the time is in days already; months, weeks or years divide it)"))
  }
  if (!length(out)) return(t_static("(nothing yet: this piece writes no line on its own)"))
  paste(out, collapse = "\n")
}

# The lines of `a` that are not in `b` (the longest common lines apart), a
# "  ..." between two runs of them
.lines_added <- function(a, b) {
  shown <- a
  # a line the same but for its "+" or "|>" at the end is the same line
  end <- "[[:space:]]*([+]|[|]>)[[:space:]]*$"
  a <- sub(end, "", a)
  b <- sub(end, "", b)
  n <- length(a)
  m <- length(b)
  L <- matrix(0L, n + 1L, m + 1L)
  for (i in rev(seq_len(n))) for (j in rev(seq_len(m))) {
    L[i, j] <- if (identical(a[i], b[j])) L[i + 1L, j + 1L] + 1L
               else max(L[i + 1L, j], L[i, j + 1L])
  }
  added <- logical(n)
  i <- 1L
  j <- 1L
  while (i <= n) {
    if (j <= m && identical(a[i], b[j])) {
      i <- i + 1L
      j <- j + 1L
    } else if (j <= m && L[i, j + 1L] >= L[i + 1L, j]) {
      j <- j + 1L
    } else {
      added[i] <- TRUE
      i <- i + 1L
    }
  }
  # a run of lines the same at both ends could be put one line up or
  # down (two terms alike: annotate( ... ) +): put it where it starts
  # least indented and not on a closing ")" -- a term's first line
  # (earliest when alike)
  w0 <- which(added)
  if (length(w0)) {
    for (run in split(w0, cumsum(c(1L, diff(w0) > 1L)))) {
      s <- run[1L]
      e <- run[length(run)]
      added[run] <- FALSE
      while (s > 1L && !added[s - 1L] && identical(a[s - 1L], a[e])) {
        s <- s - 1L
        e <- e - 1L
      }
      best <- s
      ind <- function(k) nchar(shown[k]) - nchar(sub("^ +", "", shown[k])) +
        if (grepl("^[)}]", trimws(shown[k]))) 0.5 else 0
      while (e < n && !added[e + 1L] && identical(a[e + 1L], a[s])) {
        s <- s + 1L
        e <- e + 1L
        if (ind(s) < ind(best)) best <- s
      }
      added[best:(best + length(run) - 1L)] <- TRUE
    }
  }
  # (the packages: the program's setup attaches them)
  w <- which(added & nzchar(trimws(a)) & !grepl("^library[(]", a))
  if (!length(w)) return(character())
  runs <- split(w, cumsum(c(1L, diff(w) > 1L)))
  unlist(lapply(seq_along(runs), function(r)
    c(if (r > 1L) "# ...", shown[runs[[r]]])), use.names = FALSE)
}
t_static <- function(x) x

# ---- choosing variables in the template form ------------------------------

# Every column of the datasets read, once (the first dataset that has it).
.data_columns <- function(dat) {
  cols <- list()
  for (d in dat) for (nm in names(d)) if (is.null(cols[[nm]])) cols[[nm]] <- d[[nm]]
  cols
}

# The second level of a group column (the first is the reference), or NULL
.second_level <- function(dat, group) {
  cols <- .data_columns(dat)
  g <- group %||% .group_choices(cols)[1L]
  if (is.null(g) || is.na(g) || is.null(cols[[g]])) return(NULL)
  v <- cols[[g]]
  lv <- if (is.factor(v)) levels(v) else sort(unique(stats::na.omit(as.character(v))))
  if (length(lv) >= 2L) lv[[2L]] else NULL
}

# Choices shown as "NAME \u2014 label" when the column has a label.
.labelled <- function(nm, cols) {
  nm <- as.character(nm)
  shown <- vapply(nm, function(n) {
    l <- attr(cols[[n]], "label", exact = TRUE)
    if (is.null(l) || !nzchar(l)) n else paste0(n, " \u2014 ", l)
  }, "", USE.NAMES = FALSE)
  stats::setNames(nm, shown)
}

# Columns a summary table can have as rows, shown with how they are
# summarized: "AGE \u2014 Age (numbers)".  Not identifiers, flags, dates.
.row_choices <- function(cols, words = c(continuous = "numbers",
                                          categorical = "counts")) {
  if (!length(cols)) return(character())
  ok <- vapply(names(cols), function(nm) {
    !grepl("^(STUDYID|USUBJID|SUBJID|SITEID)$|FL$", nm) &&
      !.date_like(nm, cols[[nm]]) && !is.na(.column_kind(cols[[nm]]))
  }, NA)
  nm <- names(cols)[ok]
  ch <- .labelled(nm, cols)
  kind <- vapply(nm, function(n) .column_kind(cols[[n]]), "", USE.NAMES = FALSE)
  names(ch) <- paste0(names(ch), " (",
                      words[kind],
                      ")")
  ch
}

# A date, a time, or their imputation flag, whatever its type: named so
# (ADaM / SDTM: ...DTC, ...DT, ...DTM, ...TM, ...DTF, ...TMF, ...DY) or
# holding ISO 8601 dates as text.
.date_like <- function(nm, v) {
  if (inherits(v, c("Date", "POSIXt", "difftime"))) return(TRUE)
  if (grepl("(DTC|DTM|DT|TM|DTF|TMF)$", nm)) return(TRUE)
  if (is.character(v)) {
    x <- utils::head(v[!is.na(v) & nzchar(v)], 20L)
    if (length(x) && all(grepl("^[0-9]{4}-[0-9]{2}(-[0-9]{2})?", x))) return(TRUE)
  }
  FALSE
}

# Columns a figure can be split by: named like a treatment (TRT..., ARM...),
# text or a factor, with few values -- not a date or a time (TRTSDT).
.group_choices <- function(cols, max_levels = 12L) {
  if (!length(cols)) return(character())
  ok <- vapply(names(cols), function(nm) {
    v <- cols[[nm]]
    grepl("^TRT|ARM", nm) && !.date_like(nm, v) &&
      (is.character(v) || is.factor(v)) &&
      length(unique(stats::na.omit(v))) <= max_levels
  }, NA)
  .labelled(names(cols)[ok], cols)
}
