# The table builder's row helpers: layers, row labels and keys, the hierarchy's
# sort.  Split from builder.R (no code change).

# The rows' candidates: the normalized ARD's keys less the columns' (one
# column is in cols or rows, not both); a summary table's less its
# analysis variables (values of variable, not columns), with the keys a
# stack adds -- subgroup, subgroup_level, though the ARD read has not got
# them yet -- and the block of the analysis variables (".variable")
.layer_choices <- function(keys, cols, vars, hier = FALSE, stack = NA, nest = NA) {
  keys <- keys %||% character()
  if (hier) return(setdiff(keys, cols))
  unique(c(setdiff(keys, c(cols, vars)), .stack_keys(stack, nest), ".variable"))
}

# The keys normalize_ard()'s stack adds (tables$stack "subgroup = ALL, SEX
# | ..."): each name and its _level, and its _outer when the stack nests
# (tables$stack_nest TRUE); none for no stack
.stack_keys <- function(stack, nest = NA) {
  if (.is_blank(stack)) return(character())
  part <- trimws(strsplit(stack, "|", fixed = TRUE)[[1L]])
  nm <- ifelse(grepl("=", part, fixed = TRUE), trimws(sub("=.*$", "", part)), "subgroup")
  nm <- unique(nm[nzchar(nm)])
  nested <- !.is_blank(nest) && isTRUE(as.logical(toupper(trimws(nest))))
  unlist(lapply(nm, function(n) c(n, if (nested) paste0(n, "_outer"), paste0(n, "_level"))))
}

# A summary table's row text (tables$label): ".label" (blank: what
# normalize_ard() builds), a key ("label = SEX"), NA (none: "NA"), or
# "own" (a template or a guard the builder does not write: kept)
.row_label_read <- function(label) {
  if (.is_blank(label)) return(".label")
  v <- trimws(sub("^[^=]*=", "", trimws(label)))
  if (toupper(v) %in% c("NA", "FALSE")) return(NA_character_)
  if (identical(v, ".label") || grepl("^[A-Za-z.][A-Za-z0-9._]*$", v)) return(v)
  "own"
}

# ... and written: blank for ".label", "NA" for none, "label = KEY"
.row_label_write <- function(label) {
  if (length(label) != 1L || is.na(label)) return("NA")
  if (identical(label, ".label")) return(NA_character_)
  paste("label =", label)
}

# A variable's levels in order, data first, and where the order comes
# from: this table's own (the variables sheet's levels: "table"), the
# report's code list ("codelist", ARD step 1-1 -- tflspec writes
# plan_levels() from it, so the screen and the code agree), the ARD's
# factor ("ard": `ard_factor`, its levels), else the ARD's values as
# first seen ("seen"); a code list's order is followed by the ARD's values
# it does not name.  The precedence is `steps`' order, here only.
# `differs`: the ARD keeps a factor's order and the code list puts the
# values they share in another.  A column the plan makes (`plan_made`: the
# stack's subgroup, .stat_group) has the plan's order -- its code list
# rows give its labels only (tflspec writes no plan_levels() from them):
# the ARD's as it is ("plan"), nothing said of it.
.level_order <- function(own = character(), codelist = character(), ard = character(),
                         ard_factor = FALSE, plan_made = FALSE) {
  if (isTRUE(plan_made) && !length(own)) {
    return(list(levels = as.character(ard), source = "plan", differs = FALSE))
  }
  steps <- list(table = own, codelist = codelist,
                ard = if (isTRUE(ard_factor)) ard else character(), seen = ard)
  at <- which(lengths(steps) > 0L)
  src <- if (length(at)) names(steps)[at[1L]] else "seen"
  both <- intersect(ard, codelist)
  differs <- isTRUE(ard_factor) && length(both) > 1L &&
    !identical(both, intersect(codelist, ard))
  lv <- as.character(steps[[src]])
  # (the ARD's values a code list does not name: after its own)
  if (identical(src, "codelist")) lv <- c(lv, setdiff(ard, lv))
  list(levels = lv, source = src, differs = differs)
}

# A summary table's row layers from its tables$rows ("group1 = SEX | group
# = variable"), outermost first: the keys, ".variable" for the block of the
# analysis variables where the rows put it, a constant heading as written
# ('"Overall"'); the block alone when the rows say nothing.  Rows without
# the block have none (a table of one variable: ORR by subgroup).
.row_layers_read <- function(rows) {
  if (.is_blank(rows)) return(".variable")
  part <- trimws(strsplit(rows, "|", fixed = TRUE)[[1L]])
  v <- trimws(sub("^[^=]*=", "", part))
  v <- v[nzchar(v)]
  v[v == "variable"] <- ".variable"
  v
}

# ... and back, in the order given: the keys and the constant headings a
# group each (group1, group2 ...), the block as `group = variable` where it
# is; NA for no layers (the rows' default: the block)
.row_layers_write <- function(layers) {
  layers <- layers[!is.na(layers) & nzchar(layers)]
  if (!length(layers)) return(NA_character_)
  k <- 0L
  parts <- vapply(layers, function(l) {
    if (identical(l, ".variable")) return("group = variable")
    k <<- k + 1L
    sprintf("group%d = %s", k, l)
  }, "", USE.NAMES = FALSE)
  paste(parts, collapse = " | ")
}

# a layer that is a constant heading ('"Overall"', ~"Worst grade"), not a key
.is_heading <- function(x) grepl("^[\"'~]", x)

# A table's row keys, outermost first, from its tables$rows and
# tables$label ("group1 = AEBODSYS", "label = AEDECOD"): the columns named
# (not a constant heading, not .label)
.row_keys_read <- function(rows, label) {
  part <- function(x) {
    if (.is_blank(x)) return(character())
    v <- trimws(sub("^[^=]*=", "", trimws(strsplit(x, "|", fixed = TRUE)[[1L]])))
    v[nzchar(v) & !grepl("^[\"'~]", v)]
  }
  lab <- part(label)
  c(part(rows), lab[lab != ".label"])
}

# A hierarchy table's order (tables$sort), written as plan_sort() takes
# it, one order a level, outermost first: "alpha" (by name -- a code
# list's order when the report has one: a factor sorts by its levels),
# "freq" (most frequent first, ties by name: -n), a key of a statistic
# ("-max(n)", "min(n)", "-p", "-estimate": a sign, a statistic of the ARD,
# max() / min() / sum() over the columns or not) or "=<key>" (a key of the
# user's, written as it is).  An outer level's statistic is its own row's
# (-n@group1); the innermost its rows'.  `col`: the column whose value
# counts (a level of the outer column key, or the Total column's
# heading: -n[Total]); NA every column's together; `nocol`: the
# statistics no column is added to (a comparison's: estimate).  The "any
# event" row first, and each group's own row above the rows under it.
# `hier`: which row keys are the hierarchy's levels -- a group of the ARD
# outside it (a stratum, outermost) comes before the "any event" row and
# has no summary row of its own.  `first`: the levels whose values
# plan_levels() names come first, in its order (levels(group1), before the
# level's own key; rtfreporter >= 0.8.2.9083) -- by default the "first"
# attribute of `levels`, none when it has none.
.hier_sort <- function(levels = c("alpha", "freq"), col = NA_character_,
                       hier = rep(TRUE, length(levels)), nocol = character(),
                       first = attr(levels, "first")) {
  n <- length(levels)
  first <- rep_len(as.logical(first %||% FALSE), n)
  out <- character()
  overall <- FALSE
  for (i in seq_len(n)) {
    g <- if (i < n) sprintf("group%d", i) else "label"
    if (hier[i] && !overall) {
      out <- c(out, ".overall")
      overall <- TRUE
    }
    lv <- levels[i]
    if (identical(lv, "freq")) lv <- "-n"
    count <- if (startsWith(lv, "=")) substring(lv, 2L) else if (!identical(lv, "alpha")) {
      at <- if (!is.na(col) && nzchar(col) && !.sort_key_stat(lv) %in% nocol) sprintf("[%s]", col) else ""
      paste0(lv, if (i < n) paste0("@", g), at)
    }
    if (!is.null(count) && !nzchar(trimws(count))) count <- NULL
    if (isTRUE(first[i])) count <- c(sprintf("levels(%s)", g), count)
    out <- c(out, count, g, if (hier[i] && i < n) ".depth")
  }
  if (!overall) out <- c(".overall", out)
  paste(out, collapse = " | ")
}

# A sort key's statistic ("-max(n)" n), "" when it is no key of one
.sort_key_stat <- function(key) {
  m <- regmatches(key, regexec(.sort_key_re, key))[[1L]]
  if (!length(m)) "" else if (nzchar(m[5L])) m[5L] else m[6L]
}
# (a sign; max(s), min(s), sum(s) or s; nothing after)
.sort_key_re <- "^(-?)((max|min|sum)\\(([A-Za-z.][A-Za-z0-9._]*)\\)|([A-Za-z.][A-Za-z0-9._]*))$"

# The orders a row key's card offers (a named vector: the value
# .hier_sort() takes, its label with its key): by name (`coded`: the code
# list's order), and for a level of the hierarchy each statistic of the
# ARD (`stats`; n and p first, N -- the same on every row -- left out)
# largest and smallest first, by the columns' total or by the largest /
# smallest in any column.
.sort_key_choices <- function(stats = character(), coded = FALSE, hier = TRUE,
                              lang = "en") {
  t <- function(x) tr(x, lang)
  out <- stats::setNames("alpha", if (coded) t("Code list's order") else t("By name"))
  if (!hier) return(out)
  s <- unique(as.character(stats))
  s <- s[!is.na(s) & s != "N" & grepl("^[A-Za-z.][A-Za-z0-9._]*$", s)]
  s <- c(intersect(c("n", "p"), s), setdiff(s, c("n", "p")))
  if (!length(s)) s <- "n"
  for (x in s) {
    n <- identical(x, "n")
    keys <- c(paste0("-", x), x, sprintf("-max(%s)", x), sprintf("min(%s)", x))
    labs <- if (n) c(
      if (coded) t("Most frequent first (ties in the code list's order)") else
        t("Most frequent first (ties by name)"),
      t("Least frequent first (the columns' total)"),
      t("Most frequent first (in any column)"),
      t("Least frequent first (in any column)")) else c(
        t("Largest first (the columns' total)"), t("Smallest first (the columns' total)"),
        t("Largest first (in any column)"), t("Smallest first (in any column)"))
    vals <- keys
    if (n) vals[1L] <- "freq"
    out <- c(out, stats::setNames(vals, paste0(labs, ": ", keys)))
  }
  out
}

# ... and read back: the order of each level (attribute "col": the column
# that counts, NA all of them), or "own" (an order the builder does not
# write: kept as it is).  n: the terms; nocol: as .hier_sort()'s.  A key
# not of the builder's form is the level's own ("=<key>"), written back
# as it is.  levels(<level>) before a level's key: attribute "first" (the
# levels it is on; none, no attribute).
.hier_sort_read <- function(sort, n = 2L, nocol = character()) {
  if (length(sort) != 1L || is.na(sort) || !nzchar(trimws(sort))) return("own")
  tk <- trimws(strsplit(sort, "|", fixed = TRUE)[[1L]])
  if (!".overall" %in% tk) return("own")
  tk <- tk[!tk %in% c(".overall", ".depth")]
  lv <- character(n)
  first <- logical(n)
  cols <- character()
  for (i in seq_len(n)) {
    name <- if (i < n) sprintf("group%d", i) else "label"
    if (identical(tk[1L], sprintf("levels(%s)", name))) {
      first[i] <- TRUE
      tk <- tk[-1L]
    }
    if (!length(tk)) return("own")
    if (identical(tk[1L], name)) {
      lv[i] <- "alpha"
    } else {
      k <- tk[1L]
      tk <- tk[-1L]
      if (!length(tk) || !identical(tk[1L], name)) return("own")
      # (the key less its column, and less its own group's @)
      m <- regmatches(k, regexec("^(.*?)(\\[(.+)\\])?$", k, perl = TRUE))[[1L]]
      e <- m[2L]
      if (i < n) e <- if (endsWith(e, paste0("@", name))) substr(e, 1L, nchar(e) - nchar(name) - 1L) else ""
      s <- .sort_key_stat(e)
      if (!nzchar(s) || (s %in% nocol && nzchar(m[3L]))) {
        lv[i] <- paste0("=", k)
      } else {
        lv[i] <- if (identical(e, "-n")) "freq" else e
        if (!s %in% nocol) cols <- c(cols, if (nzchar(m[3L])) m[4L] else NA_character_)
      }
    }
    tk <- tk[-1L]
  }
  # one column counts for every level, or the order is the sheets' own
  if (length(tk) || length(unique(cols)) > 1L) return("own")
  attr(lv, "col") <- if (length(cols)) cols[1L] else NA_character_
  if (any(first)) attr(lv, "first") <- first
  lv
}
