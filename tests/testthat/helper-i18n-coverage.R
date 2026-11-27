# The screen texts' coverage (test-i18n-coverage.R): the texts of tflspec the
# app shows, which need a row in inst/i18n/strings/*.csv, and the texts the
# three packages can show, which a row's key must be one of.

# The tflspec texts the app shows, from the installed tflspec: a data frame
# `text`, `where` (the function, the template or "<series>/<template>", then
# the argument), `kind`:
# - "schema": an argument's help or description in the schema file
#   (tfl_schema(); the builder's and the spec card's (i), .arg_help());
# - "defs": an argument's description in tfl_arg_defs() (the spec card's
#   help when the schema has none, a series template's argument);
# - "template": a template parameter's doc (tfl_template_params(); the
#   template form's (i), .tpl_doc_shown()).
# A text tflspec gives in Japanese itself (`help: {en, ja}`, a def's
# `help_ja`) is shown so in a template's form: not listed there.
# A figure layer's arguments that tflspec takes from the installed ggplot2
# (tfl_schema(fn) of a geom_*) are not listed: they are ggplot2's.
.i18n_tflspec_texts <- function() {
  one <- function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v)
  rows <- list()
  add <- function(text, where, kind) {
    if (one(text)) rows[[length(rows) + 1L]] <<- data.frame(text = text, where = where, kind = kind)
  }
  sch <- tflspec::tfl_schema()
  for (fn in names(sch)) {
    args <- sch[[fn]]$args
    for (a in names(args)) {
      h <- args[[a]]$help
      if (is.list(h)) {
        if (!one(h$ja)) add(h$en, paste(fn, a), "schema")
      } else {
        add(h, paste(fn, a), "schema")
      }
      add(args[[a]]$description, paste(fn, a), "schema")
    }
  }
  for (fn in c(names(sch), "data", "analysis")) {
    d <- tryCatch(tflspec::tfl_arg_defs(fn = fn), error = function(e) NULL)
    if (!is.data.frame(d) || !nrow(d)) next
    # (the file's arguments only: the others are the installed ggplot2's)
    keep <- if (fn %in% names(sch)) d$name %in% names(sch[[fn]]$args) else rep(TRUE, nrow(d))
    for (i in which(keep)) add(d$description[i], paste(fn, d$name[i]), "defs")
  }
  for (tp in tflspec::tfl_templates()$name) {
    for (p in tryCatch(tflspec::tfl_template_params(tp), error = function(e) list())) {
      add(p$doc, paste(tp, p$name), "template")
    }
  }
  for (sr in tflspec::tfl_series_list()$name) {
    dir <- file.path(tflspec::tfl_series_dir(sr), "templates")
    for (tn in sub("[.]ya?ml$", "", list.files(dir, "[.]ya?ml$"))) {
      d <- tryCatch(tflspec::tfl_arg_defs(template = paste0(sr, "/", tn)), error = function(e) NULL)
      if (!is.data.frame(d) || !nrow(d)) next
      ja <- if ("help_ja" %in% names(d)) d$help_ja else rep(NA_character_, nrow(d))
      for (i in seq_len(nrow(d))) {
        if (!one(ja[i])) add(d$description[i], paste0(sr, "/", tn, " ", d$name[i]), "defs")
      }
    }
  }
  out <- do.call(rbind, rows)
  out[!duplicated(out$text), , drop = FALSE]
}

# Every string constant a package holds: its functions' bodies and
# defaults, its objects (lists, data frames; the objects of the registries
# it keeps in environments, tflspec's .tpl_registry), each paste() /
# paste0() of constants pasted as the call would.  The texts its code can
# give t().  Not its other environments: caches, whose content depends on
# what ran before (and may nest deep enough to overflow the stack).
.i18n_constants <- function(pkg) {
  acc <- list()
  push <- function(x) acc[[length(acc) + 1L]] <<- x
  paste_const <- function(x) {
    f <- as.character(x[[1L]])
    args <- as.list(x)[-1L]
    nm <- names(args) %||% rep("", length(args))
    pos <- args[!nzchar(nm)]
    if (!length(pos) || !all(vapply(pos, function(a) is.character(a) && length(a) == 1L, NA))) return()
    sep <- if (identical(f, "paste0")) "" else if (is.character(args$sep)) args$sep else " "
    push(paste(unlist(pos), collapse = sep))
  }
  walk <- function(x) {
    if (is.null(x) || is.symbol(x)) return(invisible())
    if (is.character(x)) {
      push(x[!is.na(x)])
    } else if (is.function(x)) {
      if (!is.primitive(x)) {
        walk(formals(x))
        walk(body(x))
      }
    } else if (is.call(x)) {
      if (is.symbol(x[[1L]]) && as.character(x[[1L]]) %in% c("paste", "paste0")) paste_const(x)
      for (a in as.list(x)) if (!missing(a)) walk(a)
    } else if (is.list(x) || is.pairlist(x) || is.expression(x)) {
      for (a in as.list(x)) if (!missing(a)) walk(a)
    } else if (is.factor(x)) {
      push(levels(x))
    }
    if (!is.environment(x) && !is.function(x)) push(names(x))
    invisible()
  }
  ns <- asNamespace(pkg)
  for (nm in ls(ns, all.names = TRUE)) {
    v <- get(nm, envir = ns)
    if (!is.environment(v)) {
      walk(v)
    } else if (grepl("registry$", nm)) {
      for (k in ls(v, all.names = TRUE)) walk(get(k, envir = v))
    }
  }
  unique(unlist(acc, use.names = FALSE))
}

# The text of a package's inst/ files (YAML, CSV, markdown ...), and the
# strings its YAML files hold; the app's own translations left out
.i18n_inst_texts <- function(pkg) {
  root <- system.file(package = pkg)
  fs <- list.files(root, recursive = TRUE, full.names = TRUE)
  rel <- substring(fs, nchar(root) + 2L)
  fs <- fs[!grepl("^(R|help|html|Meta|doc|tests|libs|i18n)/", rel) &
             grepl("[.](ya?ml|csv|md|txt|json|R|Rmd)$", rel, ignore.case = TRUE)]
  txt <- vapply(fs, function(f) {
    paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }, "", USE.NAMES = FALSE)
  ok <- validUTF8(txt)
  ys <- unlist(lapply(fs[ok & grepl("[.]ya?ml$", fs)], function(f) {
    y <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    u <- unlist(y)
    c(as.character(u), names(u))
  }), use.names = FALSE)
  # (a CSV cell's quotes, doubled, as the text has them)
  c(txt[ok], gsub("\"\"", "\"", txt[ok & grepl("[.]csv$", fs)], fixed = TRUE), ys)
}

# The keys the app builds at run time: a prefix, then a value of a catalog
# the app shows ("method:<name>" ...).  Each prefix with the values it may
# take, from the installed packages.
.i18n_runtime_keys <- function() {
  std <- company_standards()
  a <- utils::read.csv(system.file("ard", "args.csv", package = "tflspec"),
                       stringsAsFactors = FALSE, na.strings = "", fileEncoding = "UTF-8")
  f <- tflspec::tfl_ard_functions(installed = FALSE)
  hints <- unlist(lapply(f$call, function(cl) {
    tryCatch(tflspec::tfl_ard_args(cl)$hint, error = function(e) NULL)
  }), use.names = FALSE)
  list(
    # srv_study.R method_label() / method_note(): the standards' methods
    "method:" = std$ard_methods$method,
    "method-note:" = std$ard_methods$method,
    # srv_ard_make.R / srv_ard_analysis.R: the standards' statistics' labels
    "stat-label:" = std$ard_statistics$label,
    # srv_ard_make.R fn_entries_make(): the ARD functions' catalog
    "fn:" = f$call,
    "fn-note:" = f$call,
    # srv_ard_make.R arg_hint(): the catalog's argument hints
    "arg-hint:" = c(a$hint, hints),
    # ai_draft_ui.R .ai_review_label(): the kinds of reports
    "ai-review:" = c("Tables", "Figures", "Listings"),
    # review_ui.R: a review finding's sheet
    "review-scope:" = NULL)
}

# Whether each key is a text the app can show: a string of tflplanner's,
# tflspec's or rtfreporter's code, in tflplanner's or tflspec's inst/ files
# (whitespace as one space: YAML folds its lines), a tflspec text the app
# shows, or a run-time key whose value its catalog has (the value found
# among those texts, for a prefix with no catalog here).
.i18n_keys_used <- function(keys) {
  cs <- unique(c(.i18n_constants("tflplanner"), .i18n_constants("tflspec"),
                 .i18n_constants("rtfreporter"), .i18n_tflspec_texts()$text))
  norm <- function(x) gsub("[[:space:]]+", " ", x)
  big <- norm(paste(c(cs, .i18n_inst_texts("tflplanner"), .i18n_inst_texts("tflspec")),
                    collapse = "\001"))
  found <- function(k) k %in% cs | vapply(k, function(x) grepl(norm(x), big, fixed = TRUE), NA)
  used <- keys %in% cs
  rest <- which(!used)
  rk <- .i18n_runtime_keys()
  rx <- paste0("^(", paste(names(rk), collapse = "|"), ")")
  for (i in rest) {
    k <- keys[i]
    if (grepl(rx, k)) {
      p <- regmatches(k, regexpr(rx, k))
      v <- substring(k, nchar(p) + 1L)
      used[i] <- if (is.null(rk[[p]])) found(v) else v %in% rk[[p]]
    } else {
      used[i] <- found(k)
    }
  }
  used
}
