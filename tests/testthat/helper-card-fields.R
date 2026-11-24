# The cards' fields in tests: a card's fields filled as an item says, an
# input's value as the drawn page shows it, the inputs a drawn card sends,
# and the values a field takes (field_samples()).  Used by
# test-card-roundtrip.R and test-card-all-fields.R.

# a card's fields filled as its item says (as the page would hold them):
# test-spec-card.R's inputs_of()
inputs_of <- function(fn, args, id = "c", defs = spec_card_defs(fn)) {
  inp <- list()
  for (a in setdiff(defs$formals, .data_args)) {
    v <- args[[a]]
    kind <- .arg_kind(defs$args[[a]]$type)
    if (.is_unset(v)) {
      inp[[paste0(id, "__", a, "__none")]] <- TRUE
      next
    }
    if (isTRUE(defs$args[[a]]$dots)) v <- .spec_dots_text(fn, a, v)
    if (identical(kind, "logical")) v <- isTRUE(v)
    if (is.null(v)) next
    inp[[paste0(id, "__", a)]] <- if (is.logical(v)) v else
      if (kind %in% c("r", "string") && !is.character(v)) .arg_to_r(v) else
        if (inherits(v, "tfl_r")) as.character(unclass(v)) else v
  }
  inp
}

# HTML's entities back to their characters
.html_unq <- function(x) {
  for (p in list(c("&quot;", "\""), c("&#39;", "'"), c("&lt;", "<"), c("&gt;", ">"),
                 c("&#10;", "\n"), c("&#13;", "\r"), c("&#9;", "\t"), c("&amp;", "&"))) {
    x <- gsub(p[[1L]], p[[2L]], x, fixed = TRUE)
  }
  x
}

# an input's value as the page shows it (the rendered HTML): a text or
# number field its value ("" none), a check whether it is ticked, a select
# its selected option ("" none); NULL: no such field
shown_value <- function(h, fid) {
  sel <- regmatches(h, regexpr(sprintf('(?s)<select[^>]*id="%s".*?</select>', fid), h, perl = TRUE))
  if (length(sel)) {
    o <- regmatches(sel, regexpr('<option value="[^"]*" selected', sel))
    return(if (length(o)) .html_unq(sub('^<option value="([^"]*)".*$', "\\1", o)) else "")
  }
  ta <- regmatches(h, regexpr(sprintf('(?s)<textarea[^>]*id="%s"[^>]*>.*?</textarea>', fid), h, perl = TRUE))
  if (length(ta)) return(.html_unq(sub("(?s)^<textarea[^>]*>(.*)</textarea>$", "\\1", ta, perl = TRUE)))
  m <- regmatches(h, regexpr(sprintf('<input[^>]*id="%s"[^>]*>', fid), h))
  if (!length(m)) return(NULL)
  if (grepl('type="checkbox"', m, fixed = TRUE)) return(grepl("checked", m, fixed = TRUE))
  v <- regmatches(m, regexpr('value="[^"]*"', m))
  if (!length(v)) "" else .html_unq(sub('^value="(.*)"$', "\\1", v))
}

# a select's selected options, all of them (a select of several)
.selected_all <- function(h, fid) {
  sel <- regmatches(h, regexpr(sprintf('(?s)<select[^>]*id="%s".*?</select>', fid), h, perl = TRUE))
  if (!length(sel)) return(NULL)
  o <- regmatches(sel, gregexpr('<option value="[^"]*" selected', sel))[[1L]]
  .html_unq(sub('^<option value="([^"]*)".*$', "\\1", o))
}

# a table field's state as the page sends it (its data-state), NULL for none
.table_state <- function(h, fid) {
  m <- regmatches(h, regexpr(sprintf('(?s)<div class="rp-tbl [^"]*" id="%s".*?data-state="[^"]*"', fid),
                             h, perl = TRUE))
  if (!length(m)) return(NULL)
  .html_unq(sub('^.*data-state="([^"]*)"$', "\\1", m))
}

# What a drawn input sends as the page holds it before anyone touches it:
# a table its state, a select its selected options (a select of one ""
# for none, one of several NULL), a number its number (blank NULL), a
# check TRUE / FALSE, a text or a box its text; NULL: no such input
sent_value <- function(h, fid) {
  tb <- .table_state(h, fid)
  if (!is.null(tb)) return(tb)
  sa <- .selected_all(h, fid)
  if (!is.null(sa)) {
    sa <- sa[nzchar(sa)]
    several <- grepl(sprintf('<select[^>]*id="%s"[^>]*multiple', fid), h)
    return(if (length(sa)) sa else if (!several) "")
  }
  x <- shown_value(h, fid)
  if (is.character(x) && grepl(sprintf('<input[^>]*type="number"[^>]*id="%s"|<input[^>]*id="%s"[^>]*type="number"',
                                       fid, fid), h)) {
    return(if (nzchar(x)) as.numeric(x))
  }
  x
}

# The inputs a drawn card sends: each field's input (sent_value()) and its
# switches (`__none`, `__isr` and the R text `__r`)
sent_inputs <- function(h) {
  fids <- unique(sub('^data-field="(.*)"$', "\\1",
                     regmatches(h, gregexpr('data-field="[^"]*"', h))[[1L]]))
  inp <- list()
  put <- function(k, v) if (!is.null(v)) inp[k] <<- list(v)
  for (f in fids) {
    put(f, sent_value(h, f))
    for (s in c("none", "isr")) put(paste0(f, "__", s), shown_value(h, paste0(f, "__", s)))
    r <- shown_value(h, paste0(f, "__r"))
    if (is.character(r) && nzchar(r)) put(paste0(f, "__r"), r)
  }
  inp
}

# Does a drawn field show the value?  "(none)": its switch on (`none`: the
# input says whether it is); a check: ticked as the value says; any other:
# each of the value's entries (an R value: its R) in what the field shows
# -- its input, its R text, a table's state
field_shows <- function(h, fid, v, none = isTRUE(shown_value(h, paste0(fid, "__none")))) {
  if (.is_unset(v)) return(none)
  shown <- c(unlist(sent_value(h, fid)), unlist(shown_value(h, paste0(fid, "__r"))))
  if (is.logical(shown) && length(shown) == 1L && is.logical(v)) return(identical(shown, v))
  want <- if (inherits(v, c("tfl_r", "tfl_call"))) .arg_to_r(v) else as.character(unlist(v))
  txt <- paste(as.character(shown), collapse = "\n")
  length(want) > 0L && all(vapply(want, function(w) grepl(w, txt, fixed = TRUE), NA))
}

# a value as one text to compare: 40 and 40L the same, a vector and a list
# of the same entries the same, a list of one entry its entry; names kept;
# R and "(none)" told from text
value_key <- function(x) {
  tag <- function(x) {
    if (.is_unset(x)) return("<unset>")
    if (inherits(x, "tfl_r")) return(paste0("<r>", as.character(unclass(x))))
    if (inherits(x, "tfl_call")) return(paste0("<call>", .arg_to_r(x)))
    if (is.atomic(x) && !is.null(names(x))) x <- as.list(x)
    if (is.list(x) && is.null(names(x)) && length(x) == 1L) return(tag(x[[1L]]))
    if (is.list(x)) return(lapply(x, tag))
    x
  }
  as.character(jsonlite::toJSON(tag(x), auto_unbox = TRUE, digits = NA, null = "null"))
}

# The kind of field a definition draws, as arg_field() decides it (the
# class rp-field-<kind> it gives the field): field_look(), the card's word
# (R/field_ui.R) -- a def's input form over the type's kind, a table where
# the type has a table, a pick of text with nothing to pick from its text
# field -- so that this helper cannot drift from the field.  `dots`: the
# entries of a `...`.
field_kind <- function(def) {
  if (isTRUE(def$dots)) return("dots")
  field_look(def)
}

# ... and the class arg_field() gives that field (rp-field-<class>)
field_class <- function(kind) {
  switch(kind, pick_many = "list", pick_one = "column", pick_enum = "enum", r = , dots = "rexpr",
         condition = "rexpr rp-field-condition", kind)
}

# Two or three values of an argument, valid for its definition and none
# its default, by the kind of field it draws (field_kind()): a logical TRUE,
# FALSE and "(none)"; a number, a text, the first and the last of a pick's
# entries, R code, a list of columns or of numbers, a table's map, a `...`'s
# entries.  A template's parameter (its definition a tfl_param): by the
# input .tpl_type() gives it, the kind "tpl:<input>".  A kind not known here
# is an error naming the function and the argument: a new kind of field
# cannot slip past the walk.
field_samples <- function(def, fn = "?", arg = "?", kind = field_kind(def)) {
  R <- tflspec::tfl_r
  U <- tflspec::tfl_unset()
  type <- def$type %||% "r"
  alts <- .type_split(trimws(type), "|")
  first <- c(setdiff(alts, "r"), intersect(alts, "r"))[[1L]]
  inner <- sub("^(list|vec)<(.*)>$", "\\2", first)
  entries <- .def_entries(def, type)
  ch <- as.character(unlist(def$choices))
  pick2 <- function(x) as.list(unique(c(x[[1L]], x[[length(x)]])))
  out <- switch(
    kind,
    logical = list(TRUE, FALSE, U),
    number = list(2.5, 40),
    integer = list(3L, 12L),
    string = list("Fictional text", "Text {value} B"),
    template = list("{n} ({p})", "{mean:.1f}"),
    colour = list("red", "#CC0000"),
    column = , pick_one = if (length(entries)) as.list(unique(c(entries[[1L]], entries[[length(entries)]]))) else
      list("AGE", "TRT01A"),
    enum = , pick_enum = {
      if (!length(entries)) entries <- .type_enum(type)
      e <- unique(c(entries[[1L]], entries[[length(entries)]]))
      # (TRUE / FALSE of a type with a logical: the logicals)
      lapply(e, function(x) if ("logical" %in% alts && x %in% c("TRUE", "FALSE")) as.logical(x) else x)
    },
    list = , pick_many = {
      if (grepl("^(integer|number)", inner)) list(c(2L, 4L), 3L) else
        if (grepl("^column", inner)) list("AGE", c("TRT01A", "SEX")) else
          list("Line A", c("Line A", "Line B"))
    },
    call = {
      fns <- .type_call_fn(type)
      list(tflspec::tfl_call(fns[[1L]], list(top = "single")),
           tflspec::tfl_call(fns[[1L]], list(all = TRUE, inside_h = TRUE)))
    },
    r = list(R("toupper"), R("function(x) x")),
    condition = list(R("n >= 5"), R('AEBODSYS != "SOC X"')),
    dots = switch(def$dots_type %||% "",
                  r = , "list<r>" = list(list(R("function(x) x")), list(R("identity"), R("toupper"))),
                  list(c("Title A"), c("Title A", "Title B"))),
    table = .table_samples(type),
    # (a template's parameters)
    "tpl:population" = list("SAF", "ENR"),
    "tpl:data" = list("ADSL", "ADAE"),
    "tpl:var" = list("AGE", "SEX"),
    "tpl:vars" = list("AGE", c("SEX", "RACE")),
    "tpl:levels" = if (isTRUE(def$multiple)) list("Level A", c("Level A", "Level B")) else
      list("Level A", "Level B"),
    "tpl:flag" = list(TRUE, FALSE, U),
    "tpl:number" = list(3L, 2.5),
    "tpl:text" = list("Fictional text", "Text {value} B", U),
    "tpl:chips" = list("Chip A", c("Chip A", "Chip B")),
    "tpl:choice" = , "tpl:pick" = if (length(ch)) pick2(ch) else list("Pick A", "Pick B"),
    "tpl:choices" = , "tpl:picks" = if (length(ch)) list(ch[[1L]], unique(c(ch[[1L]], ch[[length(ch)]]))) else
      list("Pick A", c("Pick A", "Pick B")),
    # (records, a line each, where it takes several and names its fields;
    # else a map of names and texts)
    "tpl:list" = if (isTRUE(def$multiple) && length(def$fields)) {
      f <- as.character(def$fields)
      rec <- function(i) stats::setNames(as.list(paste("Value", i, seq_along(f))), f)
      list(list(rec(1L)), list(rec(1L), rec(2L)))
    } else {
      list(list(A = "Text A"), list(A = "Text A", B = "Text B"))
    },
    "tpl:columns" = list(list(list(vars = "USUBJID", label = "Subject")),
                         list(list(vars = "USUBJID", label = "Subject"),
                              list(vars = "AEDECOD", label = "Term", width = 30L))),
    "tpl:r" = list(R("AGE >= 18"), R('SEX == "F"')),
    "tpl:table" = .table_samples(def$def$type %||% type),
    stop(sprintf("%s(%s = ): a field of kind '%s' (type %s) the walk has no values for", fn, arg,
                 kind, type), call. = FALSE))
  # (none its default; "(none)" only where the argument may be left out)
  d <- def$default
  no_unset <- isTRUE(def$required) || identical(def$unset, "stop")
  keep <- vapply(out, function(v) (is.null(d) || !identical(v, d)) && !(no_unset && .is_unset(v)), NA)
  out[keep]
}

# a table field's values, by its type: a map of a name and its columns,
# named numbers, a list of named pairs
.table_samples <- function(type) {
  t1 <- .type_split(trimws(type), "|")[[1L]]
  if (grepl("^map<string, list<column>", t1)) {
    return(list(list(subgroup = c("ALL", "SEX")), list(by_sex = c("SEX"), by_age = c("AGEGR1", "RACE"))))
  }
  if (grepl("^vec<column, number>", t1)) return(list(list(TRT01A = 2, AGE = 1), list(AGE = 1.5)))
  if (grepl("^vec<string, column>", t1)) return(list(list(n = "N"), list(n = "N", pct = "PCT")))
  if (grepl("^list<vec<string, string>>", t1)) {
    return(list(list(c(a = "Header A")), list(c(a = "Header A"), c(b = "Header B"))))
  }
  if (grepl("^string", t1)) return(list("Courier New", "Arial"))
  stop(sprintf("a table of type %s the walk has no values for", type), call. = FALSE)
}
