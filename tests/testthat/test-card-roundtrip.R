# Round trips of the generated cards (R/spec_card.R): an item's arguments
# drawn as a card, read back as the page sends its fields, give the same
# arguments; one field changed as the page would send it reads back as the
# new value, and the card drawn from that value shows it again.  These pin
# the cards' behaviour: a field is drawn from its definition only, and reads
# back what it shows.
#
# (test-spec-card.R's helpers live in that file, not in a helper file: the
# few needed here are below, written the same way)

html <- function(...) as.character(htmltools::tagList(...))

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

# an input's value as the page shows it (the rendered HTML): a text or
# number field its value ("" none), a check whether it is ticked, a select
# its selected option ("" none); NULL: no such field
shown_value <- function(h, fid) {
  unq <- function(x) {
    x <- gsub("&quot;", '"', x, fixed = TRUE)
    x <- gsub("&#39;", "'", x, fixed = TRUE)
    x <- gsub("&lt;", "<", x, fixed = TRUE)
    x <- gsub("&gt;", ">", x, fixed = TRUE)
    gsub("&amp;", "&", x, fixed = TRUE)
  }
  sel <- regmatches(h, regexpr(sprintf('(?s)<select[^>]*id="%s".*?</select>', fid), h, perl = TRUE))
  if (length(sel)) {
    o <- regmatches(sel, regexpr('<option value="[^"]*" selected', sel))
    return(if (length(o)) unq(sub('^<option value="([^"]*)".*$', "\\1", o)) else "")
  }
  ta <- regmatches(h, regexpr(sprintf('(?s)<textarea[^>]*id="%s"[^>]*>.*?</textarea>', fid), h, perl = TRUE))
  if (length(ta)) return(unq(sub("(?s)^<textarea[^>]*>(.*)</textarea>$", "\\1", ta, perl = TRUE)))
  m <- regmatches(h, regexpr(sprintf('<input[^>]*id="%s"[^>]*>', fid), h))
  if (!length(m)) return(NULL)
  if (grepl('type="checkbox"', m, fixed = TRUE)) return(grepl("checked", m, fixed = TRUE))
  v <- regmatches(m, regexpr('value="[^"]*"', m))
  if (!length(v)) "" else unq(sub('^value="(.*)"$', "\\1", v))
}

# One round trip: the item's card drawn; its fields as the item says, the
# fields `fids` (field id suffixes, "c__<suffix>") as the card shows them;
# read back.  Returns the card's HTML, the inputs and the item's arguments
# after the read.
round_trip <- function(fn, args, fids = character(), ctx = list(), defs = spec_card_defs(fn),
                       set = list()) {
  it <- list(fn = fn, args = args, own_args = args)
  h <- html(spec_card("c", fn, it, ctx = ctx, defs = defs))
  inp <- inputs_of(fn, args, defs = defs)
  for (f in fids) {
    v <- shown_value(h, paste0("c__", f))
    inp[paste0("c__", f)] <- list(if (identical(v, "")) NULL else v)
  }
  for (f in names(set)) inp[paste0("c__", f)] <- list(set[[f]])
  r <- spec_card_read(inp, "c", fn, ctx = ctx, defs = defs)
  list(html = h, input = inp, read = r, args = spec_card_args(r, it, fn, defs = defs))
}

# no edit: the same arguments, the same R and the same "(none)"s
expect_same_args <- function(fn, got, want, info = NULL) {
  expect_identical(got, want, info = info)
  expect_identical(args_to_r(fn, got), args_to_r(fn, want), info = info)
  expect_true(.args_same_r(fn, got, want), info = info)
}

# a table field's state as the page sends it (its data-state), "" for none
table_state_of <- function(h, a) {
  m <- regmatches(h, regexpr(sprintf('(?s)<div class="rp-tbl [^"]*" id="c__%s".*?data-state="[^"]*"', a),
                             h, perl = TRUE))
  if (!length(m)) return("")
  x <- sub('^.*data-state="([^"]*)"$', "\\1", m)
  for (p in list(c("&quot;", "\""), c("&#39;", "'"), c("&lt;", "<"), c("&gt;", ">"),
                 c("&#10;", "\n"), c("&#13;", "\r"), c("&#9;", "\t"), c("&amp;", "&"))) {
    x <- gsub(p[[1L]], p[[2L]], x, fixed = TRUE)
  }
  x
}

# a table round trip: the item's card drawn, its table field `a` sent as
# drawn -- or with its rows changed by `edit` (a function of the state's
# rows) -- and read back
table_trip <- function(fn, args, a, edit = NULL) {
  it <- list(fn = fn, args = args, own_args = args)
  h <- html(spec_card("c", fn, it))
  st <- table_state_of(h, a)
  if (!is.null(edit)) {
    s <- jsonlite::fromJSON(st, simplifyVector = FALSE)
    s$rows <- edit(s$rows)
    st <- as.character(jsonlite::toJSON(s, auto_unbox = TRUE))
  }
  inp <- inputs_of(fn, args)
  inp[[paste0("c__", a)]] <- st
  r <- spec_card_read(inp, "c", fn)
  list(html = h, state = st, read = r, args = spec_card_args(r, it, fn))
}

test_that("plan_columns(widths = ): named widths round-trip in the table field, an edit reads back", {
  skip_on_cran()
  fn <- "plan_columns"
  w <- list(TRT01A = 2, AGE = 1)
  rt <- table_trip(fn, list(widths = w), "widths")
  # drawn: the definition's table (a column and its width), a row a width
  expect_match(rt$html, 'class="rp-tbl rp-tbl-map" id="c__widths"', fixed = TRUE)
  expect_false(grepl('id="c__widths__1"', rt$html, fixed = TRUE))
  expect_same_args(fn, rt$args, list(widths = w))
  # one edit: AGE's width 1.5
  ed <- table_trip(fn, list(widths = w), "widths", edit = function(rows) {
    lapply(rows, function(r) if (identical(r[[1L]], "AGE")) list("AGE", "1.5") else r)
  })
  expect_identical(ed$args$widths, list(TRT01A = 2, AGE = 1.5))
  again <- table_trip(fn, ed$args, "widths")
  expect_match(again$state, "1.5", fixed = TRUE)
  expect_same_args(fn, again$args, ed$args)
})

test_that("plan_columns(widths = ): unnamed widths are kept with no edit", {
  skip_on_cran()
  fn <- "plan_columns"
  w <- c(2, 1, 1)
  rt <- table_trip(fn, list(widths = w), "widths")
  # (a width with no column: a row with a blank column, in its order)
  expect_same_args(fn, rt$args, list(widths = w))
  # one edit: the second width 1.5
  ed <- table_trip(fn, list(widths = w), "widths", edit = function(rows) {
    rows[[2L]][[2L]] <- "1.5"
    rows
  })
  expect_identical(ed$args$widths, c(2, 1.5, 1))
  # a row given a column: the widths become the columns' named
  nm <- table_trip(fn, list(widths = w), "widths", edit = function(rows) {
    c(rows, list(list("TRT01A", "3")))
  })
  expect_identical(nm$args$widths, list(TRT01A = 3))
})

test_that("plan_filter(where = ): one condition and several round-trip through the condition builder", {
  skip_on_cran()
  fn <- "plan_filter"
  R <- tflspec::tfl_r
  # (where is not a field of the generated card: the condition builder's.
  # The card reads the rest back and leaves where as it is)
  expect_false("where" %in% spec_card_fields(fn)$name)
  one <- list(where = R("n >= 5"), any = TRUE)
  several <- list(where = list(R("n >= 5"), R('AEBODSYS != "SOC X"')), any = FALSE)
  for (args in list(one, several)) {
    rt <- round_trip(fn, args)
    expect_false(grepl('data-arg="where"', rt$html, fixed = TRUE))
    expect_same_args(fn, rt$args, args)
  }
  # the builder's text: one condition as it is, several joined by &; the
  # builder giving back the same text is no change (srv_builder.R)
  expect_identical(.filter_where_text(one$where), "n >= 5")
  expect_identical(.filter_where_text(several$where), '(n >= 5) & (AEBODSYS != "SOC X")')
  # one edit: the condition changed -- written as one R condition
  e <- "n >= 10"
  ed <- utils::modifyList(several, list(where = R(e)))
  expect_identical(ed$where, R("n >= 10"))
  expect_identical(.filter_where_text(ed$where), e)
  rt2 <- round_trip(fn, ed)
  expect_same_args(fn, rt2$args, ed)
  expect_identical(args_to_r(fn, ed), args_to_r(fn, list(where = R("n >= 10"), any = FALSE)))
})

test_that("an optional logical (plan_cell_style(bold = )): unset, TRUE and FALSE round-trip", {
  skip_on_cran()
  fn <- "plan_cell_style"
  base <- list(cols = "row_label")
  for (b in list(NULL, TRUE, FALSE)) {
    args <- c(base, if (!is.null(b)) list(bold = b))
    rt <- round_trip(fn, args, fids = "bold")
    expect_identical(shown_value(rt$html, "c__bold"), if (is.null(b)) "" else as.character(b),
                     info = format(b))
    expect_identical(args_to_r(fn, rt$args), args_to_r(fn, args), info = format(b))
    expect_identical(rt$args, args, info = format(b))
  }
  # one edit: unset -> FALSE, read back and shown
  ed <- round_trip(fn, base, set = list(bold = "FALSE"))
  expect_match(args_to_r(fn, ed$args), "bold = FALSE", fixed = TRUE)
  again <- round_trip(fn, ed$args, fids = "bold")
  expect_identical(shown_value(again$html, "c__bold"), "FALSE")
  expect_identical(args_to_r(fn, again$args), args_to_r(fn, ed$args))
})

test_that("a plain logical (plan_stub(before = )): unset, TRUE and FALSE round-trip", {
  skip_on_cran()
  fn <- "plan_stub"
  expect_identical(spec_card_defs(fn)$args$before$type, "logical")
  base <- list(name = "row_label")
  for (b in list(NULL, TRUE, FALSE)) {
    args <- c(base, if (!is.null(b)) list(before = b))
    rt <- round_trip(fn, args, fids = "before")
    expect_identical(shown_value(rt$html, "c__before"), isTRUE(b), info = format(b))
    # (unticked at the default FALSE: no change, the argument not added)
    expect_same_args(fn, rt$args, args, info = format(b))
  }
  # one edit: ticked
  ed <- round_trip(fn, base, set = list(before = TRUE))
  expect_identical(ed$args$before, TRUE)
  again <- round_trip(fn, ed$args, fids = "before")
  expect_true(shown_value(again$html, "c__before"))
  expect_same_args(fn, again$args, ed$args)
})

test_that("normalize_ard(stack = ) with a cross layer round-trips", {
  skip_on_cran()
  fn <- "normalize_ard"
  # a layer of one key and a cross layer (AGEGR1 x SEX), no edit: the same
  # (the table's cells are lines of keys: a cross layer is shown as R, its
  # R switch on, so that nothing of it is lost)
  st <- list(subgroup = list("ALL", c("AGEGR1", "SEX")))
  rt <- round_trip(fn, list(stack = st), fids = c("stack__r", "stack__isr"))
  expect_identical(shown_value(rt$html, "c__stack__r"), 'list(subgroup = list("ALL", c("AGEGR1", "SEX")))')
  expect_true(shown_value(rt$html, "c__stack__isr"))
  expect_same_args(fn, rt$args, list(stack = st))
  # one edit of that R: the value it says
  ed <- round_trip(fn, list(stack = st), set = list(stack__isr = TRUE,
                                                     stack__r = 'list(subgroup = list("ALL", c("AGEGR1", "RACE")))'))
  expect_identical(ed$args$stack, list(subgroup = list("ALL", c("AGEGR1", "RACE"))))
  # layers of one key each: one edit (a layer added) reads back
  st1 <- list(subgroup = c("ALL", "SEX"))
  ed <- table_trip(fn, list(stack = st1), "stack", edit = function(rows) {
    rows[[1L]][[2L]] <- "ALL\nSEX\nRACE"
    rows
  })
  expect_identical(ed$args$stack, list(subgroup = c("ALL", "SEX", "RACE")))
  again <- table_trip(fn, ed$args, "stack")
  expect_same_args(fn, again$args, ed$args)
})

test_that("plan_append: the part's card round-trips its child steps (name, data, content)", {
  skip_on_cran()
  args <- list(name = "summary", data = list(source = "ard"), content = .part_quickstart("ATPT"))
  it <- list(path = "plan_append", fn = "plan_append", args = args)
  h <- as.character(.part_body("c", it, lang = "en"))
  inp <- list(c__name = shown_value(h, "c__name"), c__absent = shown_value(h, "c__absent"),
              c__content = shown_value(h, "c__content"))
  expect_identical(inp$c__name, "summary")
  expect_identical(inp$c__absent, "error")
  back <- .part_args_read(inp, "c", it)
  expect_identical(names(back), names(args))
  expect_identical(back$data, args$data)
  expect_identical(vapply(back$content, function(i) i$fn, ""), vapply(args$content, function(i) i$fn, ""))
  expect_identical(.part_items_yaml(back$content), .part_items_yaml(args$content))
  # one edit: the part's table by AVISIT
  inp$c__content <- sub("ATPT", "AVISIT", inp$c__content, fixed = TRUE)
  ed <- .part_args_read(inp, "c", it)
  expect_identical(ed$content[[2L]]$args$cols, "AVISIT")
  it2 <- it
  it2$args <- ed
  h2 <- as.character(.part_body("c", it2, lang = "en"))
  expect_identical(shown_value(h2, "c__content"), inp$c__content)
})

test_that("plan_append: the generated card draws the child steps", {
  skip("plan_append child steps: not drawn yet (card step 2)")
  args <- list(name = "summary", data = list(source = "ard"), content = .part_quickstart("ATPT"))
  rt <- round_trip("plan_append", args)
  expect_same_args("plan_append", rt$args, args)
})

test_that("plan_titles(pages = ): a page of several lines is shown as R and reads back as the value", {
  skip_on_cran()
  fn <- "plan_titles"
  args <- list(pages = list(c("Table 1", "Group A"), c("Table 2", "Group B")))
  rt <- round_trip(fn, args, fids = c("pages__r", "pages__isr"))
  expect_identical(shown_value(rt$html, "c__pages__r"), .arg_to_r(args$pages))
  expect_true(shown_value(rt$html, "c__pages__isr"))
  expect_same_args(fn, rt$args, args)
  # one edit of that R: the value it says (the type takes no R)
  txt <- sub("Group B", "Group C", .arg_to_r(args$pages), fixed = TRUE)
  ed <- round_trip(fn, args, set = list(pages__isr = TRUE, pages__r = txt))
  expect_identical(ed$args$pages, list(c("Table 1", "Group A"), c("Table 2", "Group C")))
})

test_that("plan_footnotes(pages = ), plan_columns(column_widths_twips = ): a list typed, one entry each", {
  skip_on_cran()
  # a select of several that takes a value typed (input text on a list<..>)
  h <- html(spec_card("c", "plan_footnotes", list(fn = "plan_footnotes", args = list(pages = list("Note A", "Note B")))))
  expect_match(h, '<select[^>]*id="c__pages"[^>]*multiple', perl = TRUE)
  for (case in list(list(fn = "plan_footnotes", a = "pages", v = list("Note A", "Note B"),
                         sent = c("Note A", "Note B"), edit = c("Note A", "Note C"),
                         want = c("Note A", "Note C")),
                    list(fn = "plan_columns", a = "column_widths_twips", v = c(3000L, 1500L, 1500L),
                         sent = c("3000", "1500", "1500"), edit = c("2500", "2000"),
                         want = c(2500L, 2000L)))) {
    it <- list(fn = case$fn, args = stats::setNames(list(case$v), case$a))
    it$own_args <- it$args
    inp <- stats::setNames(list(case$sent), paste0("c__", case$a))
    # (the page has its main fields; here the field itself tells the card is there)
    d <- spec_card_defs(case$fn)
    d$visible <- case$a
    r <- spec_card_read(inp, "c", case$fn, defs = d)
    got <- spec_card_args(r, it, case$fn, defs = d)
    expect_identical(args_to_r(case$fn, got), args_to_r(case$fn, it$args), info = case$a)
    inp[[1L]] <- case$edit
    ed <- spec_card_args(spec_card_read(inp, "c", case$fn, defs = d), it, case$fn, defs = d)
    expect_identical(as.vector(unlist(ed[[case$a]])), case$want, info = case$a)
  }
})

test_that("a logical among other types is a pick of the definition's values, read back as its type says", {
  skip_on_cran()
  # plan_listing(wrap = ): logical|string, a pick -- not a check
  d <- spec_card_defs("plan_listing")
  h <- html(spec_card("c", "plan_listing", list(fn = "plan_listing", args = list(wrap = FALSE))))
  expect_match(h, '<select[^>]*id="c__wrap"', perl = TRUE)
  expect_false(grepl('<input type="checkbox" class="form-check-input" id="c__wrap"', h, fixed = TRUE))
  rd <- function(v, a = "wrap") builder_card_read(stats::setNames(list(v), paste0("c__", a)), "c",
                                                  schema = d, args = a)[[a]]
  expect_false(rd("FALSE"))
  expect_true(rd("TRUE"))
  expect_identical(rd("word"), "word")
  expect_null(rd(""))
  # rtf_border()'s sides: TRUE / FALSE and the line styles
  b <- spec_card_defs("rtf_border")
  hb <- html(spec_card("c", "rtf_border", list(fn = "rtf_border", args = list(top = "single"))))
  expect_match(hb, '<select[^>]*id="c__top"', perl = TRUE)
  expect_match(hb, '<option value="single" selected', fixed = TRUE)
  rb <- function(v) builder_card_read(list(c__top = v), "c", schema = b, args = "top")$top
  expect_identical(rb("single"), "single")
  expect_true(rb("TRUE"))
  # a pick left blank is not FALSE: nothing written
  it <- list(fn = "plan_listing", args = list(), own_args = list())
  r <- spec_card_read(list(c__wrap = "", c__type = ""), "c", "plan_listing")
  expect_null(spec_card_args(r, it, "plan_listing")$wrap)
})

test_that("a type with a call has the R switch (a call is written as R)", {
  skip_on_cran()
  expect_true(.type_takes_r("string|call<rtf_border>"))
  expect_true(.type_takes_r("call<position_dodge>"))
  expect_false(.type_takes_r("string|integer"))
  h <- html(spec_card("c", "plan_style", list(fn = "plan_style", args = list(border = "tfl"))))
  expect_match(h, 'id="c__border__isr"', fixed = TRUE)
})
