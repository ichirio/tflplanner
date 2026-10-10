test_that("the presets read into the form's lines and back unchanged", {
  hp <- header_presets()
  for (nm in names(hp)) {
    d <- hp[[nm]]
    l <- header_read(d, "TRT01A")
    expect_false(is.null(l), label = nm)
    expect_true(.same_header(d, header_write(l)), label = nm)
  }
  l <- header_read(hp[["Arm / (N=n)"]], "TRT01A")
  expect_identical(l[[2]]$stub, c(row_label = "Characteristic"))
  expect_identical(l[[2]]$mode, "each")
  expect_identical(l[[2]]$text, "(N={n})")
})

test_that("a spanner over a key, merged row-header columns, a line with no value cell", {
  d <- data.frame(line = c("1", "1", "2", "2"),
                  cols = c("a | b", ".values", "a", "b"),
                  span = c(NA, "TRT01A", NA, NA),
                  text = c("Parameter", "{col1}", "Name", "Unit"),
                  bold = c(NA, "TRUE", NA, NA),
                  border_bottom = c(NA, "single", NA, NA))
  l <- header_read(d, c("TRT01A", "SEX"))
  expect_true(l[[1]]$merge)
  expect_identical(l[[1]]$mode, "key")
  expect_identical(l[[1]]$key, "TRT01A")
  expect_identical(l[[2]]$mode, "none")
  expect_identical(names(l[[2]]$stub), c("a", "b"))
  expect_true(.same_header(d, header_write(l)))
})

test_that("a header the form cannot show is left to the sheet", {
  expect_null(header_read(data.frame(line = "1", cols = "3:5", span = NA, text = "x")))
  expect_null(header_read(data.frame(line = "1", cols = "TRT01A = Placebo", span = NA,
                                     text = "x")))
  # a styled row-header cell
  expect_null(header_read(data.frame(line = "1", cols = "row_label", span = NA,
                                     text = "x", bold = "TRUE")))
  # two value cells on one line
  expect_null(header_read(data.frame(line = c("1", "1"), cols = ".values",
                                     span = c("each", NA), text = c("a", "b"))))
  expect_identical(header_read(NULL), list())
})

test_that("a token's chip says what it holds in the preview", {
  tk <- data.frame(token = c("{col}", "{n}"), resolved = c(TRUE, FALSE))
  tk$values <- list(c(A = "Placebo", B = "Drug"), NULL)
  l <- header_token_labels(c("{col}", "{n}", "{n:sum}"), tk)
  expect_identical(unname(l), c("{col}", "{n}", "{n:sum}"))
  expect_identical(names(l), c("{col} = Placebo / Drug", "{n}", "{n:sum}"))
  expect_identical(names(header_token_labels("{col}")), "{col}")
})

test_that("the tokens are the table's", {
  expect_identical(header_token_choices("TRT01A"), c("{col}", "{n}", "{n:sum}"))
  expect_true(all(c("{col1}", "{col2}", "{n2}") %in%
                    header_token_choices(c("TRT01A", "SEX"))))
  expect_true("{N}" %in% header_token_choices("TRT01A", "n = page | N = table"))
  expect_true(header_uses_n(list(list(text = "(N={n})", stub = c(row_label = NA)))))
  expect_false(header_uses_n(list(list(text = "{col}", stub = c(row_label = NA)))))
})

test_that("the builder's header form writes the report's col_header", {
  skip_on_cran()
  skip_if_not_installed("cards")
  local_home()
  # the company defaults (the row-header column the preview's header needs)
  p <- add_standard_defaults(add_output(new_planner(), "T-DM", type = "table"), "H1")
  s <- create_study("H1", planner = p)
  ard <- cards::ard_stack(cards::ADSL, .by = TRT01A,
                          cards::ard_continuous(variables = AGE))
  data <- rtfreporter::normalize_ard(ard)
  m <- ard_meta(ard, data)
  f <- .meta_file(s, "T-DM")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(m, f)
  saveRDS(data, sub("[.]rds$", "_data.rds", f))
  shiny::testServer(server_for("H1"), {
    rv <- session$userData$rv
    bform <- session$userData$bform
    session$setInputs(target = "T-DM", nav = "make", step = "content", content_nav = "content",
                      table_nav = "builder")
    b <- function(x) paste0("b", bform$n, "_", x)
    v <- list()
    v[[b("key")]] <- "TRT01A"
    v[[b("vars")]] <- "AGE"
    v[[b("arms_TRT01A")]] <- m$keys$TRT01A
    v[[b("stats")]] <- c("n", "mean_sd")
    v[[b("dec")]] <- 0
    v[[b("cat")]] <- "npct"
    v[[b("pct")]] <- 1
    v[[b("lab1")]] <- ""
    do.call(session$setInputs, v)
    # a preset into the lines, then the lines into col_header
    v2 <- list(); v2[[b("hdr_preset")]] <- "Arm / (N=n)"
    do.call(session$setInputs, v2)
    # a preset replaces the lines only when confirmed
    v2 <- list(); v2[[b("hdr_preset_ok")]] <- 1
    do.call(session$setInputs, v2)
    session$elapse(1000)
    # the header as the report has it: its own rows, else the study's (the
    # preset is the study default here, so nothing of its own is written)
    eff <- function() {
      own <- sheet_rows(rv$p, "col_header", "T-DM")
      if (nrow(own)) own else inherited_rows(rv$p, "col_header", "T-DM")
    }
    expect_true(.same_header(eff(), header_presets()[["Arm / (N=n)"]]))
    h <- output[[b("hdr_ui")]]$html
    expect_match(h, "Line 2", fixed = TRUE)
    tk <- output[[b("hdr_tok")]]$html
    expect_match(tk, "{n:sum}", fixed = TRUE)
    # the preview's values beside the tokens
    expect_match(tk, "{col} = ", fixed = TRUE)
    # {n} is used: what it counts is asked, in a box of its own
    expect_match(h, "What the header's {n} counts", fixed = TRUE)
    expect_match(h, "rp-hdr-box", fixed = TRUE)
    # a line's tools come before its fields (its number on top of it)
    u1 <- bform$hdr[[1]]$uid
    at_tools <- regexpr("Line 1", h, fixed = TRUE)
    at_field <- regexpr(paste0(b(paste0("h", u1)), "_"), h, fixed = TRUE)
    expect_true(at_tools > 0 && at_field > 0 && at_tools < at_field)
    # the head: one button to load a standard header, no select
    expect_match(h, b("hdr_load"), fixed = TRUE)
    expect_false(grepl("From a preset", h, fixed = TRUE))
    # the insert chips in their box
    expect_match(tk, "rp-hdr-box", fixed = TRUE)
    # {n}: the ARD states one population here, chosen, with its values
    nh <- output[[b("hdr_n_ui")]]$html
    expect_match(nh, "Subjects, by TRT01A: ", fixed = TRUE)
    expect_match(nh, "checked", fixed = TRUE)
    # every field as the browser has it, by each line's own number
    fields <- function() {
      v <- list()
      for (l in bform$hdr) {
        id <- function(part) paste0(b(paste0("h", l$uid)), "_", part)
        for (k in seq_along(l$stub)) v[[id(paste0("stub", k))]] <-
          if (is.na(l$stub[[k]])) "" else l$stub[[k]]
        v[[id("mode")]] <- l$mode
        v[[id("key")]] <- if (is.na(l$key)) "TRT01A" else l$key
        v[[id("text")]] <- if (is.na(l$text)) "" else l$text
        v[[id("align")]] <- if (is.na(l$align)) "" else l$align
        v[[id("bold")]] <- isTRUE(as.logical(l$bold))
        v[[id("ul")]] <- !is.na(l$border_bottom)
      }
      do.call(session$setInputs, v)
    }
    fields()
    u2 <- bform$hdr[[2]]$uid
    # a line's text changed
    v3 <- list(); v3[[paste0(b(paste0("h", u2)), "_text")]] <- "N={n}"
    do.call(session$setInputs, v3)
    session$elapse(1000)
    ch <- eff()
    expect_identical(ch$text[ch$line == "2" & ch$cols == ".values"], "N={n}")
    # a line added above: the others keep what they say
    v4 <- list(); v4[[b("hdr_add")]] <- 1
    do.call(session$setInputs, v4)
    fields()
    session$elapse(1000)
    ch <- eff()
    expect_identical(ch$text[ch$cols == ".values"], c(NA, "{col}", "N={n}"))
    # the new line moved down: still its own text, the others theirs
    nu <- bform$hdr[[1]]$uid
    v5 <- list(); v5[[paste0(b(paste0("h", nu)), "_text")]] <- "Treatment"
    do.call(session$setInputs, v5)
    v6 <- list(); v6[[b("hdr_act")]] <- list(i = 1L, act = "down", t = 1)
    do.call(session$setInputs, v6)
    fields()
    session$elapse(1000)
    ch <- eff()
    expect_identical(ch$text[ch$cols == ".values"], c("{col}", "Treatment", "N={n}"))
    # a line removed
    v7 <- list(); v7[[b("hdr_act")]] <- list(i = 2L, act = "del", t = 2)
    do.call(session$setInputs, v7)
    fields()
    session$elapse(1000)
    ch <- eff()
    expect_identical(ch$text[ch$cols == ".values"], c("{col}", "N={n}"))
    # bold and underline ticked on line 1 (its value cells): in the sheet,
    # the preview, the RTF
    u1 <- bform$hdr[[1]]$uid
    v8 <- list()
    v8[[paste0(b(paste0("h", u1)), "_bold")]] <- TRUE
    v8[[paste0(b(paste0("h", u1)), "_ul")]] <- TRUE
    do.call(session$setInputs, v8)
    session$elapse(1000)
    ch <- eff()
    expect_identical(ch$bold[ch$line == "1" & ch$cols == ".values"], "TRUE")
    expect_identical(ch$border_bottom[ch$line == "1" & ch$cols == ".values"], "single")
    expect_true(all(is.na(ch$bold[ch$line == "2"])))
    pages <- preview_pages(rv$p, "T-DM", data)
    hd <- pages[[1]]$col_header[[1]]
    val <- hd[vapply(hd, function(x) as.integer(x$from) > 1L, NA)]
    expect_true(all(vapply(val, function(x) isTRUE(x$bold), NA)))
    pv <- as.character(preview_html(pages))
    th <- regmatches(pv, gregexpr("<th[^>]*>[^<]*", pv))[[1L]]
    lab <- val[[1L]]$label
    one <- th[grepl(sub("\n.*$", "", lab), th, fixed = TRUE)][1L]
    expect_match(one, "font-weight: bold;", fixed = TRUE)
    expect_match(one, "border-bottom: 1px solid", fixed = TRUE)
    expect_false(any(grepl("font-weight: bold", th[grepl("N=", th, fixed = TRUE)], fixed = TRUE)))
    f <- tempfile(fileext = ".rtf")
    rtfreporter::generate_rtfreport(
      rtfreporter::rtf_tables(rtfreporter::rtf_document(), pages), f)
    rtf <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_match(rtf, paste0("\\b ", sub("\n.*$", "", lab)), fixed = TRUE)
    expect_match(rtf, "\\clbrdrb", fixed = TRUE)
  })
})

test_that("a line cell by cell: read, written back, merged and split", {
  d <- data.frame(line = c("1", "1", "1", "2", "2"),
                  cols = c("row_label", "TRT01A = Placebo",
                           "TRT01A = Xanomeline Low Dose | TRT01A = Xanomeline High Dose",
                           "row_label", ".values"),
                  span = c(NA, NA, NA, NA, "each"),
                  text = c(NA, "Control", "Xanomeline", "Characteristic", "{col}"))
  l <- header_read(d, "TRT01A")
  expect_identical(l[[1]]$mode, "cells")
  expect_identical(l[[1]]$key, "TRT01A")
  expect_length(l[[1]]$segments, 2L)
  expect_identical(l[[1]]$segments[[2]]$levels,
                   c("Xanomeline Low Dose", "Xanomeline High Dose"))
  expect_true(.same_header(d, header_write(l)))
  expect_true(header_uses_n(l) == FALSE)
  lv <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
  # every value has its cell, in order
  s <- header_segments(l[[1]]$segments, lv)
  expect_identical(lapply(s, `[[`, "levels"),
                   list("Placebo", c("Xanomeline Low Dose", "Xanomeline High Dose")))
  # values not side by side are not one cell
  s <- header_segments(list(list(levels = c("Placebo", "Xanomeline High Dose"),
                                 text = "x")), lv)
  expect_length(s, 3L)
  expect_true(is.na(s[[2]]$text))
  # a blank cell writes nothing
  l[[1]]$segments[[1]]$text <- ""
  w <- header_write(l)
  expect_false(any(grepl("Placebo", w$cols)))
})

test_that("what {n} counts: the ARD's populations, each with its values", {
  # no ARD yet: the three in words
  ch <- header_n_choices(NULL, "TRT01A")
  expect_identical(unname(ch$choices), c("", "table", "n = page | N = table"))
  expect_identical(ch$selected, "")
  expect_match(ch$note, "not known yet")
  expect_identical(header_n_choices(NULL, "TRT01A", "page")$choices[[1L]], "page")
  # one population: it alone, chosen, header_n as it is
  one <- data.frame(scope = "all", page = NA, column = c("A", "B", NA),
                    value = c(86, 84, 170), stringsAsFactors = FALSE)
  attr(one, "differ") <- FALSE
  ch <- header_n_choices(one, "TRT01A")
  expect_identical(names(ch$choices), "Subjects, by TRT01A: 86 / 84")
  expect_identical(unname(ch$choices), "")
  expect_identical(header_n_choices(one, "TRT01A", "table")$selected, "table")
  # only the total stated: it
  tot <- one[3L, ]
  attr(tot, "differ") <- FALSE
  expect_match(names(header_n_choices(tot, "TRT01A")$choices), ": 170$")
  # a joined key's columns are not shown: the first key's
  two <- data.frame(scope = "all", page = NA,
                    column = c("A", "A____F", "A____M", "B"),
                    value = c(86, 40, 46, 84), stringsAsFactors = FALSE)
  attr(two, "differ") <- FALSE
  expect_match(names(header_n_choices(two, c("TRT01A", "SEX"))$choices),
               "by TRT01A \u00d7 SEX: 86 / 84$")
  # two populations that differ: each, both, nothing chosen and a warning
  pg <- data.frame(scope = c("page", "page", "page", "page", "table", "table"),
                   page = c("ALT", "ALT", "HGB", "HGB", NA, NA),
                   column = c("A", "B", "A", "B", "A", "B"),
                   value = c(22, 24, 20, 21, 25, 26), stringsAsFactors = FALSE)
  attr(pg, "differ") <- TRUE
  attr(pg, "page_col") <- "PARAMCD"
  ch <- header_n_choices(pg, "TRT01A")
  expect_identical(unname(ch$choices), c("page", "table", "n = page | N = table"))
  expect_identical(names(ch$choices)[1:2], c(
    "Subjects on each page (per PARAMCD, by TRT01A): 22 / 24 (the first page, ALT)",
    "Analysis set (by TRT01A, the same on every page): 25 / 26"))
  expect_null(ch$selected)
  expect_match(ch$warn, "different numbers")
  ch <- header_n_choices(pg, "TRT01A", "table")
  expect_identical(ch$selected, "table")
  expect_null(ch$warn)
  # the same numbers: one choice
  attr(pg, "differ") <- FALSE
  ch <- header_n_choices(pg, "TRT01A")
  expect_length(ch$choices, 1L)
  expect_match(names(ch$choices), "the same numbers", fixed = TRUE)
  # in Japanese
  ja <- function(x) tr(x, lang = "ja")
  expect_match(names(header_n_choices(one, "TRT01A", tr = ja)$choices), "\u88ab\u9a13\u8005\u6570")
})

test_that("what {n} counts, read from a page-split ARD as rtfreporter prints it", {
  skip_if_not_installed("cards")
  set.seed(1)
  lb <- expand.grid(USUBJID = cards::ADSL$USUBJID, PARAM = c("ALT", "HGB"),
                    stringsAsFactors = FALSE)
  lb$BASEGR <- sample(c("G0", "G1"), nrow(lb), TRUE)
  lb$WORSTGR <- sample(c("G0", "G1", "G2"), nrow(lb), TRUE)
  lb <- lb[!(lb$PARAM == "HGB" & seq_len(nrow(lb)) %% 10 == 0), ]
  d <- rtfreporter::normalize_ard(cards::bind_ard(
    cards::ard_categorical(lb, by = c(PARAM, BASEGR), variables = WORSTGR),
    cards::ard_categorical(lb, by = PARAM, variables = BASEGR),
    cards::ard_total_n(cards::ADSL)), drop_contexts = "attributes")
  plan <- rtfreporter::table_plan(d, cols = "BASEGR", rows = c(PARAM = "PARAM"),
                                  label = c(label = ".label")) |>
    rtfreporter::plan_cells("{n}") |>
    rtfreporter::plan_paginate_group(keep = FALSE)
  ch <- header_n_choices(rtfreporter::plan_n_candidates(plan), "BASEGR")
  expect_identical(unname(ch$choices), c("page", "table", "n = page | N = table"))
  expect_match(names(ch$choices)[1L], "per PARAM, by BASEGR", fixed = TRUE)
  expect_match(names(ch$choices)[1L], "(the first page, ALT)", fixed = TRUE)
  expect_match(ch$warn, "different numbers")
})

test_that("a preset's lines in short, for the list it is chosen from", {
  pr <- header_presets()
  smp <- vapply(pr, header_preset_sample, "")
  expect_true(all(nzchar(smp)))
  expect_false(any(grepl("\n", smp, fixed = TRUE)))
  expect_match(smp[["Arm / (N=n)"]], "{n}", fixed = TRUE)
  expect_identical(header_preset_sample(NULL), "")
})
