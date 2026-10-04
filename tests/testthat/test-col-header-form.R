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
    session$setInputs(target = "T-DM", nav = "outputs", rep_nav = "content",
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
    # {n} is used: whose {n} is asked
    expect_match(h, "Whose {n}", fixed = TRUE)
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
  })
})
