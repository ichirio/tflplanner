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
  p <- add_output(new_planner(), "T-DM", type = "table")
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
    session$elapse(1000)
    expect_true(.same_header(sheet_rows(rv$p, "col_header", "T-DM"),
                             header_presets()[["Arm / (N=n)"]]))
    h <- output[[b("hdr_ui")]]$html
    expect_match(h, "Line 2", fixed = TRUE)
    expect_match(h, "{n:sum}", fixed = TRUE)
    # {n} is used: whose {n} is asked
    expect_match(h, "Whose {n}", fixed = TRUE)
    # a line's text changed
    v3 <- list(); v3[[paste0(b("h2"), "_text")]] <- "N={n}"
    do.call(session$setInputs, v3)
    session$elapse(1000)
    ch <- sheet_rows(rv$p, "col_header", "T-DM")
    expect_identical(ch$text[ch$line == "2" & ch$cols == ".values"], "N={n}")
    # a line removed
    v4 <- list(); v4[[b("hdr_act")]] <- list(i = 1L, act = "del", t = 1)
    do.call(session$setInputs, v4)
    session$elapse(1000)
    ch <- sheet_rows(rv$p, "col_header", "T-DM")
    expect_identical(unique(ch$line), "1")
    expect_identical(ch$text[ch$cols == ".values"], "N={n}")
  })
})
