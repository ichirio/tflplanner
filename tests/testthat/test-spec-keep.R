# The GUI writes the definition; what it cannot show it must not change
# (design 13-2).  One test a form of steps 1, 3 and 4: a definition with
# values the form does not draw is read, shown, one other field changed
# and saved -- and those values are as they were.

# what rhandsontable sends when one cell of a grid drawn by the app is
# changed: the grid's own rows (as the browser has them), one cell new
grid_edit <- function(session, output, id, row, col, value) {
  w <- jsonlite::fromJSON(output[[id]], simplifyVector = FALSE)$x
  hdr <- unlist(w$colHeaders)
  j <- match(col, hdr)
  data <- w$data
  old <- data[[row]][[j]]
  data[[row]][[j]] <- value
  v <- list(list(
    data = data,
    changes = list(event = "afterChange",
                   changes = list(list(row - 1L, col, old, value))),
    params = list(planner_key = w$planner_key, rClass = "data.frame",
                  rColHeaders = as.list(hdr), rColClasses = w$rColClasses,
                  rDataDim = list(length(data), length(hdr)))))
  names(v) <- id
  do.call(session$setInputs, v)
}

# values a form or grid could take for something else: text that looks
# like a number or a logical, a line break, a bar, a token, a blank cell
.odd <- c("007", "TRUE", "a\nb", "x | y", "{STUDY}", "1.50", NA)

test_that("steps 1 and 4: a grid edit changes that cell and nothing else", {
  local_home()
  p <- add_output(new_planner(), "T1", type = "table", description = "one")
  p <- add_output(p, "T2", type = "table", description = "other")
  text_sheets <- list(
    codelists = function(n) data.frame(variable = "SEX", value = paste0("v", seq_len(n)),
                                       label = .odd[seq_len(n)], order = c("2", "1", NA)[seq_len(n)]),
    titles = function(n) data.frame(line = as.character(seq_len(n)), left = .odd[seq_len(n)],
                                    center = rev(.odd)[seq_len(n)], right = "r"),
    footnotes = function(n) data.frame(line = as.character(seq_len(n)), left = .odd[seq_len(n)]),
    header = function(n) data.frame(line = as.character(seq_len(n)), left = "{STUDY}",
                                    right = .odd[seq_len(n)]),
    footer = function(n) data.frame(line = as.character(seq_len(n)), center = .odd[seq_len(n)]),
    tokens = function(n) data.frame(name = paste0("TK", seq_len(n)), value = .odd[seq_len(n)]))
  for (sh in names(text_sheets)) {
    p <- set_sheet_rows(p, sh, "T1", text_sheets[[sh]](3L))
    p <- set_sheet_rows(p, sh, "T2", text_sheets[[sh]](2L))
    p <- set_sheet_rows(p, sh, NA, text_sheets[[sh]](1L))
  }
  # the page and the report: settings the app draws no field for
  p <- set_sheet_rows(p, "page", "T1", data.frame(orientation = "landscape",
                                                  margin_top_in = "1.25", markup = "TRUE"))
  p <- set_sheet_rows(p, "report", "T1", data.frame(type = "table", watermark = "DRAFT",
                                                    page_header = "FALSE"))
  create_study("S1", planner = p)
  shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    session$setInputs(target = "T1", nav = "make", step = "page")
    for (sh in c(names(text_sheets), "page", "report")) {
      before <- rv$p
      grid_id <- paste0("hot_", sh)
      d <- sheet_rows(before, sh, "T1")
      # one cell changed: the last column of the first row (the page and
      # the report: a setting of the right type)
      col <- setdiff(names(d), c("output_id", "note"))
      col <- col[length(col)]
      new <- "changed"
      if (sh == "page") { col <- "margin_bottom_in"; new <- "0.9" }
      if (sh == "report") { col <- "watermark"; new <- "FINAL" }
      grid_edit(session, output, grid_id, 1L, col, new)
      after <- sheet_rows(rv$p, sh, "T1")
      want <- d
      want[[col]][1L] <- new
      expect_identical(after, want, info = sh)
      # the other report's rows and the study defaults: untouched
      expect_identical(sheet_rows(rv$p, sh, "T2"), sheet_rows(before, sh, "T2"),
                       info = sh)
      expect_identical(sheet_rows(rv$p, sh, NA), sheet_rows(before, sh, NA),
                       info = sh)
    }
    edited <- rv$p
    session$setInputs(save = 1)
    saved <- open_study("S1")$planner
    for (sh in c(names(text_sheets), "page", "report")) {
      expect_identical(saved$sheets[[sh]], edited$sheets[[sh]], info = sh)
    }
  })
})

# a table study with an ARD read for the builder: AGE (continuous), SEX
# (categorical) by TRT01A
builder_study <- function(id, p) {
  s <- create_study("B1", planner = p)
  ard <- cards::ard_stack(cards::ADSL, .by = TRT01A,
                          cards::ard_continuous(variables = AGE),
                          cards::ard_categorical(variables = SEX))
  data <- rtfreporter::normalize_ard(ard)
  m <- ard_meta(ard, data)
  f <- .meta_file(s, id)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(m, f)
  saveRDS(data, sub("[.]rds$", "_data.rds", f))
  m
}

# every field of the builder's form as the browser has it once drawn
builder_show <- function(session, bform) {
  st <- bform$st
  b <- function(x) paste0("b", bform$n, "_", x)
  v <- list()
  v[[b("key")]] <- st$key
  v[[b("vars")]] <- st$variables$variable
  for (k in st$key) v[[b(paste0("arms_", make.names(k)))]] <- st$arms[[k]]
  v[[b("stats")]] <- st$stats
  v[[b("dec")]] <- st$decimals
  v[[b("cat")]] <- st$cat_format
  v[[b("pct")]] <- st$pct_decimals
  for (i in seq_len(nrow(st$variables))) {
    v[[b(paste0("lab", i))]] <- st$variables$label[i] %||% ""
    lv <- st$levels[[st$variables$variable[i]]]
    if (length(lv)) v[[b(paste0("lv", i))]] <- lv
  }
  do.call(session$setInputs, v)
  session$elapse(1000)
}

test_that("step 3, the table builder: what it cannot show stays as written", {
  skip_on_cran()
  skip_if_not_installed("cards")
  local_home()
  p <- add_output(new_planner(), "T-DM", type = "table")
  p <- set_sheet_rows(p, "tables", "T-DM", data.frame(
    cols = "TRT01A", rows = "group = variable", na = "-", sort = "TRUE"))
  p <- set_sheet_rows(p, "variables", "T-DM", data.frame(
    variable = c("AGE", "SEX"), label = c("Age (years)", NA),
    order = c("10", "20"), empty_levels = c(NA, "hide")))
  # a statistic's own template and digits, a condition, significant
  # digits; a categorical template that is none of the builder's formats
  p <- set_sheet_rows(p, "cells", "T-DM", data.frame(
    variable = c("continuous", "continuous", "continuous", "categorical"),
    row = c("n", "Mean (SD)", "Median", NA),
    when = c(NA, NA, "!is.na(median)", NA),
    template = c("{N}", "{mean} [{sd}]", "{median}", "{n} [{p}%]"),
    digits = c(NA, "2,3", "1", "0,1"),
    signif = c(NA, NA, "3", NA)))
  p <- set_sheet_rows(p, "cell_styles", "T-DM", data.frame(
    where = "label == \"Mean (SD)\"", bold = "TRUE"))
  p <- set_sheet_rows(p, "columns", "T-DM", data.frame(
    column = ".values", rel_width = "3"))
  builder_study("T-DM", p)
  shiny::testServer(server_for("B1"), {
    rv <- session$userData$rv
    bform <- session$userData$bform
    p0 <- rv$p
    session$setInputs(target = "T-DM", nav = "make", step = "content",
                      content_nav = "content", table_right = "result")
    expect_false(is.null(bform$st))
    # shown: nothing written
    builder_show(session, bform)
    for (sh in table_sheets()) {
      expect_identical(sheet_rows(rv$p, sh, "T-DM"), sheet_rows(p0, sh, "T-DM"),
                       info = paste("shown:", sh))
    }
    # one label changed: that label, nothing else
    i <- match("SEX", bform$st$variables$variable)
    v <- list()
    v[[paste0("b", bform$n, "_lab", i)]] <- "Sex"
    do.call(session$setInputs, v)
    session$elapse(1000)
    want <- sheet_rows(p0, "variables", "T-DM")
    want$label[want$variable == "SEX"] <- "Sex"
    expect_identical(sheet_rows(rv$p, "variables", "T-DM"), want)
    for (sh in setdiff(table_sheets(), "variables")) {
      expect_identical(sheet_rows(rv$p, sh, "T-DM"), sheet_rows(p0, sh, "T-DM"),
                       info = paste("label:", sh))
    }
    # the decimals changed: the digits follow, each statistic's own
    # template and condition stay
    v <- list()
    v[[paste0("b", bform$n, "_dec")]] <- bform$st$decimals + 1
    do.call(session$setInputs, v)
    session$elapse(1000)
    ce <- sheet_rows(rv$p, "cells", "T-DM")
    ce0 <- sheet_rows(p0, "cells", "T-DM")
    expect_identical(ce[c("variable", "row", "when", "template", "signif")],
                     ce0[c("variable", "row", "when", "template", "signif")])
    expect_false(identical(ce$digits, ce0$digits))
    edited <- rv$p
    session$setInputs(save = 1)
    saved <- open_study("B1")$planner
    for (sh in table_sheets()) {
      expect_identical(saved$sheets[[sh]], edited$sheets[[sh]], info = sh)
    }
  })
})

test_that("step 3, the listing form: what it cannot show stays as written", {
  local_home()
  p <- add_output(new_planner(), "L1", type = "listing")
  # a dataset the catalog does not have; columns the form has no field for
  p <- set_lf_rows(p, "listings", "L1", data.frame(
    type = "multiline", dataset = "ADXX", where = "AESEV == \"SEVERE\"",
    sort = "USUBJID | -ASTDT", max_rows = "22", blank_row = "USUBJID",
    wrap = "TRUE"))
  p <- set_lf_rows(p, "listing_cols", "L1", data.frame(
    vars = "USUBJID | AGE", label = "Subject\nAge", width = "12",
    collapse_repeats = "TRUE"))
  create_study("S1", planner = p)
  shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    env <- session$userData$lf_env
    p0 <- rv$p
    session$setInputs(target = "L1", nav = "make", step = "content",
                      content_nav = "content", lf_right = "result")
    expect_match(output$lf_form$html, "ADXX", fixed = TRUE)
    id <- function(x) paste0("lf", env$n, "_", x)
    # shown: every field as drawn
    v <- list()
    v[[id("type")]] <- "multiline"
    v[[id("dataset")]] <- "ADXX"
    v[[id("where")]] <- "AESEV == \"SEVERE\""
    v[[id("sort")]] <- "USUBJID | -ASTDT"
    v[[id("max_rows")]] <- 22
    do.call(session$setInputs, v)
    session$elapse(1000)
    expect_identical(rv$p$lf, p0$lf)
    # one field changed: that field, nothing else
    v <- list()
    v[[id("max_rows")]] <- 30
    do.call(session$setInputs, v)
    session$elapse(1000)
    want <- p0$lf
    want$listings$max_rows[want$listings$output_id == "L1"] <- "30"
    expect_identical(rv$p$lf, want)
    edited <- rv$p
    session$setInputs(save = 1)
    expect_identical(open_study("S1")$planner$lf, edited$lf)
  })
})

test_that("step 3, the figure designer: what it cannot show stays as written", {
  local_home()
  p <- add_output(new_planner(), "F1", type = "figure")
  d <- tflspec::tfl_fig_template("mean_se", data = "ADVS", param = "SYSBP")
  # a number as the design has it (written as text), a setting the
  # designer has no field for
  d$plot$y_max <- "180"
  d$plot$my_setting <- "kept"
  p <- set_fig_design(p, "F1", d)
  create_study("S1", planner = p)
  shiny::testServer(server_for("S1"), {
    rv <- session$userData$rv
    pd <- session$userData$pd
    d0 <- fig_design(rv$p, "F1")
    session$setInputs(target = "F1", nav = "make", step = "content",
                      content_nav = "content")
    expect_match(output$pd_form$html, "Visit", fixed = TRUE)
    id <- function(x) paste0("pd", pd$n, "_", x)
    # shown: the fields as the browser sends them once drawn
    v <- list()
    v[[id("x_label")]] <- "Visit"
    v[[id("y_max")]] <- 180
    v[[id("palette")]] <- "treatment"
    v[[id("legend")]] <- "bottom"
    do.call(session$setInputs, v)
    session$elapse(1000)
    expect_identical(fig_design(rv$p, "F1"), d0)
    # one field changed: that one
    v <- list()
    v[[id("x_label")]] <- "Week"
    do.call(session$setInputs, v)
    session$elapse(1000)
    d1 <- fig_design(rv$p, "F1")
    expect_identical(d1$plot$x_label, "Week")
    d1$plot$x_label <- d0$plot$x_label
    expect_identical(d1, d0)
    edited <- rv$p
    session$setInputs(save = 1)
    expect_identical(fig_design(open_study("S1")$planner, "F1"),
                     fig_design(edited, "F1"))
  })
})
