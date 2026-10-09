# A code list edited in place: a click on a column of the code lists' part
# (1-1 part 4, a listing's, a figure's) opens its values and labels right
# below, one at a time; and the table builder's rows as the code list of
# `variable` (the variables sheet's labels and order).

# what rhandsontable sends when one cell of a grid drawn by the app is
# changed (as test-spec-keep.R's)
.cl_grid_edit <- function(session, output, id, row, col, value) {
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

test_that("1-1 part 4: a click opens a column's code list in place, one at a time", {
  skip_on_cran()
  local_home()
  p <- add_output(new_planner(), "T1", type = "table")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  for (sh in names(p$ard)) p$ard[[sh]] <- .normalize_ard_sheet(p$ard[[sh]], sh)
  p <- set_analysis_data(p, "T1", "adsl_saf", from = "ADSL", population_id = "SAF")
  p <- set_codelist(p, "T1", data.frame(variable = c("SEX", "SEX", "RACE"),
                                        value = c("F", "M", "WHITE"),
                                        label = c("Female", "Male", "White"),
                                        order = c("1", "2", "1")))
  s <- create_study("CI", planner = p)
  dir.create(file.path(s$path, "data", "adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(data.frame(USUBJID = c("1", "2"), SEX = c("F", "M"), RACE = "WHITE",
                     AGEGR1 = c("<65", ">=65"), SAFFL = "Y"),
          file.path(s$path, "data", "adam", "adsl.rds"))
  shiny::testServer(server_for("CI"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "ard", target = "T1")
    session$setInputs(ard_adata_pick = "adsl_saf")
    session$setInputs(adata_id = "adsl_saf", adata_from = "ADSL", adata_add = NULL,
                      adata_keep = NULL)
    h <- output$adata_cl$html
    # each line opens its list; a column without one can start one
    expect_match(h, "Shiny.setInputValue(&#39;adata_cl_pick&#39;, &#39;SEX&#39;", fixed = TRUE)
    expect_match(h, "adata_cl_new", fixed = TRUE)
    expect_match(h, "AGEGR1", fixed = TRUE)
    expect_error(output$adata_cl_detail)          # nothing open
    session$setInputs(adata_cl_pick = "SEX")
    expect_match(output$adata_cl_detail$html, "Code list of SEX", fixed = TRUE)
    expect_match(output$adata_cl$html, "table-active", fixed = TRUE)
    g <- output$adata_cl_hot
    expect_match(g, "Female", fixed = TRUE)
    expect_false(grepl("White", g, fixed = TRUE))  # one column's grid only
    # an edit writes that column's rows, the others as they were
    .cl_grid_edit(session, output, "adata_cl_hot", 2L, "label", "Men")
    d <- sheet_rows(rv$p, "codelists", "T1")
    expect_identical(d$variable, c("SEX", "SEX", "RACE"))
    expect_identical(d$label, c("Female", "Men", "White"))
    expect_match(output$adata_cl$html, "M → Men", fixed = TRUE)
    # another column: its grid instead (one at a time); its own click closes
    session$setInputs(adata_cl_pick = "RACE")
    expect_match(output$adata_cl_detail$html, "Code list of RACE", fixed = TRUE)
    session$setInputs(adata_cl_pick = "RACE")
    expect_error(output$adata_cl_detail)
    # a list for a column without one, typed in its grid
    session$setInputs(adata_cl_new = "AGEGR1")
    expect_match(output$adata_cl_detail$html, "Code list of AGEGR1", fixed = TRUE)
    .cl_grid_edit(session, output, "adata_cl_hot", 1L, "value", "<65")
    d <- sheet_rows(rv$p, "codelists", "T1")
    expect_identical(d$value[d$variable == "AGEGR1"], "<65")
    # the values its data has that the list lacks: added, and the list open
    expect_match(output$adata_cl$html, "AGEGR1: the data has values", fixed = TRUE)
    session$setInputs(adata_cl_close = 1)
    session$setInputs(adata_cl_add = "AGEGR1")
    expect_match(output$adata_cl_detail$html, "Code list of AGEGR1", fixed = TRUE)
    d <- sheet_rows(rv$p, "codelists", "T1")
    expect_identical(d$value[d$variable == "AGEGR1"], c("<65", ">=65"))
    # the dialog of every code list and the copy dialog stay
    expect_match(output$adata_cl$html, "cl21_open", fixed = TRUE)
    expect_match(output$adata_cl$html, "cl21_copy", fixed = TRUE)
    # another report: nothing open
    session$setInputs(target = NULL)
    session$setInputs(target = "T1")
    expect_error(output$adata_cl_detail)
  })
})

test_that("the table builder's rows as the code list of variable: labels and order", {
  skip_on_cran()
  skip_if_not_installed("cards")
  local_home()
  p <- add_output(new_planner(), "T-DM", type = "table")
  s <- create_study("BV", planner = p)
  ard <- cards::ard_stack(cards::ADSL, .by = TRT01A,
                          cards::ard_continuous(variables = AGE),
                          cards::ard_categorical(variables = SEX))
  data <- rtfreporter::normalize_ard(ard)
  m <- ard_meta(ard, data)
  f <- .meta_file(s, "T-DM")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(m, f)
  saveRDS(data, sub("[.]rds$", "_data.rds", f))
  shiny::testServer(server_for("BV"), {
    rv <- session$userData$rv
    bform <- session$userData$bform
    session$setInputs(target = "T-DM", nav = "make", step = "content",
                      content_nav = "content", table_nav = "builder")
    b <- function(x) paste0("b", bform$n, "_", x)
    v <- list()
    v[[b("key")]] <- "TRT01A"
    v[[b("vars")]] <- c("AGE", "SEX")
    v[[b("arms_TRT01A")]] <- m$keys$TRT01A
    v[[b("rows")]] <- c("n", "Mean (SD)")
    v[[b("cat")]] <- "npct"
    v[[b("pct")]] <- 1
    v[[b("header")]] <- "keep"
    do.call(session$setInputs, v)
    expect_match(output$b_vcl$html, "b_vcl_toggle", fixed = TRUE)
    expect_false(grepl("b_vcl_hot", output$b_vcl$html, fixed = TRUE))
    session$setInputs(b_vcl_toggle = 1)
    expect_match(output$b_vcl$html, "b_vcl_hot", fixed = TRUE)
    w <- jsonlite::fromJSON(output$b_vcl_hot, simplifyVector = FALSE)$x
    expect_identical(unlist(w$colHeaders), c("value", "label", "order"))
    expect_identical(vapply(w$data, function(r) r[[1]], ""), c("AGE", "SEX"))
    # a heading, written to the variables sheet's label
    .cl_grid_edit(session, output, "b_vcl_hot", 2L, "label", "Sex, n (%)")
    vr <- sheet_rows(rv$p, "variables", "T-DM")
    expect_identical(vr$label[vr$variable == "SEX"], "Sex, n (%)")
    # the form is drawn again from it, the grid still open
    expect_match(output$b_vcl$html, "b_vcl_hot", fixed = TRUE)
    # the order: SEX first
    v <- list()
    v[[b("key")]] <- "TRT01A"
    v[[b("vars")]] <- c("AGE", "SEX")
    v[[b("arms_TRT01A")]] <- m$keys$TRT01A
    v[[b("rows")]] <- c("n", "Mean (SD)")
    v[[b("cat")]] <- "npct"
    v[[b("pct")]] <- 1
    v[[b("header")]] <- "keep"
    v[[b("lab2")]] <- "Sex, n (%)"
    do.call(session$setInputs, v)
    .cl_grid_edit(session, output, "b_vcl_hot", 2L, "order", "0")
    vr <- sheet_rows(rv$p, "variables", "T-DM")
    expect_identical(vr$order[match(c("SEX", "AGE"), vr$variable)], c("1", "2"))
    expect_identical(vr$label[vr$variable == "SEX"], "Sex, n (%)")
    # one place: no code list rows of `variable`, the program's labels
    expect_false(any(sheet_rows(rv$p, "codelists", "T-DM")$variable %in% "variable"))
  })
})
