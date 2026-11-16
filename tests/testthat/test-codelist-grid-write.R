# The "All code lists.." dialog's grid: drawn again (another report, the box
# ticked) it sends its rows back with no edit; that must not write the
# planner (a write puts it out of step with its spec; the next sync then
# converted the whole spec, about 10 s).

# what rhandsontable sends for the grid as drawn; `value` set in one cell
# when `col` is given (an edit), else nothing changed
.clw_event <- function(session, output, id, row = NULL, col = NULL, value = NULL,
                       event = "afterChange") {
  w <- jsonlite::fromJSON(output[[id]], simplifyVector = FALSE)$x
  hdr <- unlist(w$colHeaders)
  data <- w$data
  ch <- list()
  if (!is.null(col)) {
    old <- data[[row]][[match(col, hdr)]]
    data[[row]][[match(col, hdr)]] <- value
    ch <- list(list(row - 1L, col, old, value))
  } else {
    ch <- list(list(0L, hdr[[1L]], data[[1L]][[1L]], data[[1L]][[1L]]))
  }
  v <- list(list(
    data = data,
    changes = list(event = event, changes = ch),
    params = list(planner_key = w$planner_key, rClass = "data.frame",
                  rColHeaders = as.list(hdr), rColClasses = w$rColClasses,
                  rDataDim = list(length(data), length(hdr)))))
  names(v) <- id
  do.call(session$setInputs, v)
}

test_that("the code-list grid writes nothing for a redraw with no edit, and still writes an edit", {
  skip_on_cran()
  local_home()
  p <- add_output(new_planner(), "T1")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  p$ard$analyses <- data.frame(
    output_id = "T1", analysis_id = "SEX", method = "categorical",
    population_id = "SAF", by = "TRT01A", variables = "SEX")
  p <- set_analysis_data(p, "T1", "adsl_saf", from = "ADSL", population_id = "SAF")
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  # the report's own variable between two the grid hides
  p <- set_codelist(p, "T1", data.frame(
    variable = c("AEDECOD", "SEX", "SEX", "RACE"),
    value = c("DRY EYE", "F", "M", "WHITE"),
    label = c("Dry eye", "Female", "Male", "White"),
    order = c("1", "1", "2", "1")))
  create_study("CW", planner = p)
  shiny::testServer(server_for("CW"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "ard", target = "T1")
    session$setInputs(ard_adata_pick = "adsl_saf")
    session$setInputs(adata_id = "adsl_saf", adata_add = "SEX", adata_keep = NULL)
    session$setInputs(cl21_open = 1)
    expect_false(grepl("AEDECOD", paste(output$cl21_hot, collapse = ""), fixed = TRUE))
    for (all in c(FALSE, TRUE)) {
      session$setInputs(cl21_all = all)
      before <- rv$p
      .clw_event(session, output, "cl21_hot")
      expect_identical(rv$p, before)
      .clw_event(session, output, "cl21_hot", event = "afterCreateRow")
      expect_identical(rv$p, before)
    }
    # an edit is written
    session$setInputs(cl21_all = FALSE)
    w <- jsonlite::fromJSON(output$cl21_hot, simplifyVector = FALSE)$x
    hdr <- unlist(w$colHeaders)
    vals <- vapply(w$data, function(r) as.character(r[[match("value", hdr)]] %||% NA), "")
    .clw_event(session, output, "cl21_hot", match("F", vals), "label", "Women")
    d <- sheet_rows(rv$p, "codelists", "T1")
    expect_identical(d$label[d$variable == "SEX"], c("Women", "Male"))
    expect_identical(d$label[d$variable != "SEX"], c("Dry eye", "White"))
  })
})
