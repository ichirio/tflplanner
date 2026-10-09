# Extracted from test-app-robust.R:524

# prequel ----------------------------------------------------------------------
ard_study <- function() {
  p <- add_output(new_planner(), "T1", description = "a table")
  p$ard$analyses <- .normalize_ard_sheet(data.frame(
    output_id = "T1", analysis_id = "A1", method = "continuous",
    dataset = "ADSL", variables = "AGE", stringsAsFactors = FALSE),
    "analyses")
  create_study("S1", planner = p)
}
analyses_payload <- function(rows, key, event = "afterChange") {
  cols <- setdiff(.ard_sheets()$analyses, "output_id")
  list(data = rows,
       changes = list(event = event,
                      changes = list(list(length(rows) - 1L, "method", NULL,
                                          "continuous"))),
       params = list(planner_key = key, rClass = "data.frame",
                     rColHeaders = as.list(cols),
                     rColClasses = stats::setNames(as.list(rep("character", length(cols))),
                                                   cols),
                     rDataDim = list(length(rows), length(cols))))
}

# test -------------------------------------------------------------------------
local_home()
alive <- TRUE
px <- list(is_alive = function() alive, get_exit_status = function() 0L,
             kill = function() invisible(TRUE))
started <- NULL
local_mocked_bindings(run_batch = function(study, parts, ...) {
    started <<- list(id = study$meta$study_id, parts = parts)
    px
  })
skip_if_not_installed("cards")
skip_if_not_installed("cardx")
shiny::testServer(function(input, output, session)
    app_server(input, output, session, NULL), {
    rv <- session$userData$rv
    session$setInputs(try_sample = 1L)
    session$setInputs(ns_from = "sample", ns_id = "TRAIN-01",
                      ns_root = studies_root(), ns_ok = 1L)
    # copied and listed, its official run started and not waited for
    expect_identical(list_studies()$study_id, "TRAIN-01")
    expect_identical(started, list(id = "TRAIN-01", parts = c("ard", "tfl")))
    expect_identical(rv$job_study, "TRAIN-01")
    session$setInputs(nav = "study")
    expect_match(output$studies, "TRAIN-01 \u25cf (running)", fixed = TRUE)
    # done: the mark goes
    alive <- FALSE
    session$elapse(1500)
    expect_null(rv$job)
    expect_false(grepl("(running)", output$studies, fixed = TRUE))
  })
