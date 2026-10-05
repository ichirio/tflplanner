# Closing the app: the system's browser by default, the Close button, and
# no beforeunload hold inside RStudio's own windows.

test_that("run_app() opens the system's browser unless told otherwise", {
  got <- NULL
  local_mocked_bindings(runApp = function(appDir, ...) got <<- list(...),
                        .package = "shiny")
  local_home()
  suppressMessages(run_app())
  expect_identical(got$launch.browser, tflplanner:::.external_browser)
  suppressMessages(run_app(launch.browser = FALSE, port = 1234))
  expect_false(got$launch.browser)
  expect_identical(got$port, 1234)
})

test_that("the page holds a close only outside RStudio's windows", {
  js <- tflplanner:::.unsaved_js
  expect_match(js, "/RStudio/i.test(navigator.userAgent)", fixed = TRUE)
  expect_match(js, "dirty && !inRStudio", fixed = TRUE)
  expect_match(js, "tflplanner-closed", fixed = TRUE)
})

test_that("the Close button is offered where the app runs on this computer", {
  loc <- tflplanner:::.is_local_host
  expect_true(loc("localhost"))
  expect_true(loc("127.0.0.1"))
  expect_false(loc("connect.example.com"))
  expect_false(loc(NULL))
  # a test session is no local browser: no button
  local_home()
  two_studies()
  shiny::testServer(server_for("S1"), {
    expect_null(output$close_btn$html)
  })
})
