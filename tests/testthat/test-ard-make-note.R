# The "working" notice of [ARD を作る]: one fixed id, replaced by each step,
# removed when the work ends -- also by an error (#717).
fake_session <- function() {
  log <- character()
  list(log = function() log,
       sendNotification = function(type, message) {
         log <<- c(log, if (type == "remove") paste("remove", message) else paste("show", message$id))
         invisible(NULL)
       })
}

test_that("the notice is removed when the work ends", {
  s <- fake_session()
  r <- tflplanner:::.with_working_note("n1", function(say) {
    say("a", "1/2")
    say("a", "2/2")
    "done"
  }, session = s)
  expect_identical(r, "done")
  expect_identical(s$log(), c("show n1", "show n1", "remove n1"))
})

test_that("the notice is removed when the work fails", {
  s <- fake_session()
  expect_error(tflplanner:::.with_working_note("n1", function(say) {
    say("a")
    stop("boom")
  }, session = s), "boom")
  expect_identical(s$log(), c("show n1", "remove n1"))
})
