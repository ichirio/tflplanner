# The template arguments' headings: "meaning (name)", in English and
# Japanese, for every argument the card shows; the doc in the (i)

test_that("every argument a template card shows has a heading and its Japanese", {
  tp <- tflspec::tfl_templates()$name
  shown <- unique(unlist(lapply(tp, function(n) names(.tpl_shown(.tpl_params(n))))))
  expect_identical(setdiff(shown, names(.tpl_arg_labels)), character())
  s <- utils::read.csv(system.file("i18n", "strings.csv", package = "tflplanner"),
                       encoding = "UTF-8", stringsAsFactors = FALSE)
  expect_identical(setdiff(c(unname(.tpl_arg_labels), unname(.tpl_doc_labels)), s[[1L]]), character())
  docs <- unique(unlist(lapply(tp, function(n) vapply(.tpl_shown(.tpl_params(n)),
                                                      function(x) x$doc %||% "", ""))))
  expect_identical(setdiff(docs[nzchar(docs)], s[[1L]]), character())
})
