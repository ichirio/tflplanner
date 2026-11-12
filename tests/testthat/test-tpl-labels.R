# The template arguments' headings: "meaning (name)", in English and
# Japanese, for every argument the card shows; the doc in the (i)

test_that("every argument a template card shows has a heading and its Japanese", {
  tp <- tflspec::tfl_templates()$name
  shown <- unique(unlist(lapply(tp, function(n) names(.tpl_shown(.tpl_params(n))))))
  expect_identical(setdiff(shown, names(.tpl_arg_labels)), character())
  s <- .strings()
  expect_identical(setdiff(c(unname(.tpl_arg_labels), unname(.tpl_doc_labels)), s[[1L]]), character())
  docs <- unique(unlist(lapply(tp, function(n) vapply(.tpl_shown(.tpl_params(n)),
                                                      function(x) x$doc %||% "", ""))))
  expect_identical(setdiff(docs[nzchar(docs)], s[[1L]]), character())
})

test_that("the card's top group: a key argument stays up at its default, another default is folded", {
  prm <- list(population = list(name = "population", required = TRUE, tier = "required"),
              hierarchy = list(name = "hierarchy", required = FALSE, tier = "key",
                               default = c("AEBODSYS", "AEDECOD")),
              where = list(name = "where", required = FALSE, tier = "usual", default = "x"),
              sort = list(name = "sort", required = FALSE, tier = "other"),
              by = list(name = "by", required = FALSE, tier = "usual", default = "TRT01A"),
              old = list(name = "old", required = FALSE))
  g <- .tpl_card_groups(prm, list())
  expect_identical(g$up, c("population", "hierarchy"))
  expect_identical(g$low, c("where", "sort", "by", "old"))
  # the report's own value keeps its argument up, as before
  expect_identical(.tpl_card_groups(prm, list(by = "TRT01P"))$up, c("population", "hierarchy", "by"))
  # a tflspec with no "key" tier: nothing is key
  expect_false(.tpl_is_key(list(name = "x", tier = "usual")))
  expect_false(.tpl_is_key(list(name = "x")))
  expect_true(.tpl_is_key(prm$hierarchy))
  # (the templates tflspec marks: key when it knows the tier)
  skip_if_not("key" %in% tflspec::tfl_tpl_args("ae_soc_pt")$tier)
  expect_true(.tpl_is_key(.tpl_params("ae_soc_pt")$hierarchy))
  expect_true(.tpl_is_key(.tpl_params("ae_pt_freq")$term))
  expect_true(.tpl_is_key(.tpl_params("ae_max_grade")$grade))
  expect_false(.tpl_is_key(.tpl_params("hierarchical")$hierarchy))
  expect_identical(tr("key", "ja"), "\u4e3b\u8981")
})
