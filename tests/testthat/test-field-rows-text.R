test_that("plan_sort's keys are a Rows field: the key text shown as rows, read back as the text", {
  skip_if_not(is.function(get0("tfl_rows_read", envir = asNamespace("tflspec"), inherits = FALSE)))
  defs <- spec_card_defs("plan_sort")
  d <- defs$args$keys
  expect_identical(d$serializer, "sort_keys")
  expect_identical(field_type(d)[[1L]], "rows")
  src <- .sort_rows_sources(list(row_keys = c("AEBODSYS", "AEDECOD"), key = "TRT01A",
                                 arms = list(TRT01A = c("Drug X", "Control"))),
                            stats = c("N", "n", "p"), total = "Total")
  expect_identical(unname(src$columns$row_keys), c("group1", "label"))
  expect_identical(unname(src$columns$row_groups), "group1")
  expect_identical(unname(src$columns$spread), c("Drug X", "Control", "Total"))
  expect_identical(unname(src$stats), c("n", "p"))
  keys <- c(".overall", "-n@group1", "group1", ".depth", "-n[Total]", "label")
  html <- function(x) gsub("&quot;", "\"", as.character(x), fixed = TRUE)
  h <- html(arg_field("c", "keys", d$type, value = keys, def = d, ctx = src, lang = "ja"))
  # one row a key, four cells: by, direction, level, column (their Japanese heads)
  expect_match(h, "rp-tbl rp-tbl-rows", fixed = TRUE)
  expect_match(h, "\u4e26\u3079\u308b\u57fa\u6e96", fixed = TRUE)
  expect_match(h, "\u4ef6\u6570\u3092\u898b\u308b\u968e\u5c64", fixed = TRUE)
  expect_match(h, '"cols":["by","direction","level","column"]', fixed = TRUE)
  expect_match(h, '["n","desc","group1",""]', fixed = TRUE)
  expect_match(h, '["n","desc","","Total"]', fixed = TRUE)
  # the switch that writes false, off
  expect_match(h, 'id="c__keys__false"', fixed = TRUE)
  expect_no_match(h, 'rp-tbl-false" checked', fixed = TRUE)
  # read back: the same keys
  st <- jsonlite::toJSON(.tbl_state(.tbl_spec(d$type, d, src, lang = "ja"),
                                    tflspec::tfl_rows_read(keys, "sort_keys")), auto_unbox = FALSE)
  got <- builder_card_read(list(c__keys = as.character(st)), "c", defs, args = "keys")
  expect_identical(got$keys, keys)
  # the switch on: false (the data's order), the rows set aside
  got <- builder_card_read(list(c__keys = as.character(st), c__keys__false = TRUE), "c", defs, args = "keys")
  expect_identical(got$keys, FALSE)
  # false drawn: no rows, the switch on
  h <- html(arg_field("c", "keys", d$type, value = FALSE, def = d, ctx = src))
  expect_match(h, "rp-tbl-false\" checked", fixed = TRUE)
  expect_match(h, '"rows":[]', fixed = TRUE)
  # no row left: nothing written
  empty <- jsonlite::toJSON(list(shape = "rows", cols = c("by", "direction", "level", "column"),
                                 kinds = rep("pick", 4L), rows = list()))
  expect_null(builder_card_read(list(c__keys = as.character(empty)), "c", defs, args = "keys")$keys)
  # TRUE, which the rows cannot say: shown as its R, nothing lost
  h <- html(arg_field("c", "keys", d$type, value = TRUE, def = d, ctx = src))
  expect_match(h, "rp-field-r", fixed = TRUE)
})

test_that("a Rows column's fixed values are its choices, labelled in the language", {
  cd <- list(values = c("asc", "desc"),
             value_labels = data.frame(value = c("asc", "desc"), label_en = c("Up", "Down"),
                                       label_ja = c("\u6607\u9806", NA), stringsAsFactors = FALSE))
  expect_identical(names(.tbl_fixed_values(cd, "ja")), c("\u6607\u9806", "Down"))
  expect_identical(names(.tbl_fixed_values(cd, "en")), c("Up", "Down"))
  expect_identical(.tbl_fixed_values(list(values = c("a", "b")), "en"), c("a", "b"))
  expect_null(.tbl_fixed_values(list(), "en"))
})
