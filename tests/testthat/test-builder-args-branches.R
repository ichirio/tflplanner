test_that(".only_r_value turns every string of a list or vector into R text and leaves other values", {
  v <- .only_r_value(c("AGE > 1", "SEX == 'F'"))
  expect_true(is.list(v))
  expect_true(all(vapply(v, inherits, NA, "tfl_r")))
  expect_identical(as.character(unclass(v[[2L]])), "SEX == 'F'")
  l <- .only_r_value(list("a", 1))
  expect_s3_class(l[[1L]], "tfl_r")
  expect_identical(l[[2L]], 1)
  expect_identical(.only_r_value(3), 3)
})

test_that(".args_known is FALSE for anything that is not one function name", {
  expect_false(.args_known(NULL))
  expect_false(.args_known(1))
  expect_false(.args_known(c("plan_sort", "plan_filter")))
  expect_false(.args_known(""))
  expect_false(.args_known("no_such_function_xyz"))
  expect_true(.args_known("plan_sort"))
})

test_that(".arg_to_r writes a factor as its labels, a list as list(), and other objects by deparse", {
  expect_identical(.arg_to_r(factor("x")), "\"x\"")
  expect_identical(.arg_to_r(list(a = 1, "x")), "list(a = 1, \"x\")")
  expect_match(.arg_to_r(function(x) x + 1), "function", fixed = TRUE)
})

test_that("an unknown function's arguments with no name are an error naming the function", {
  expect_error(.args_from_r_own("no_such_function_xyz", "1, 2"),
               "no_such_function_xyz: an argument with no name", fixed = TRUE)
  expect_error(.args_from_r_own(NULL, "\"a\""), "The item: an argument with no name", fixed = TRUE)
})

test_that("arguments with no name go to the positional key of the function, one value or a vector", {
  expect_identical(.args_from_r_own("plan_sort", "\"a\""), list(keys = "a"))
  expect_identical(.args_from_r_own("plan_sort", "\"a\", \"b\""), list(keys = c("a", "b")))
  # not all scalars: a list
  k <- .args_from_r_own("plan_sort", "c(\"a\", \"b\"), \"z\"")$keys
  expect_true(is.list(k))
  expect_length(k, 2L)
  expect_identical(k[[1L]], c("a", "b"))
})

test_that(".args_positional_key is NULL for no function or one without a positional ...", {
  expect_null(.args_positional_key(NULL))
  expect_null(.args_positional_key("no_such_function_xyz"))
  expect_identical(.args_positional_key("plan_sort"), "keys")
})

test_that(".arg_from_r reads a signed number as the number", {
  expect_identical(.arg_from_r(quote(-1)), -1)
  expect_identical(.arg_from_r(quote(+0.5)), 0.5)
})

test_that(".arg_from_r reads a c() of literals as a vector, keeping its names only when it has some", {
  expect_identical(.arg_from_r(quote(c("x", "y"))), c("x", "y"))
  expect_identical(.arg_from_r(quote(c(a = 1, b = 2))), c(a = 1, b = 2))
  expect_identical(.arg_from_r(quote(c(a = "x", "y"))), c(a = "x", "y"))
})

test_that(".arg_from_r keeps a c() with a non-literal as R text", {
  v <- .arg_from_r(quote(c("x", foo)))
  expect_s3_class(v, "tfl_r")
  expect_identical(as.character(unclass(v)), "c(\"x\", foo)")
})

test_that(".arg_from_r reads list() with its names, NULL as !r NULL, and a call as R", {
  expect_identical(.arg_from_r(quote(list(a = 1, b = "z"))), list(a = 1, b = "z"))
  expect_identical(.arg_from_r(quote(list(1, 2))), list(1, 2))
  expect_identical(as.character(unclass(.arg_from_r(NULL))), "NULL")
  expect_identical(as.character(unclass(.arg_from_r(quote(x + y)))), "x + y")
})

test_that(".args_names spreads the call's names over the entries and is NULL for a call with none", {
  expect_identical(.args_names(quote(c(a = "x", "y")), list("x", "y")), c("a", ""))
  expect_null(.args_names(quote(c(1, 2)), list(1, 2)))
})

test_that(".args_simplify makes one vector of scalars and keeps anything else as a list", {
  expect_identical(.args_simplify(list("a", "b")), c("a", "b"))
  expect_identical(.args_simplify(list(c("a", "b"), "z")), list(c("a", "b"), "z"))
})
