test_that("several unnamed scalars of different kinds stay apart", {
  # one vector when they are of a kind
  expect_identical(.args_simplify(list("a", "b")), c("a", "b"))
  # 1 is not turned into "1"
  expect_identical(.args_simplify(list("a", 1)), list("a", 1))
  expect_identical(.args_from_r_own("plan_sort", "\"a\", 1"), list(keys = list("a", 1)))
})
