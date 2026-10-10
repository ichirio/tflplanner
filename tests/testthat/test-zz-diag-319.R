# Diagnosis only for #319 (this branch is not merged): what fails under
# broom.helpers' "Unable to tidy `x`" for a glm on the oldest R
test_that("diagnosis #319: a glm's tidy on this R", {
  skip_if_not_installed("broom")
  adsl <- cards::ADSL[cards::ADSL$ARM != "Xanomeline Low Dose", ]
  adsl$RESP <- as.integer(adsl$AGE > 75)
  fit <- glm(RESP ~ ARM, data = adsl, family = binomial)
  try_msg <- function(expr) tryCatch({ force(expr); "OK" }, error = function(e) conditionMessage(e))
  msgs <- c(
    R = R.version.string,
    broom = as.character(utils::packageVersion("broom")),
    MASS = as.character(utils::packageVersion("MASS")),
    parameters = requireNamespace("parameters", quietly = TRUE),
    confint = try_msg(stats::confint(fit)),
    confint_default = try_msg(stats::confint.default(fit)),
    mass_confint = try_msg(MASS:::confint.glm(fit)),
    tidy = try_msg(broom::tidy(fit)),
    tidy_ci = try_msg(broom::tidy(fit, conf.int = TRUE)),
    tidy_ci_exp = try_msg(broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE)),
    bh = try_msg(broom.helpers::tidy_with_broom_or_parameters(fit, conf.int = TRUE, exponentiate = TRUE)))
  message("DIAG319 ", paste(names(msgs), msgs, sep = " = ", collapse = " || "))
  expect_true(FALSE, info = paste("DIAG319", paste(names(msgs), msgs, sep = " = ", collapse = " || ")))
})
