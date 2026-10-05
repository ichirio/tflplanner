# The ARD form's function search (R/ard_fn_search.R): the dictionary, the
# text both sides are compared in, and what a query finds first.

fn_find <- function(q, lang = "en") {
  m <- .std_ard_methods()
  f <- tflspec::tfl_ard_functions()
  .fn_search(q, .ard_fn_entries(m, f), .fn_keywords(m, f), lang = lang)
}
fn_first <- function(q) fn_find(q)$value[1L]

test_that("the dictionary names the catalog's functions, rightly", {
  d <- .fn_dict()
  m <- tflspec::tfl_ard_methods()
  f <- tflspec::tfl_ard_functions()
  expect_identical(names(d), c("fn", "lang", "keyword", "rank", "note_ja", "note_en", "args"))
  # every function is the catalog's, or a company keyword with no function
  expect_true(all(d$fn %in% c(f$call, m$method)))
  expect_true(all(d$lang %in% c("en", "ja", "sas", "r")))
  expect_true(all(d$rank %in% 1:3))
  # a near one always says why
  near <- d[d$rank == 3L, ]
  expect_true(all(nzchar(near$note_ja) & nzchar(near$note_en)))
  # a note in both languages, or in neither
  expect_identical(nzchar(d$note_ja), nzchar(d$note_en))
  # no word twice for a function
  expect_false(any(duplicated(d[c("fn", "keyword")])))
  # every function the builder offers has a word in English and in Japanese
  old <- !is.na(f$replaced_by) & nzchar(f$replaced_by)
  for (fn in f$call[f$offered %in% TRUE & !old]) {
    expect_true(any(d$fn == fn & d$lang == "en"), info = fn)
    expect_true(any(d$fn == fn & d$lang == "ja"), info = fn)
  }
  # SAS: a PROC, or a statement's name
  sas <- d$keyword[d$lang == "sas"]
  expect_true(all(grepl("^(PROC|LSMEANS|ESTIMATE|TEST)( |$)", sas)))
  expect_true(all(validUTF8(d$keyword)))
})

test_that("both sides are compared in one text", {
  expect_identical(.fn_norm("\uff34 \uff34\uff45\uff53\uff54"), "t test")
  expect_identical(.fn_norm("Fisher's exact (2x2)"), "fisher s exact 2x2")
  expect_identical(.fn_norm("t-test_one/sample"), "t test one sample")
  # the long vowel mark and the middle dot go; small kana are large
  expect_identical(.fn_norm("\u30ab\u30d7\u30e9\u30f3\u30fb\u30de\u30a4\u30e4\u30fc"),
                   .fn_norm("\u30ab\u30d7\u30e9\u30f3\u30de\u30a4\u30e4"))
  expect_identical(.fn_norm("\u30a6\u30a3\u30eb\u30b3\u30af\u30bd\u30f3"),
                   .fn_norm("\u30a6\u30a4\u30eb\u30b3\u30af\u30bd\u30f3"))
  # hiragana as katakana
  expect_identical(.fn_norm("\u304b\u3044"), "\u30ab\u30a4")
  # half-width kana, voiced and semi-voiced
  expect_identical(.fn_norm("\uff76\uff9e\uff8c\uff9f"), "\u30ac\u30d7")
  expect_identical(.fn_norm("\uff76\uff8c\uff9f\uff97\uff9d\uff8f\uff72\uff94\uff70"),
                   .fn_norm("\u30ab\u30d7\u30e9\u30f3\u30de\u30a4\u30e4\u30fc"))
  # chi-square written with chi, and squared
  expect_identical(.fn_norm("\u03c7\u00b2"), "chi2")
  expect_identical(.fn_norm("\u03c72\u4e57"), .fn_norm("\u03c7\u4e8c\u4e57"))
  expect_identical(.fn_norm(c(NA, "")), c("", ""))
})

test_that("a query finds the function for it first", {
  skip_if_not_installed("cardx")
  # an abbreviation: SMD is the balance of the background, then the
  # effect sizes
  s <- fn_find("SMD")
  expect_identical(s$value[1:3], c("cardx::ard_smd_smd", "cardx::ard_effectsize_cohens_d",
                                   "cardx::ard_effectsize_hedges_g"))
  # one word, several functions: the model first, the tests related
  s <- fn_find("\u30aa\u30c3\u30ba\u6bd4")
  expect_identical(s$call[1L], "cardx::ard_regression")
  expect_true(all(c("cardx::ard_regression_basic", "cardx::ard_stats_fisher_test",
                    "cardx::ard_stats_mantelhaen_test") %in% s$call))
  expect_match(s$note[1L], "exponentiate = TRUE", fixed = TRUE)
  expect_identical(s$hit[1L], "\u30aa\u30c3\u30ba\u6bd4")
  # a SAS procedure
  s <- fn_find("PROC LOGISTIC")
  expect_identical(s$call[1:2], c("cardx::ard_regression", "cardx::ard_regression_basic"))
  expect_identical(s$hit[1L], "PROC LOGISTIC (SAS)")
  s <- fn_find("proc freq")
  expect_identical(s$call[1L], "cards::ard_tabulate")
  expect_true(all(c("cardx::ard_stats_chisq_test", "cardx::ard_stats_fisher_test",
                    "cardx::ard_stats_mantelhaen_test", "cardx::ard_categorical_ci") %in% s$call))
  # words ANDed
  expect_identical(fn_find("t\u691c\u5b9a \u5bfe\u5fdc")$call, "cardx::ard_stats_paired_t_test")
  # full-width; the company's keyword and its function are one row
  s <- fn_find("\uff54\u691c\u5b9a")
  expect_identical(s$value[1L], "ttest")
  expect_false("cardx::ard_stats_t_test" %in% s$value)
  # the words written together; not inside "fisher's exact test"
  expect_identical(fn_first("ttest"), "ttest")
  expect_false("cardx::ard_stats_fisher_test" %in% fn_find("ttest")$call)
  # Kaplan-Meier, however written
  for (q in c("\u30ab\u30d7\u30e9\u30f3\u30de\u30a4\u30e4\u30fc",
              "\u30ab\u30d7\u30e9\u30f3\u30fb\u30de\u30a4\u30e4\u30fc",
              "\uff76\uff8c\uff9f\uff97\uff9d\uff8f\uff72\uff94\uff70", "KM")) {
    expect_identical(fn_first(q), "cardx::ard_survival_survfit", info = q)
  }
  expect_identical(fn_find("\u30cf\u30b6\u30fc\u30c9\u6bd4")$call[1L], "cardx::ard_regression")
  expect_identical(fn_find("proc lifetest")$call,
                   c("cardx::ard_survival_survfit", "cardx::ard_survival_survfit_diff",
                     "cardx::ard_survival_survdiff"))
  expect_identical(fn_find("\u6700\u5c0f\u4e8c\u4e57\u5e73\u5747 \u5dee")$call,
                   "cardx::ard_emmeans_contrast")
  expect_identical(fn_first("lsmeans"), "cardx::ard_emmeans_emmeans")
  expect_identical(fn_first("CMH"), "cardx::ard_stats_mantelhaen_test")
  expect_identical(fn_find("\u6709\u5bb3\u4e8b\u8c61 SOC")$call[1L],
                   "cards::ard_stack_hierarchical")
  expect_identical(fn_find("\u6700\u60aa\u30b0\u30ec\u30fc\u30c9")$call[1L],
                   "cardx::ard_tabulate_max")
  # AND narrows: a proportion's interval, not a mean's
  s <- fn_find("\u4fe1\u983c\u533a\u9593 \u5272\u5408")
  expect_identical(s$call[1L], "cardx::ard_categorical_ci")
  expect_false("cardx::ard_continuous_ci" %in% s$call)
  expect_identical(fn_find("clopper")$call[1L], "cardx::ard_categorical_ci")
  expect_identical(fn_find("\u4eba\u5e74")$call[1L], "cardx::ard_incidence_rate")
  expect_identical(fn_first("\u30a6\u30a4\u30eb\u30b3\u30af\u30bd\u30f3"), "wilcox")
  # a function's name, as written or as words (as before)
  expect_identical(fn_find("ard_stats_t_test")$call[1L], "cardx::ard_stats_t_test")
  expect_identical(fn_find("stats t test")$call[1L], "cardx::ard_stats_t_test")
  expect_true("cardx::ard_continuous_ci" %in% fn_find("continuous ci")$call)
})

test_that("a company with no keywords of its own has no such category", {
  skip_if_not_installed("cardx")
  m <- .std_ard_methods()[0, ]
  f <- tflspec::tfl_ard_functions()
  e <- .ard_fn_entries(m, f)
  expect_false("Company standard" %in% e$category)
  s <- .fn_search("t test", e, .fn_keywords(m, f))
  expect_identical(s$value[1L], "cardx::ard_stats_t_test")
})

test_that("a short word is a whole word only", {
  skip_if_not_installed("cardx")
  # or: not inside categorical, not the ORR of a response rate
  expect_setequal(fn_find("or")$call,
                  c("cardx::ard_regression", "cardx::ard_stats_fisher_test",
                    "cardx::ard_stats_mantelhaen_test"))
  # cox: not inside Wilcoxon
  s <- fn_find("cox")
  expect_identical(s$call[1L], "cardx::ard_regression")
  expect_false(any(grepl("wilcox", s$call)))
})

test_that("a near spelling is found when little else is", {
  skip_if_not_installed("cardx")
  s <- fn_find("wilcoxn")
  expect_true(all(s$tier == 4))
  expect_identical(s$value[1L], "wilcox")
  expect_identical(s$near_to[1L], "wilcoxon")
  expect_identical(fn_find("logstic")$call[1L], "cardx::ard_regression")
  expect_identical(fn_find("kaplan meir")$call, "cardx::ard_survival_survfit")
  # a function found: no near spellings
  expect_false(any(fn_find("lsmeans")$tier == 4))
})

test_that("a question with no function of its own leads on", {
  skip_if_not_installed("cardx")
  # MMRM: the mmrm package through ard_regression and emmeans (checked)
  s <- fn_find("MMRM")
  expect_setequal(s$call, c("cardx::ard_regression", "cardx::ard_emmeans_emmeans",
                            "cardx::ard_emmeans_contrast"))
  expect_true(all(s$rank == 2L))
  expect_match(s$note[1L], "package = \"mmrm\"", fixed = TRUE)
  # a correlation: near, to custom code, saying so
  s <- fn_find("\u76f8\u95a2", lang = "ja")
  expect_identical(s$value, "custom")
  expect_identical(s$rank, 3L)
  expect_match(s$note, "ard_*", fixed = TRUE)
  expect_identical(nrow(fn_find("xyzabc")), 0L)
  expect_null(fn_find("  - "))
})

test_that("the screen says why a function was found, and its category", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  local_home()
  s <- create_study("FS")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADSL, file.path(s$path, "data/adam/adsl.rds"))
  s$planner <- first_table(s$planner, "T1", "data/adam/adsl.rds", cards::ADSL,
                           "SAFFL", "TRT01A", c("AGE", "SEX"))
  save_study(s)
  shiny::testServer(server_for("FS"), {
    session$setInputs(nav = "make", step = "ard", target = "T1")
    session$setInputs(hot_ard_analyses_select = list(select = list(r = 2L)))
    st_env <- session$userData$st_env
    id <- function(x) paste0("st", st_env$n, "_", x)
    expect_match(output$ard_stat_ui$html, "PROC LOGISTIC", fixed = TRUE)
    inp <- list(); inp[[id("fn_q")]] <- "PROC LOGISTIC"
    do.call(session$setInputs, inp)
    h <- output$ard_fn_list$html
    expect_match(h, "ard_regression", fixed = TRUE)
    expect_match(h, "Match: PROC LOGISTIC (SAS)", fixed = TRUE)
    expect_match(h, "exponentiate = TRUE", fixed = TRUE)
    # across the categories, each row's named
    expect_match(h, "found", fixed = TRUE)
    expect_match(h, "Models", fixed = TRUE)
    # a near one says so
    inp <- list(); inp[[id("fn_q")]] <- "risk ratio"
    do.call(session$setInputs, inp)
    expect_match(output$ard_fn_list$html, "Near: ", fixed = TRUE)
    # nothing found: where to go
    inp <- list(); inp[[id("fn_q")]] <- "xyzabc"
    do.call(session$setInputs, inp)
    expect_match(output$ard_fn_list$html, "Own functions tab", fixed = TRUE)
    # a category narrows the search too
    inp <- list(); inp[[id("fn_q")]] <- "odds ratio"; inp[[id("fn_cat")]] <- "Models"
    do.call(session$setInputs, inp)
    h <- output$ard_fn_list$html
    expect_match(h, "ard_regression", fixed = TRUE)
    expect_false(grepl("ard_stats_fisher_test", h, fixed = TRUE))
    # cleared: the category alone, then all of them
    inp <- list(); inp[[id("fn_q")]] <- ""
    do.call(session$setInputs, inp)
    h <- output$ard_fn_list$html
    expect_false(grepl(" found", h, fixed = TRUE))
    expect_match(h, "ard_regression", fixed = TRUE)
    expect_false(grepl("ard_summary", h, fixed = TRUE))
    inp <- list(); inp[[id("fn_cat")]] <- ".all"
    do.call(session$setInputs, inp)
    h <- output$ard_fn_list$html
    expect_match(h, "ard_summary", fixed = TRUE)
    expect_match(h, "ard_regression", fixed = TRUE)
  })
})

test_that("the chooser is one line once a function is chosen", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  local_home()
  s <- create_study("FC")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADSL, file.path(s$path, "data/adam/adsl.rds"))
  s$planner <- first_table(s$planner, "T1", "data/adam/adsl.rds", cards::ADSL,
                           "SAFFL", "TRT01A", c("AGE", "SEX"))
  save_study(s)
  shiny::testServer(server_for("FC"), {
    session$setInputs(nav = "make", step = "ard", target = "T1")
    session$setInputs(hot_ard_analyses_select = list(select = list(r = 2L)))
    h <- output$ard_stat_ui$html
    # closed (no open attribute), "method" and the choice on one line
    expect_match(h, '<details class="ard-fn mb-2">', fixed = TRUE)
    expect_match(h, ">method</strong>", fixed = TRUE)
    expect_match(h, "Change", fixed = TRUE)
    expect_false(grepl("Chosen:", output$ard_fn_now$html, fixed = TRUE))
    # all the categories by default
    expect_match(h, '<option value=".all" selected>', fixed = TRUE)
  })
})

test_that("a function of one's own is found by its tflplanner-keywords lines", {
  f <- tempfile(fileext = ".R")
  writeLines(c(
    "# tflplanner-keywords: risk difference, Miettinen-Nurminen\u3001MN",
    "#' Risk difference",
    "#'",
    "#' The difference of two proportions.",
    "# tflplanner-keywords: PROC FREQ RISKDIFF",
    "ard_rd_mn <- function(data, ...) NULL",
    "",
    "# a helper: no keywords",
    "ard_other <- function(data, ...) NULL",
    "# tflplanner-keywords: not this one (a blank line between)",
    "",
    "ard_third <- function(data, ...) NULL"), f)
  k <- .fn_own_keywords(f)
  expect_identical(k$keywords[k$name == "ard_rd_mn"],
                   "risk difference, Miettinen-Nurminen, MN, PROC FREQ RISKDIFF")
  expect_true(is.na(k$keywords[k$name == "ard_other"]))
  expect_true(is.na(k$keywords[k$name == "ard_third"]))
  d <- .fn_own_dict(k)
  expect_identical(unique(d$fn), "ard_rd_mn")
  expect_true(all(d$rank == 1L))
  # found as any other function, by its words
  m <- .std_ard_methods()
  fs <- tflspec::tfl_ard_functions()
  e <- .ard_fn_entries(m, fs, current = "ard_rd_mn")
  s <- .fn_search("riskdiff", e, rbind(.fn_keywords(m, fs), d))
  expect_identical(s$value[1L], "ard_rd_mn")
  expect_identical(s$hit[1L], "PROC FREQ RISKDIFF")
  expect_identical(.fn_own_keywords(character())$name, character())
})

test_that("the company's own words are added to the dictionary", {
  co <- data.frame(fn = c("cardx::ard_stats_t_test", "", "cards::ard_tabulate"),
                   lang = c("sas", "en", NA), keyword = c("%m_ttest", "x", " TEAE table "),
                   rank = c(NA, "1", "7"), note_ja = NA, note_en = NA)
  d <- .fn_company_dict(co)
  expect_identical(d$keyword, c("%m_ttest", "TEAE table"))
  expect_identical(d$rank, c(1L, 1L))
  expect_identical(nrow(.fn_company_dict(NULL)), 0L)
  m <- .std_ard_methods()
  fs <- tflspec::tfl_ard_functions()
  kw <- .fn_keywords(m, fs, dict = rbind(.fn_dict(), d))
  s <- .fn_search("%m_ttest", .ard_fn_entries(m, fs), kw)
  expect_identical(s$call[1L], "cardx::ard_stats_t_test")
  expect_identical(s$hit[1L], "%m_ttest (SAS)")
  # the sheet: empty by default, kept through a workbook
  expect_identical(nrow(.builtin_standards()$ard_fn_keywords), 0L)
  skip_if_not_installed("writexl")
  x <- tempfile(fileext = ".xlsx")
  standards_template(x)
  st <- read_standards(x)
  expect_identical(names(st$ard_fn_keywords), names(.fn_dict()))
  expect_identical(nrow(st$ard_fn_keywords), 0L)
})

test_that("a new function of one's own has a keywords line to fill in", {
  skip_on_cran()
  local_home()
  s <- create_study("KW")
  s <- new_ard_function(s, "ard_kw", "summary", test = FALSE)
  f <- file.path(s$path, "programs/ard/functions/ard_kw.R")
  x <- readLines(f)
  k <- grep("^# tflplanner-keywords: $", x)
  expect_length(k, 1L)
  # at the top of the comments above the function, roxygen's untouched
  expect_true(grepl("^#'", x[k + 1L]))
  expect_true(k == 1L || !grepl("^#", x[k - 1L]))
  expect_true(is.na(study_ard_functions(s)$keywords))
  writeLines(sub("^# tflplanner-keywords: $", "# tflplanner-keywords: coefficient of variation", x), f)
  expect_identical(own_ard_functions(s)$keywords, "coefficient of variation")
})

test_that("the statistical review's points (S1, #139)", {
  skip_if_not_installed("cardx")
  # a CI of the pseudo-median is not a median CI
  s <- fn_find("median CI")
  expect_identical(s$rank[s$call == "cardx::ard_continuous_ci"], 3L)
  expect_match(s$note[s$call == "cardx::ard_continuous_ci"], "pseudo-median", fixed = TRUE)
  # Pearson: the chi-square test, and the correlation
  s <- fn_find("Pearson")
  expect_true("cardx::ard_stats_chisq_test" %in% s$call && "custom" %in% s$value)
  expect_match(fn_find("Student")$note[1L], "var.equal = TRUE", fixed = TRUE)
  # an adjusted difference
  expect_true("cardx::ard_emmeans_contrast" %in% fn_find("\u5e73\u5747\u306e\u5dee")$call)
  # SAS's defaults
  expect_match(fn_find("PROC LIFETEST")$note[1L], "log-log", fixed = TRUE)
  expect_match(fn_find("PROC PHREG")$note[1L], "breslow", fixed = TRUE)
  expect_match(fn_find("PROC FREQ CHISQ")$note[1L], "correct = FALSE", fixed = TRUE)
  expect_match(fn_find("PROC FREQ BINOMIAL")$note[1L], "waldcc", fixed = TRUE)
})

test_that("every setting in the dictionary runs as written", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  d <- .fn_dict()
  d <- d[nzchar(d$args), , drop = FALSE]
  # only functions take a setting, and each is R
  expect_true(all(grepl("::", d$fn, fixed = TRUE)))
  for (a in unique(d$args)) {
    expect_no_error(parse(text = paste0("f(", a, ")")))
  }
  adsl <- cards::ADSL[cards::ADSL$ARM != "Xanomeline Low Dose", ]
  adsl$RESP <- as.integer(adsl$AGE > 75)
  adsl$FEMALE <- adsl$SEX == "F"
  adsl$CNT <- rep(0:4, length.out = nrow(adsl))
  adsl$SEX2 <- factor(ifelse(adsl$AGE > 70, adsl$SEX, ifelse(adsl$SEX == "F", "M", "F")))
  adtte <- cards::ADTTE
  surv <- "survival::Surv(AVAL, 1 - CNSR)"
  # the data and the analysis each function is tried on
  base <- c(
    "cardx::ard_stats_t_test" = "adsl, by = ARM, variables = AGE",
    "cardx::ard_stats_wilcox_test" = "adsl, by = ARM, variables = AGE",
    "cardx::ard_stats_chisq_test" = "adsl, by = ARM, variables = SEX",
    "cardx::ard_stats_mantelhaen_test" = "adsl, by = ARM, variables = SEX, strata = AGEGR1",
    "cardx::ard_stats_mcnemar_test" = "adsl, by = SEX, variables = SEX2",
    "cardx::ard_stats_prop_test" = "adsl, by = ARM, variables = FEMALE",
    "cardx::ard_categorical_ci" = "adsl, variables = FEMALE",
    "cardx::ard_survival_survdiff" = paste(surv, "~ TRTA, data = adtte"),
    "cardx::ard_survival_survfit" = paste0("adtte, y = ", surv, ", variables = \"TRTA\", times = 60"))
  regression <- c(
    'method = "glm", method.args = list(family = binomial), exponentiate = TRUE' = "adsl, formula = RESP ~ ARM",
    'method = "glm", method.args = list(family = poisson), exponentiate = TRUE' = "adsl, formula = CNT ~ ARM",
    'method = "coxph", package = "survival", exponentiate = TRUE' = paste0("adtte, formula = ", surv, " ~ TRTA"),
    'method = "coxph", package = "survival", method.args = list(ties = "breslow"), exponentiate = TRUE' =
      paste0("adtte, formula = ", surv, " ~ TRTA"),
    'method = "glm.nb", package = "MASS", exponentiate = TRUE' = "adsl, formula = CNT ~ ARM")
  run <- function(fn, b, a) {
    r <- suppressWarnings(eval(parse(text = paste0(fn, "(", b, ", ", a, ")"))))
    err <- unlist(r$error)
    expect_true(nrow(r) > 0 && !length(err), info = paste(fn, a))
  }
  # (a model's table needs broom.helpers; R CMD check offers only the
  # packages the DESCRIPTION names)
  has <- function(p) requireNamespace(p, quietly = TRUE)
  done <- 0L
  for (i in seq_len(nrow(d))) {
    fn <- d$fn[i]
    a <- d$args[i]
    if (grepl("mmrm", a, fixed = TRUE)) next
    if (fn == "cardx::ard_regression" && !has("broom.helpers")) next
    if (grepl("MASS", a, fixed = TRUE) && !has("MASS")) next
    b <- if (fn == "cardx::ard_regression") regression[[a]] else base[[fn]]
    expect_false(is.null(b), info = paste(fn, a))
    run(fn, b, a)
    done <- done + 1L
  }
  expect_true(done > 20L)
  # MMRM (the mmrm package)
  skip_if_not_installed("mmrm")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("broom.helpers")
  skip_if_not_installed("pharmaverseadam")
  advs <- as.data.frame(getExportedValue("pharmaverseadam", "advs"))
  advs <- advs[advs$PARAMCD == "SYSBP" & !is.na(advs$CHG) &
                 advs$AVISIT %in% c("Week 2", "Week 4", "Week 8") &
                 advs$ATPT %in% "AFTER LYING DOWN FOR 5 MINUTES" &
                 advs$TRTA %in% c("Placebo", "Xanomeline High Dose"), ]
  advs <- advs[!duplicated(advs[c("USUBJID", "AVISIT")]), ]
  for (k in c("TRTA", "USUBJID", "AVISIT")) advs[[k]] <- factor(advs[[k]])
  mm <- 'method = "mmrm", package = "mmrm"'
  run("cardx::ard_regression", "advs, formula = CHG ~ TRTA * AVISIT + us(AVISIT | USUBJID)", mm)
  for (fn in c("cardx::ard_emmeans_emmeans", "cardx::ard_emmeans_contrast")) {
    run(fn, paste("advs, formula = CHG ~ TRTA + AVISIT + BASE + us(AVISIT | USUBJID),",
                  "response_type = \"continuous\""), mm)
  }
})

test_that("a word that means a setting brings it", {
  skip_if_not_installed("cardx")
  s <- fn_find("PROC LOGISTIC")
  expect_identical(s$args[1L], 'method = "glm", method.args = list(family = binomial), exponentiate = TRUE')
  expect_identical(fn_find("PROC PHREG")$args[1L],
                   'method = "coxph", package = "survival", method.args = list(ties = "breslow"), exponentiate = TRUE')
  expect_identical(fn_find("clopper")$args[1L], 'method = "clopper-pearson"')
  # a word that means none
  expect_true(is.na(fn_find("SMD")$args[1L]))
  # the company's sheet may leave the column out
  co <- data.frame(fn = "cardx::ard_stats_t_test", lang = "sas", keyword = "%m_ttest",
                   rank = "1", note_ja = NA, note_en = NA)
  expect_identical(.fn_company_dict(co)$args, "")
})

test_that("the form starts from the setting a word means", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  local_home()
  s <- create_study("FP")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADSL, file.path(s$path, "data/adam/adsl.rds"))
  s$planner <- first_table(s$planner, "T1", "data/adam/adsl.rds", cards::ADSL,
                           "SAFFL", "TRT01A", c("AGE", "SEX"))
  save_study(s)
  shiny::testServer(server_for("FP"), {
    session$setInputs(nav = "make", step = "ard", target = "T1")
    session$setInputs(hot_ard_analyses_select = list(select = list(r = 2L)))
    st_env <- session$userData$st_env
    id <- function(x) paste0("st", st_env$n, "_", x)
    inp <- list(); inp[[id("fn_q")]] <- "PROC LOGISTIC"
    do.call(session$setInputs, inp)
    expect_match(output$ard_fn_list$html, 'Its setting: method = "glm"', fixed = TRUE)
    # nothing offered before a function is picked
    expect_null(output$ard_fn_preset)
    inp <- list(); inp[[id("fn_pick")]] <- "cardx::ard_regression"
    do.call(session$setInputs, inp)
    expect_match(output$ard_fn_preset$html, "Start with this setting", fixed = TRUE)
    session$setInputs(ard_fn_preset_go = 1)
    expect_match(output$ard_fn_preset$html, "Started from the setting", fixed = TRUE)
    h <- output$ard_an_args$html
    expect_match(h, 'value="glm"', fixed = TRUE)
    expect_match(h, "exponentiate = TRUE", fixed = TRUE)
  })
})

test_that("a setting that replaces the analysis's own arguments says so", {
  skip_on_cran()
  skip_if_not_installed("cardx")
  local_home()
  s <- create_study("FR")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cards::ADSL, file.path(s$path, "data/adam/adsl.rds"))
  s$planner <- first_table(s$planner, "T1", "data/adam/adsl.rds", cards::ADSL,
                           "SAFFL", "TRT01A", c("AGE", "SEX"))
  a <- ard_rows(s$planner, "analyses", "T1")
  k <- which(a$analysis_id == "CONT")
  a$method[k] <- "cardx::ard_regression"
  a$args[k] <- 'formula = AGE ~ TRT01A, method = "lm"'
  s$planner <- set_ard_rows(s$planner, "analyses", "T1", a)
  save_study(s)
  shiny::testServer(server_for("FR"), {
    session$setInputs(nav = "ard", target = "T1")
    session$setInputs(ard_ol_pick = "CONT")
    st_env <- session$userData$st_env
    id <- function(x) paste0("st", st_env$n, "_", x)
    inp <- list(); inp[[id("fn_q")]] <- "PROC LOGISTIC"
    do.call(session$setInputs, inp)
    inp <- list(); inp[[id("fn_pick")]] <- "cardx::ard_regression"
    do.call(session$setInputs, inp)
    h <- output$ard_fn_preset$html
    expect_match(h, "Start with this setting", fixed = TRUE)
    expect_match(h, "It replaces the analysis's own arguments", fixed = TRUE)
    expect_match(h, 'method = "lm"', fixed = TRUE)
    session$setInputs(ard_fn_preset_go = 1)
    expect_match(output$ard_fn_preset$html, "It replaces the analysis's own arguments", fixed = TRUE)
  })
})
