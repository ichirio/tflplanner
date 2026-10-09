# 1-1's columns made (analysis_data$derive), by kind: R/derive_builder.R

test_that("the four kinds read back and are written as they were", {
  same <- function(x) {
    r <- .drv_read(x)
    r$text <- NULL
    .drv_write(r)
  }
  x <- "AGEGRP = ifelse(AGE < 65, \"<65\", \">=65\")"
  r <- .drv_read(x)
  expect_identical(r$kind, "cond")
  expect_identical(r$branches[[1L]], list(cond = "AGE < 65", value = "<65"))
  expect_identical(r$otherwise, ">=65")
  expect_identical(same(x), x)
  x <- "G = dplyr::case_when(AGE < 65 ~ \"a\", AGE < 75 ~ \"b\", TRUE ~ \"c\")"
  expect_identical(.drv_read(x)$kind, "cond")
  expect_length(.drv_read(x)$branches, 2L)
  expect_identical(same(x), x)
  x <- "C = cut(AGE, c(-Inf, 65, 75, Inf), c(\"<65\", \"65-74\", \">=75\"), right = FALSE)"
  r <- .drv_read(x)
  expect_identical(r[c("kind", "var", "breaks", "labels")],
                   list(kind = "cut", var = "AGE", breaks = "65, 75", labels = "<65 | 65-74 | >=75"))
  expect_identical(same(x), x)
  x <- "DUR = as.numeric(AENDT - ASTDT) + 1"
  r <- .drv_read(x)
  expect_identical(r[c("kind", "end", "start", "plus1")],
                   list(kind = "days", end = "AENDT", start = "ASTDT", plus1 = TRUE))
  expect_identical(same(x), x)
  # a text that reads as a number keeps its quotes; a number stays one
  x <- "F = ifelse(X == \"Y\", \"1\", 0)"
  expect_identical(.drv_read(x)$branches[[1L]]$value, "\"1\"")
  expect_identical(same(x), x)
  # what the four cannot hold: an R expression, as written
  for (x in c("C2 = cut(AGE, c(0, 65, Inf))", "P = paste(A, B)",
              "Q = ifelse(A > 1, B, \"x\")", "R = ifelse(A > 1, \"y\", \"x\", 3)")) {
    r <- .drv_read(x)
    expect_identical(r$kind, "r", info = x)
    expect_identical(same(x), x, info = x)
  }
})

test_that("a derive's columns: each kept as written unless made again", {
  d <- "G = ifelse(AGE < 65, \"a\", ifelse(AGE < 75, \"b\", \"c\")) | PHASE = APHASE"
  v <- .drv_read_all(d)
  expect_identical(vapply(v, `[[`, "", "kind"), c("cond", "r"))
  # untouched: the nested ifelse() as it was, not a case_when()
  expect_identical(.drv_text(v), d)
  expect_identical(.drv_read_all(NA), list())
  expect_identical(.drv_text(list()), NA_character_)
})

test_that("a column the form makes: the errors in words", {
  expect_error(.drv_write(list(name = "", kind = "r", expr = "A")), "needs a name")
  expect_error(.drv_write(list(name = "X", kind = "cond",
                               branches = list(list(cond = NA, value = "a")))),
               "at least one condition")
  expect_error(.drv_write(list(name = "X", kind = "cut", var = "AGE", breaks = "65, x")),
               "numbers")
  expect_error(.drv_write(list(name = "X", kind = "cut", var = "AGE", breaks = "75, 65")),
               "go up")
  expect_error(.drv_write(list(name = "X", kind = "cut", var = "AGE", breaks = "65",
                               labels = "a | b | c")), "give 2 labels")
  expect_error(.drv_write(list(name = "X", kind = "days", end = "A", start = "")), "two dates")
  expect_error(.drv_write(list(name = "X", kind = "r", expr = "A +")), "does not read as R")
  # a blank value is NA; a word is quoted
  expect_identical(.drv_write(list(name = "X", kind = "cond",
                                   branches = list(list(cond = "A > 1", value = "high")),
                                   otherwise = "")),
                   "X = ifelse(A > 1, \"high\", NA)")
  expect_identical(.drv_write(list(name = "X", kind = "cut", var = "AGE", breaks = "65")),
                   "X = cut(AGE, c(-Inf, 65, Inf), right = FALSE)")
})

test_that("1-1: a column made in the form, saved as the sheet's derive", {
  skip_if_not_installed("cards")
  local_home()
  p <- add_output(new_planner(), "DM")
  p$ard$datasets <- data.frame(dataset = "ADSL", path = "data/adam/adsl.rds")
  p$ard$populations <- data.frame(population_id = "SAF", dataset = "ADSL",
                                  where = "SAFFL == \"Y\"")
  for (s in names(p$ard)) p$ard[[s]] <- .normalize_ard_sheet(p$ard[[s]], s)
  p <- set_analysis_data(p, "DM", "adsl_saf", from = "ADSL", population_id = "SAF",
                         derive = "G = ifelse(AGE < 65, \"a\", ifelse(AGE < 75, \"b\", \"c\"))")
  s <- create_study("DV", planner = p)
  saveRDS(as.data.frame(cards::ADSL), file.path(s$path, "data", "adam", "adsl.rds"))
  shiny::testServer(server_for("DV"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "ard", target = "DM")
    session$setInputs(ard_adata_pick = "adsl_saf")
    expect_match(output$drv_list$html, "Split by conditions", fixed = TRUE)
    session$setInputs(adata_id = "adsl_saf", adata_label = "", adata_from = "ADSL",
                      adata_add = NULL, adata_keep = NULL, adata_distinct = NULL)
    # a group cut from AGE
    session$setInputs(drv_new = 1)
    f <- session$userData$drv$fid
    set <- function(...) {
      a <- list(...)
      do.call(session$setInputs, stats::setNames(a[c(FALSE, TRUE)], vapply(a[c(TRUE, FALSE)], f, "")))
    }
    expect_match(output$drv_editor$html, f("name"), fixed = TRUE)
    set("name", "AGEGR", "kind", "cut")
    expect_match(output$drv_kind_ui$html, f("breaks"), fixed = TRUE)
    set("var", "AGE", "breaks", "65, 75", "labels", "<65 | 65-74 | >=75")
    session$setInputs(drv_ok = 1)
    expect_match(output$drv_list$html, "AGEGR = cut(AGE, c(-Inf, 65, 75, Inf)", fixed = TRUE)
    # a split: the condition from the builder, its value, otherwise
    session$setInputs(drv_new = 2)
    set("name", "OLD", "kind", "cond")
    b <- session$userData$drv
    b$cond_value("AGE >= 75")
    b$cond_key(shiny::isolate(b$cond_key()) + 1L)
    session$flushReact()
    set("val1", "Y", "else", "N")
    session$setInputs(drv_ok = 2)
    expect_match(output$drv_list$html, "OLD = ifelse(AGE &gt;= 75, &quot;Y&quot;, &quot;N&quot;)", fixed = TRUE)
    # saved: the first as it was written (not opened), then the two made
    session$setInputs(adata_save = 1)
    d <- .adata_rows(rv$p, "DM")$derive
    expect_identical(d, paste(
      "G = ifelse(AGE < 65, \"a\", ifelse(AGE < 75, \"b\", \"c\"))",
      "AGEGR = cut(AGE, c(-Inf, 65, 75, Inf), c(\"<65\", \"65-74\", \">=75\"), right = FALSE)",
      "OLD = ifelse(AGE >= 75, \"Y\", \"N\")", sep = " | "))
    # a name made already is refused
    session$setInputs(drv_new = 3)
    set("name", "OLD", "kind", "r", "expr", "1")
    session$setInputs(drv_ok = 3)
    expect_length(.drv_read_all(b$text()), 3L)
    # one taken out
    session$setInputs(drv_cancel = 1, drv_del = 1)
    expect_false(grepl("G = ifelse", b$text(), fixed = TRUE))
  })
})
