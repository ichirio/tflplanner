# A report's code list that is one of the study's lists (tflspec#977, A7:
# `{list: SEX}`, the codelists sheet's `list` cell): its rows are the
# study's -- made again from the study's lists when these change, read
# only in the grids and in set_codelist() (R/codelist_refs.R).

# (a tflspec before #977 has no `list` column: nothing of this to test)
skip_without_list_refs <- function() {
  skip_if_not("list" %in% sheet_columns("codelists"), "this tflspec has no code-list references (tflspec#977)")
}

test_that("a reference's rows are made from the study's lists, the other rows kept", {
  scl <- list(SEX = c(F = "Female", M = "Male", U = "Unknown"), RACE = c("ASIAN", "WHITE"))
  d <- data.frame(output_id = c("T1", "T1", "T1", "T2", "T2", "T2", "T3", "T3"),
                  variable = c("SEX", "SEX", "AGEGR1", "SEX", "RACE", "ETH", "SEX", "SEX"),
                  value = c("F", "M", "<65", "x", "y", "z", "F", "M"),
                  label = c("Old F", "Male", "Under 65", "x", NA, NA, NA, NA),
                  order = c("1", "2", "1", "1", "1", "1", "1", "2"), under = NA_character_,
                  list = c("SEX", "SEX", NA, "SEX: M | F", "RACE", "NOPE", "SEX", NA),
                  note = NA_character_, stringsAsFactors = FALSE)
  r <- .cl_refs_remake(d, scl)
  t1 <- r[r$output_id == "T1", ]
  # all the list's codes in its order, the study's labels
  expect_identical(t1$value, c("F", "M", "U", "<65"))
  expect_identical(t1$label, c("Female", "Male", "Unknown", "Under 65"))
  expect_identical(t1$order, c("1", "2", "3", "1"))
  expect_identical(t1$list, c("SEX", "SEX", "SEX", NA))
  # the codes picked, in the order picked; a list of codes alone: the code
  t2 <- r[r$output_id == "T2", ]
  expect_identical(t2$value, c("M", "F", "ASIAN", "WHITE", NA))
  expect_identical(t2$label, c("Male", "Female", "ASIAN", "WHITE", NA))
  # a list the study has not got: one row, its cell and no code (as tflspec)
  expect_identical(t2$list[5L], "NOPE")
  expect_true(is.na(t2$order[5L]))
  # cells that differ: left as they are (tflspec stops on them, naming them)
  expect_identical(r[r$output_id == "T3", ], d[d$output_id == "T3", ], ignore_attr = TRUE)
  # nothing a reference: the same rows
  expect_identical(.cl_refs_remake(d[3L, ], scl), d[3L, ])
  # a code the study's list has not got: the cell alone too
  d2 <- d[4L, ]
  d2$list <- "SEX: M | X"
  expect_identical(.cl_refs_remake(d2, scl)$value, NA_character_)
})

test_that("a grid's edit of a study's list, or of the list column, is put back", {
  old <- data.frame(variable = c("SEX", "SEX", "AGEGR1"), value = c("F", "M", "<65"),
                    label = c("Female", "Male", "Under 65"), order = c("1", "2", "1"),
                    under = NA_character_, list = c("SEX", "SEX", NA), note = NA_character_,
                    stringsAsFactors = FALSE)
  # unchanged: nothing kept
  k <- .cl_refs_keep(old, old)
  expect_identical(attr(k, "kept"), character())
  expect_identical(k, old, ignore_attr = TRUE)
  # a label of the study's list, a row of it gone, a row added to it
  d <- old
  d$label[1L] <- "Woman"
  k <- .cl_refs_keep(d, old)
  expect_identical(attr(k, "kept"), "SEX")
  expect_identical(k, old, ignore_attr = TRUE)
  k <- .cl_refs_keep(old[-2L, ], old)
  expect_identical(attr(k, "kept"), "SEX")
  expect_identical(k, old, ignore_attr = TRUE)
  # (the report's own rows: edited as the grid has them, the study's where they were)
  d <- rbind(old[3L, ], old[1:2, ])
  d$label[1L] <- "Young"
  d <- rbind(d, data.frame(variable = "SEX", value = "U", label = NA, order = "3", under = NA,
                           list = NA, note = NA))
  k <- .cl_refs_keep(d, old)
  expect_identical(attr(k, "kept"), "SEX")
  expect_identical(k$variable, c("AGEGR1", "SEX", "SEX"))
  expect_identical(k$label, c("Young", "Female", "Male"))
  # the `list` cell typed into a report's own rows: not taken
  d <- old
  d$list[3L] <- "AGEGR1"
  k <- .cl_refs_keep(d, old)
  expect_identical(attr(k, "kept"), "AGEGR1")
  expect_true(is.na(k$list[3L]))
  expect_match(.cl_refs_kept_text("SEX", identity), "Not changed: SEX.", fixed = TRUE)
})

test_that("a writer's defaults.yml takes the spec's own blocks (code lists, ADaM metadata) from its text", {
  old <- c("content:", "- plan_style: {}", "# the study's lists", "codelists:", "  SEX: [F, M]",
           "adam_meta:", "  variables: []", "styles:", "  standard: {}")
  # (the writer's copy: older -- from the file on disk -- and of another form)
  new <- c("content:", "- plan_style: {size: 9}", "codelists:", "  SEX:", "  - F", "  - M",
           "adam_meta:", "  variables:", "  - x", "styles:", "  standard: {}")
  expect_identical(.keep_study_blocks(new, old),
                   c("content:", "- plan_style: {size: 9}", old[3:7], "styles:", "  standard: {}"))
  # a block the spec's text has not: the writer's
  expect_identical(.keep_study_blocks(new, old[1:2]), new)
})

# a report file's code lists, as read (a reference: list(list = <name>, ...))
file_codelists <- function(lines) yaml::yaml.load(paste(lines, collapse = "
"))$codelists

# a study whose report T1 refers to the study's list SEX (and picks AGEGR1's codes)
ref_study <- function(id = "REF") {
  s <- create_study(id, planner = add_output(new_planner(), "T1", type = "table", description = "one"))
  dflt <- file.path(s$path, "spec", "defaults.yml")
  writeLines(.set_study_block(readLines(dflt, encoding = "UTF-8"), "codelists", c(
    "codelists:", "  SEX:", "    - {value: F, label: Female}", "    - {value: M, label: Male}",
    "  AGEGR1:", "    - {value: '<65', label: Under 65}", "    - {value: '>=65', label: 65 or over}")),
    dflt, useBytes = TRUE)
  f <- file.path(s$path, "spec", "outputs", "T1.yml")
  writeLines(.set_study_block(readLines(f, encoding = "UTF-8"), "codelists", c(
    "codelists:", "  SEX: {list: SEX}", "  AGEGR1: {list: AGEGR1, codes: ['>=65', '<65']}",
    "  RACE:", "    - {value: ASIAN, label: Asian}", "    - {value: WHITE, label: White}")),
    f, useBytes = TRUE)
  suppressMessages(reload_from_spec(id))
}

test_that("a study's list in a report: its rows on open, set_codelist(), the study's lists changed", {
  skip_on_cran()
  skip_without_list_refs()
  local_home()
  s <- ref_study()
  cl <- sheet_rows(s$planner, "codelists", "T1")
  expect_identical(cl$value[cl$variable == "SEX"], c("F", "M"))
  expect_identical(cl$label[cl$variable == "SEX"], c("Female", "Male"))
  expect_identical(unique(cl$list[cl$variable == "AGEGR1"]), "AGEGR1: >=65 | <65")
  expect_identical(cl$value[cl$variable == "AGEGR1"], c(">=65", "<65"))
  # the rows tflspec gave are the rows made here (one reading of the cell)
  expect_identical(.cl_refs_remake(s$planner$sheets$codelists, s$planner$spec$study$codelists),
                   s$planner$sheets$codelists)
  # set_codelist(): the study's lists are read only, the report's own are not
  expect_error(set_codelist(s$planner, "T1", data.frame(variable = "SEX", value = "U")),
               "SEX in T1 is one of the study's lists")
  p <- set_codelist(s$planner, "T1", data.frame(variable = "RACE", value = "BLACK"))
  expect_identical(sheet_rows(p, "codelists", "T1")$value[sheet_rows(p, "codelists", "T1")$variable == "RACE"],
                   c("ASIAN", "WHITE", "BLACK"))
  # the study's lists changed in memory (S8's editor; a defaults.yml read
  # again): the rows made again, the view and its base alike -- no edit
  p <- s$planner
  p$spec$study$codelists$SEX <- c(M = "Men", F = "Women", U = "Unknown")
  p <- .spec_codelist_refs(p)
  cl <- sheet_rows(p, "codelists", "T1")
  expect_identical(cl$value[cl$variable == "SEX"], c("M", "F", "U"))
  expect_identical(cl$label[cl$variable == "SEX"], c("Men", "Women", "Unknown"))
  expect_identical(.planner_body(p), p$spec$base)
  # the programs read the rows: the new list
  expect_identical(.study_codelists(p)$label[.study_codelists(p)$variable == "SEX"],
                   c("Men", "Women", "Unknown"))
  # (no edit of a report: nothing to write)
  p2 <- .spec_sync(p, p$spec$meta)
  expect_length(attr(p2, "changed"), 0L)
  # an edit of the report's own list: its file alone, the reference kept, the
  # study's lists given to the bridge (no warning) and defaults.yml's text kept
  p <- set_codelist(s$planner, "T1", data.frame(variable = "RACE", value = "ASIAN", label = "Asian people"))
  expect_no_warning(p2 <- .spec_sync(p, p$spec$meta))
  expect_identical(attr(p2, "changed"), "spec/outputs/T1.yml")
  y <- p2$spec$text[["spec/outputs/T1.yml"]]
  expect_identical(file_codelists(y)$SEX, list(list = "SEX"))
  expect_true(any(grepl("Asian people", y, fixed = TRUE)))
  expect_identical(p2$spec$text[["spec/defaults.yml"]], s$planner$spec$text[["spec/defaults.yml"]])
  # the bridge's sheets: the study's lists as tflspec's own sheets give them
  sh <- .planner_sheets(s$planner, s$meta)
  expect_identical(sh$study_codelists,
                   tflspec::tfl_sheets_from_spec(s$planner$spec$study, s$planner$spec$outputs)$study_codelists,
                   ignore_attr = TRUE)
  # defaults.yml changed on disk, the study read again: the new rows
  dflt <- file.path(s$path, "spec", "defaults.yml")
  x <- readLines(dflt, encoding = "UTF-8")
  writeLines(sub("label: Female}", "label: Woman}", x, fixed = TRUE), dflt, useBytes = TRUE)
  s2 <- suppressMessages(reload_from_spec("REF"))
  cl <- sheet_rows(s2$planner, "codelists", "T1")
  expect_identical(cl$label[cl$variable == "SEX"], c("Woman", "Male"))
  expect_identical(file_codelists(readLines(file.path(s$path, "spec", "outputs", "T1.yml")))$SEX,
                   list(list = "SEX"))
})

test_that("the code lists' dialog: a study's list is shown read only, an edit of it put back", {
  skip_on_cran()
  skip_if_quick()
  skip_without_list_refs()
  local_home()
  s <- ref_study()
  shiny::testServer(server_for("REF"), {
    rv <- session$userData$rv
    session$setInputs(target = "T1", nav = "make", step = "page")
    session$setInputs(cl21_open = 1, cl21_all = TRUE)
    w <- jsonlite::fromJSON(output$cl21_hot, simplifyVector = FALSE)$x
    hdr <- unlist(w$colHeaders)
    # the list column, and the study's lists' rows: read only
    ro <- vapply(w$columns, function(cc) isTRUE(cc$readOnly), NA)
    expect_true(ro[match("list", hdr)])
    cells <- Filter(function(cc) isTRUE(cc$readOnly), w$cell %||% list())
    rows <- unique(vapply(cells, function(cc) as.integer(cc$row), 0L)) + 1L
    vars <- vapply(w$data, function(r) r[[match("variable", hdr)]] %||% "", "")
    expect_setequal(vars[rows], c("SEX", "AGEGR1"))
    expect_match(output$cl21_refs$html, "SEX (SEX), AGEGR1 (AGEGR1: &gt;=65 | &lt;65)", fixed = TRUE)
    # an edit of SEX's label (what the browser would refuse): put back
    before <- rv$p
    i <- match("SEX", vars)
    data <- w$data
    data[[i]][[match("label", hdr)]] <- "Woman"
    session$setInputs(cl21_hot = list(
      data = data, changes = list(event = "afterChange", changes = list(list(i - 1L, "label", "Female", "Woman"))),
      params = list(planner_key = w$planner_key, rClass = "data.frame", rColHeaders = as.list(hdr),
                    rColClasses = w$rColClasses, rDataDim = list(length(data), length(hdr)))))
    expect_identical(rv$p$sheets$codelists, before$sheets$codelists)
    # the report's own list: edited as before
    w <- jsonlite::fromJSON(output$cl21_hot, simplifyVector = FALSE)$x
    data <- w$data
    j <- match("RACE", vapply(data, function(r) r[[match("variable", hdr)]] %||% "", ""))
    data[[j]][[match("label", hdr)]] <- "Asian people"
    session$setInputs(cl21_hot = list(
      data = data, changes = list(event = "afterChange", changes = list(list(j - 1L, "label", "Asian", "Asian people"))),
      params = list(planner_key = w$planner_key, rClass = "data.frame", rColHeaders = as.list(hdr),
                    rColClasses = w$rColClasses, rDataDim = list(length(data), length(hdr)))))
    cl <- sheet_rows(rv$p, "codelists", "T1")
    expect_identical(cl$label[cl$variable == "RACE"], c("Asian people", "White"))
    expect_identical(cl$list[cl$variable == "SEX"], c("SEX", "SEX"))
    session$setInputs(save = 1)
  })
  y <- readLines(file.path(s$path, "spec", "outputs", "T1.yml"), encoding = "UTF-8")
  expect_identical(file_codelists(y)$SEX, list(list = "SEX"))
  expect_true(any(grepl("Asian people", y, fixed = TRUE)))
  # (the study's lists: defaults.yml's own, which the save keeps)
  expect_identical(tflspec::tfl_read_study_spec(s$path)$codelists$SEX, c(F = "Female", M = "Male"))
})
