# The study's model is its YAML spec; the sheets are a view of it (#407 B1)

model_sample <- function(env = parent.frame()) {
  home <- withr_tempdir(env)
  old <- options(tflplanner.home = home)
  do.call(on.exit, list(substitute(options(old)), add = TRUE), envir = env)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  suppressMessages(create_sample_study(run = FALSE, home = home))
}

spec_md5 <- function(root) {
  f <- .spec_files_yaml(root)
  stats::setNames(unname(tools::md5sum(file.path(root, f))), f)
}

changed_files <- function(a, b) {
  k <- union(names(a), names(b))
  k[!vapply(k, function(x) identical(a[x], b[x]), NA)]
}

test_that("an opened study holds its spec, in step with the sheets' view", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  s <- suppressMessages(open_study("SAMPLE-01", home = home))
  sp <- s$planner$spec
  expect_s3_class(sp$study, "tfl_study_spec")
  expect_setequal(names(sp$outputs), s$planner$outputs$output_id)
  expect_true(all(vapply(sp$outputs, inherits, NA, "tfl_output")))
  expect_identical(sp$where[["T-14-1-1"]], "spec/outputs/T-14-1-1.yml")
  expect_true(all(c(.study_file, "spec/toc.yml") %in% names(sp$text)))
  expect_true(.same_planner(sp$base, s$planner))
  # the view made from the spec is the sheets the screens read
  v <- .planner_from_spec(sp$study, sp$outputs)
  expect_identical(v$sheets$tables, s$planner$sheets$tables)
  expect_identical(v$outputs, .planner_body(s$planner)$outputs)
  # the spec is not part of what is compared or kept
  expect_null(.state_of(s)$planner$spec)
  expect_true(.same_planner(s$planner, .planner_body(s$planner)))
})

test_that("opened and saved with no change, no spec file is written", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  m0 <- spec_md5(s$path)
  s <- suppressMessages(open_study("SAMPLE-01", home = home))
  s <- suppressMessages(save_study(s, home = home))
  expect_identical(changed_files(m0, spec_md5(s$path)), character())
  y <- s$files[grepl("\\.ya?ml$", s$files$file), ]
  expect_true(all(y$status == "unchanged"))
})

test_that("an edit of one report's sheet rows writes that report's file only", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  m0 <- spec_md5(s$path)
  id <- "T-14-1-1"
  d <- sheet_rows(s$planner, "tables", id)
  d$output_id <- NULL
  d$note <- "an edit"
  s$planner <- set_sheet_rows(s$planner, "tables", id, d)
  s <- suppressMessages(save_study(s, home = home))
  expect_identical(changed_files(m0, spec_md5(s$path)), "spec/outputs/T-14-1-1.yml")
  # the spec in memory follows, in step again
  expect_true(.same_planner(s$planner$spec$base, s$planner))
  expect_identical(s$planner$spec$text[["spec/outputs/T-14-1-1.yml"]],
                   readLines(file.path(s$path, "spec/outputs/T-14-1-1.yml"), encoding = "UTF-8"))
  # and reads back as saved
  s2 <- suppressMessages(open_study("SAMPLE-01", home = home))
  expect_identical(sheet_rows(s2$planner, "tables", id)$note[1L], "an edit")
})

test_that("what the sheets cannot hold stays in a report the edits do not touch", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  # a page key with no sheet, written straight into one report's spec
  o <- s$planner$spec$outputs[["T-14-3-1"]]
  o$page$kept_key <- "kept"
  s$planner <- .spec_set_output(s$planner, "T-14-3-1", o)
  expect_identical(s$planner$spec$outputs[["T-14-3-1"]]$page$kept_key, "kept")
  # an edit of another report through its sheets
  d <- sheet_rows(s$planner, "tables", "T-14-1-1")
  d$output_id <- NULL
  d$note <- "another edit"
  s$planner <- set_sheet_rows(s$planner, "tables", "T-14-1-1", d)
  m0 <- spec_md5(s$path)
  s <- suppressMessages(save_study(s, home = home))
  expect_setequal(changed_files(m0, spec_md5(s$path)),
                  c("spec/outputs/T-14-1-1.yml", "spec/outputs/T-14-3-1.yml"))
  back <- tflspec::tfl_read_output(file.path(s$path, "spec/outputs/T-14-3-1.yml"))
  expect_identical(back$page$kept_key, "kept")
  # the programs as before
  expect_identical(program_code(s$planner, "T-14-3-1"),
                   program_code(.planner_body(s$planner), "T-14-3-1"))
})

test_that("an edit of a report's sheets keeps what only its spec holds (#415)", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  id <- "T-14-3-1"
  rel <- "spec/outputs/T-14-3-1.yml"
  # a content argument and a page key with no sheet, written into the spec
  o <- s$planner$spec$outputs[[id]]
  k <- which(vapply(o$content, function(i) i$fn, "") == "table_plan")
  o$content[[k]]$args$kept_arg <- "kept"
  o$page$kept_key <- "kept"
  s$planner <- .spec_set_output(s$planner, id, o)
  # (a) the same report's ARD sheet and page sheet edited, then saved
  a <- ard_rows(s$planner, "analyses", id)
  a$output_id <- NULL
  a$label <- "An edited label"
  s$planner <- set_ard_rows(s$planner, "analyses", id, a)
  f <- sheet_rows(s$planner, "footer", id)
  f$output_id <- NULL
  f$left <- "An edited footnote."
  s$planner <- set_sheet_rows(s$planner, "footer", id, f)
  m0 <- spec_md5(s$path)
  s <- suppressMessages(save_study(s, home = home))
  expect_identical(changed_files(m0, spec_md5(s$path)), rel)
  back <- tflspec::tfl_read_output(file.path(s$path, rel))
  # the edits are in
  expect_identical(back$ard$analyses[[1L]]$label, "An edited label")
  expect_identical(back$page$footer[["1"]]$left, "An edited footnote.")
  # and what only the spec held is kept
  expect_identical(back$content[[k]]$args$kept_arg, "kept")
  expect_identical(back$page$kept_key, "kept")
  # the other parts as they were
  expect_identical(back$content[-k], o$content[-k])
  expect_identical(back$codelists, o$codelists)
  # saved again with no change: no file written
  m1 <- spec_md5(s$path)
  s <- suppressMessages(save_study(s, home = home))
  expect_identical(changed_files(m1, spec_md5(s$path)), character())
})

test_that("a draft keeps what only the spec holds, and gives it back (#415)", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  s <- suppressMessages(open_study("SAMPLE-01", home = home))
  id <- "T-14-3-1"
  rel <- "spec/outputs/T-14-3-1.yml"
  # a draft of nothing changed holds no spec
  expect_null(.draft_spec_of(s))
  o <- s$planner$spec$outputs[[id]]
  k <- which(vapply(o$content, function(i) i$fn, "") == "table_plan")
  o$content[[k]]$args$kept_arg <- "kept"
  s$planner <- .spec_set_output(s$planner, id, o)
  # then the same report's page sheet edited (not yet in the spec)
  f <- sheet_rows(s$planner, "footer", id)
  f$output_id <- NULL
  f$left <- "An edited footnote."
  s$planner <- set_sheet_rows(s$planner, "footer", id, f)
  .write_draft(s, home)
  d <- .read_draft("SAMPLE-01", home)
  expect_false(is.null(d$planner$spec))
  expect_identical(d$planner$spec$outputs[[id]]$content[[k]]$args$kept_arg, "kept")
  # taken back (as the app does) and saved: both are in the file
  d$planner <- .study_spec_keys(d$planner)
  m0 <- spec_md5(d$path)
  d <- suppressMessages(save_study(d, home = home))
  expect_identical(changed_files(m0, spec_md5(d$path)), rel)
  back <- tflspec::tfl_read_output(file.path(d$path, rel))
  expect_identical(back$content[[k]]$args$kept_arg, "kept")
  expect_identical(back$page$footer[["1"]]$left, "An edited footnote.")
  # a report removed in the spec stays removed
  s <- suppressMessages(open_study("SAMPLE-01", home = home))
  s$planner <- .spec_set_output(s$planner, "T-14-1-2", NULL)
  .write_draft(s, home)
  d <- .read_draft("SAMPLE-01", home)
  expect_false("T-14-1-2" %in% names(d$planner$spec$outputs))
  expect_false("T-14-1-2" %in% d$planner$outputs$output_id)
})

test_that("a three-way merge keeps the spec's own keys and takes the edits", {
  spec <- list(a = 1, b = list(x = 1, y = 2), only = "s")
  was <- list(a = 1, b = list(x = 1))
  now <- list(b = list(x = 3), c = 4)
  # a removed, b$x changed, c added, b$y and only kept
  expect_identical(.merge3(spec, was, now), list(b = list(x = 3, y = 2), c = 4, only = "s"))
  # unchanged by the edits: the spec's
  expect_identical(.merge3(spec, was, was), spec)
  # a sequence changed: the edits' whole
  expect_identical(.merge3(list(1, 2), list(1), list(3)), list(3))
})

test_that("a report written into the spec has its view made again", {
  s <- model_sample()
  id <- "T-14-1-1"
  o <- s$planner$spec$outputs[[id]]
  p <- .spec_set_output(s$planner, id, o)
  # the same report: the same view
  expect_identical(.planner_body(p)$sheets, .planner_body(s$planner)$sheets)
  expect_identical(p$outputs, s$planner$outputs)
  # its population changed in the spec: the report list says it
  o$population <- "SAF"
  p <- .spec_set_output(s$planner, id, o)
  expect_identical(.spec_output(p, id)$population, "SAF")
  expect_true(.same_planner(p$spec$base, p))
  expect_error(.spec_set_output(.planner_body(s$planner), id, o), "no YAML spec")
})

test_that("a planner with no spec is written whole, and gets one", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  s$planner <- .planner_body(s$planner)
  m0 <- spec_md5(s$path)
  s <- suppressMessages(save_study(s, home = home))
  expect_false(is.null(s$planner$spec))
  expect_setequal(names(s$planner$spec$outputs), s$planner$outputs$output_id)
  # the sample's files are what the bridge writes of it
  expect_identical(changed_files(m0, spec_md5(s$path)), character())
})

test_that("a report's items say where each argument comes from (#417)", {
  s <- model_sample()
  p <- s$planner
  x <- .spec_items(p, "T-14-1-1")
  fns <- vapply(x$items, function(it) it$fn, "")
  expect_true(all(c("table_plan", "normalize_ard", "plan_cells") %in% fns))
  expect_identical(vapply(x$own, function(it) it$fn, "")[1L], "table_plan")
  # a named-dots verb key by key; any other verb the layer's whole
  pc <- x$items[[match("plan_cells", fns)]]
  expect_identical(pc$arg_from[["default"]], "defaults")
  expect_identical(pc$arg_from[["continuous"]], "own")
  expect_identical(pc$below$fn, "plan_cells")
  st <- x$items[[match("plan_stub", fns)]]
  expect_true(all(st$arg_from == "defaults"))
  tp <- x$items[[match("table_plan", fns)]]
  expect_true(all(tp$arg_from == "own"))
  expect_null(tp$below)
  # T-14-3-1's normalization is its own
  y <- .spec_items(p, "T-14-3-1")
  na <- Filter(function(it) identical(it$fn, "normalize_ard"), y$items)[[1L]]
  expect_identical(na$from, "own")
  expect_true("hierarchy" %in% names(na$arg_from))
  # no spec, no such report, or edits of the sheets not in it yet: NULL
  expect_null(.spec_items(.planner_body(p), "T-14-1-1"))
  expect_null(.spec_items(p, "T-99"))
  d <- sheet_rows(p, "tables", "T-14-1-1")
  d$output_id <- NULL
  d$note <- "an edit"
  expect_null(.spec_items(set_sheet_rows(p, "tables", "T-14-1-1", d), "T-14-1-1"))
})

test_that("a study with no report keeps its spec a tfl_study_spec (#413)", {
  home <- withr_tempdir()
  old <- options(tflplanner.home = home)
  on.exit(options(old), add = TRUE)
  suppressMessages(setup_tflplanner(studies_root = file.path(home, "studies")))
  s <- suppressMessages(create_study("Z-01", home = home))
  expect_identical(nrow(s$planner$outputs), 0L)
  expect_s3_class(s$planner$spec$study, "tfl_study_spec")
  expect_identical(.study_format(s$path), "yaml")
  # reopened and saved again: still the spec, in step
  s <- suppressMessages(open_study("Z-01", home = home))
  expect_s3_class(s$planner$spec$study, "tfl_study_spec")
  expect_length(s$planner$spec$outputs, 0L)
  s <- suppressMessages(save_study(s, home = home))
  expect_s3_class(s$planner$spec$study, "tfl_study_spec")
  expect_true(.same_planner(s$planner$spec$base, s$planner))
  # its programs are written from the spec
  expect_false(is.null(.code_spec(s$planner)))
})

test_that("a card's edit writes that report's YAML only, its content only; Revert takes it back (#417)", {
  s <- model_sample()
  home <- getOption("tflplanner.home")
  m0 <- spec_md5(s$path)
  id <- "T-14-1-1"
  rel <- "spec/outputs/T-14-1-1.yml"
  y0 <- s$planner$spec$text[[rel]]
  items <- builder_items(s$planner, id)
  it <- Filter(function(x) identical(x$fn, "plan_stub"), items)[[1L]]
  expect_identical(it$from, "defaults")
  # a card of the study's defaults changed: the item whole the report's own (Q1)
  p <- .builder_item_write(s$planner, id, "plan_stub", utils::modifyList(it$args, list(indent = 3L)))
  st <- .spec_output(p, id)
  own <- Filter(function(x) identical(x$fn, "plan_stub"), st$content)[[1L]]
  expect_identical(own$args, list(name = "row_label", before = TRUE, indent = 3L))
  # only the report's file changes, and in it only lines of the content
  expect_identical(changed_files(s$planner$spec$text, p$spec$text), rel)
  y1 <- p$spec$text[[rel]]
  at <- grep("^content:", y1)
  expect_identical(utils::head(y1, at), utils::head(y0, at))
  # the spec and the view in step: the program is written from the YAML
  expect_false(is.null(.code_spec(p)))
  # a key-by-key verb: only the key that differs is the report's
  pc <- Filter(function(x) identical(x$fn, "plan_cells"), builder_items(p, id))[[1L]]
  p2 <- .builder_item_write(p, id, "plan_cells", utils::modifyList(pc$args, list(default = "{n}")))
  own <- Filter(function(x) identical(x$fn, "plan_cells"), .spec_output(p2, id)$content)[[1L]]
  expect_identical(own$args$default, "{n}")
  # what the sheets never held is kept: plan_filter(by =), plan_columns(cell_format =)
  p3 <- .builder_item_write(p2, id, "plan_filter", list(where = tflspec::tfl_r("n > 0"), by = "TRT01A"))
  cf <- Filter(function(x) identical(x$fn, "plan_columns"), builder_items(p3, id))[[1L]]
  p3 <- .builder_item_write(p3, id, "plan_columns",
                            c(cf$args, list(cell_format = tflspec::tfl_r("list(row_label = \"x\")"))))
  s$planner <- p3
  s <- suppressMessages(save_study(s, home = home))
  expect_identical(changed_files(m0, spec_md5(s$path)), rel)
  s2 <- suppressMessages(open_study("SAMPLE-01", home = home))
  i2 <- builder_items(s2$planner, id)
  get <- function(fn) Filter(function(x) identical(x$fn, fn), i2)[[1L]]
  expect_identical(get("plan_filter")$args$by, "TRT01A")
  expect_s3_class(get("plan_columns")$args$cell_format, "tfl_r")
  expect_identical(get("plan_stub")$args$indent, 3L)
  # Revert: the report's own item gone, the study's defaults again
  q <- .builder_item_revert(s2$planner, id, "plan_stub")
  expect_identical(Filter(function(x) identical(x$fn, "plan_stub"), builder_items(q, id))[[1L]]$from,
                   "defaults")
  expect_false("plan_stub" %in% vapply(.spec_output(q, id)$content, function(x) x$fn, ""))
})

test_that("the builder's form reads and writes the report's items: roles, labels, levels, nests (#417)", {
  s <- model_sample()
  p <- s$planner
  id <- "T-14-1-1"
  rel <- "spec/outputs/T-14-1-1.yml"
  st <- builder_read(p, id)
  expect_identical(st$key, "TRT01A")
  expect_identical(st$variables$variable, c("AGE", "AGEGR1", "SEX", "RACE", "ETHNIC"))
  expect_identical(st$variables$label[1L], "Age (years)")
  # unchanged: nothing written
  expect_identical(builder_write(p, id, st, was = st)$spec$text, p$spec$text)
  own_of <- function(q, fn) Filter(function(x) identical(x$fn, fn), .spec_output(q, id)$content)[[1L]]
  # a heading: plan_labels() only, in that report's file only
  st1 <- st
  st1$variables$label[1L] <- "Age, years"
  q <- builder_write(p, id, st1, was = st)
  expect_identical(changed_files(p$spec$text, q$spec$text), rel)
  y0 <- p$spec$text[[rel]]
  y1 <- q$spec$text[[rel]]
  expect_identical(setdiff(y1, y0), "      AGE: Age, years")
  expect_identical(own_of(q, "plan_labels")$args$AGE, "Age, years")
  # (written into the spec, not the sheets: the program from the YAML)
  expect_false(is.null(.spec_items(q, id)))
  # the variables moved and an arm's order: plan_levels(variable = , TRT01A = ),
  # the headings in the new order
  st2 <- st
  st2$variables <- st$variables[c(3, 1, 2, 4, 5), ]
  rownames(st2$variables) <- NULL
  st2$arms$TRT01A <- rev(st$arms$TRT01A)
  q <- builder_write(p, id, st2, was = st)
  lv <- own_of(q, "plan_levels")$args
  expect_identical(lv$variable, c("SEX", "AGE", "AGEGR1", "RACE", "ETHNIC"))
  expect_identical(lv$TRT01A, c("Control", "Drug X"))
  expect_identical(names(own_of(q, "plan_labels")$args)[1L], "SEX")
  expect_identical(builder_read(q, id)$variables$variable, st2$variables$variable)
  # the rows' layers: table_plan(rows = )
  st3 <- st
  st3$layers <- c("SEX", ".variable")
  q <- builder_write(p, id, st3, was = st)
  expect_identical(own_of(q, "table_plan")$args$rows, list(group1 = "SEX", group = "variable"))
  # a variable under another's level: plan_nest(), out of the variables' order
  p13 <- "T-14-1-3"
  s13 <- builder_read(p, p13)
  s13b <- s13
  s13b$variables$under[s13b$variables$variable == "SEX"] <- "AGEGR1: <65 years"
  q <- builder_write(p, p13, s13b, was = s13)
  o13 <- .spec_output(q, p13)$content
  ne <- Filter(function(x) identical(x$fn, "plan_nest"), o13)[[1L]]
  expect_identical(ne$args, list(SEX = list(AGEGR1 = "<65 years")))
  lv13 <- Filter(function(x) identical(x$fn, "plan_levels"), o13)[[1L]]$args
  expect_identical(lv13$variable, "AGEGR1")
  expect_identical(changed_files(p$spec$text, q$spec$text), "spec/outputs/T-14-1-3.yml")
  # ... and read back so: under its parent, after it
  r13 <- builder_read(q, p13)
  expect_identical(r13$variables$under[r13$variables$variable == "SEX"], "AGEGR1: <65 years")
  # a hierarchy's rows: table_plan(rows = , label = )
  m <- list(hierarchy = c("AEBODSYS", "AEDECOD"), by = "TRT01A",
            keys = list(TRT01A = c("Drug X", "Control"), AEBODSYS = character(), AEDECOD = character()))
  h <- builder_read(p, "T-14-3-1", m)
  h2 <- h
  h2$row_keys <- rev(h$row_keys)
  q <- builder_write(p, "T-14-3-1", h2, was = h)
  tp <- Filter(function(x) identical(x$fn, "table_plan"), .spec_output(q, "T-14-3-1")$content)[[1L]]
  expect_identical(tp$args$rows, list(group1 = "AEDECOD"))
  expect_identical(tp$args$label, list(label = "AEBODSYS"))
  # the variables shown: plan_filter(where = ); all shown again, no filter
  pf <- .builder_args(builder_items(p, id), "plan_filter") %||% list()
  pf$where <- tflspec::tfl_r(.hidden_vars_write("ETHNIC"))
  q <- .spec_set_items(p, id, list(list(fn = "plan_filter", args = pf, drop_empty = TRUE)))
  w <- .builder_args(builder_items(q, id), "plan_filter")$where
  expect_identical(.hidden_vars_read(.filter_where_text(w)), "ETHNIC")
  expect_identical(changed_files(p$spec$text, q$spec$text), rel)
  q2 <- .spec_set_items(q, id, list(list(fn = "plan_filter", args = list(), drop_empty = TRUE)))
  expect_null(.builder_args(builder_items(q2, id), "plan_filter"))
  expect_identical(q2$spec$text, p$spec$text)
  # empty values hidden: plan_levels(.drop_empty = )
  lv <- .builder_args(builder_items(p, id), "plan_levels")
  lv$.drop_empty <- "RACE"
  q <- .spec_set_items(p, id, list(list(fn = "plan_levels", args = lv, drop_empty = TRUE)))
  expect_identical(builder_read(q, id)$variables$variable, st$variables$variable)
  expect_identical(.builder_vars_rows(builder_items(q, id))$empty_levels[
    .builder_vars_rows(builder_items(q, id))$variable == "RACE"], "hide")
})

test_that("the builder's rows and the items' arguments: one way and back (#417)", {
  expect_identical(.role_text(c(group1 = "AEBODSYS", group = "variable")),
                   "group1 = AEBODSYS | group = variable")
  expect_identical(.role_text(NA), "NA")
  expect_identical(.role_text(NULL), NA_character_)
  expect_identical(.role_text(list(group1 = ~"Overall", group = "variable")),
                   "group1 = \"Overall\" | group = variable")
  expect_identical(.role_value("group1 = AEBODSYS | group = variable"),
                   c(group1 = "AEBODSYS", group = "variable"))
  expect_true(is.na(.role_value("NA")))
  expect_null(.role_value(NA_character_))
  v <- .role_value("group1 = \"Overall\" | group = variable")
  expect_true(inherits(v$group1, "formula"))
  expect_identical(.role_text(v), "group1 = \"Overall\" | group = variable")
  # the variables rows of items: a nested one after its parent
  its <- list(
    list(fn = "plan_levels", args = list(variable = c("AGE", "RACE", "SEX"), RACE = c("White", "Asian"),
                                         .drop_empty = "SEX")),
    list(fn = "plan_labels", args = list(AGE = "Age", subgroup = c(ALL = "All"))),
    list(fn = "plan_nest", args = list(ASIANSUB = list(RACE = "Asian"))))
  vr <- .builder_vars_rows(its)
  expect_identical(vr$variable, c("AGE", "RACE", "SEX", "ASIANSUB"))
  expect_identical(vr$order, c("1", "2", "4", "3"))
  expect_identical(vr$label, c("Age", NA, NA, NA))
  expect_identical(vr$empty_levels, c(NA, NA, "hide", NA))
  expect_identical(vr$under, c(NA, NA, NA, "RACE: Asian"))
  # nothing edited: nothing to write
  tb <- data.frame(cols = "TRT01A", rows = "group = variable", label = NA_character_,
                   keep_where = NA_character_)
  its$tp <- list(fn = "table_plan", args = list(cols = "TRT01A", rows = list(group = "variable")))
  expect_length(.builder_items_from_rows(its, tb, vr, vr), 0L)
})

test_that("the builder's cells and decimals are plan_cells(); the rounding the study's defaults.yml (#417)", {
  s <- model_sample()
  p <- s$planner
  id <- "T-14-1-1"
  rel <- "spec/outputs/T-14-1-1.yml"
  st <- builder_read(p, id)
  # the decimals the templates state are the form's
  expect_identical(st$digits[c("mean", "sd")], c(mean = 1L, sd = 2L))
  cells <- function(q) .builder_args(builder_items(q, id), "plan_cells")
  # a statistic's decimals: in the templates of the rows
  st1 <- st
  st1$digits[["mean"]] <- 2L
  q <- builder_write(p, id, st1, was = st)
  expect_identical(changed_files(p$spec$text, q$spec$text), rel)
  expect_identical(cells(q)$continuous[["Mean (SD)"]], "{mean:.2f} ({sd:.2f})")
  expect_false(is.null(.spec_items(q, id)))
  expect_identical(builder_read(q, id)$digits[["mean"]], 2L)
  # a variable's own decimals: its rows again, read back as its own
  st2 <- st
  st2$exceptions <- data.frame(variable = "AGE", statistic = "mean", digits = 3L)
  q <- builder_write(p, id, st2, was = st)
  expect_identical(cells(q)$AGE[["Mean (SD)"]], "{mean:.3f} ({sd:.2f})")
  expect_identical(builder_read(q, id)$exceptions,
                   data.frame(variable = "AGE", statistic = "mean", digits = 3L))
  # ... and taken away again
  q2 <- builder_write(q, id, st, was = builder_read(q, id))
  expect_null(cells(q2)$AGE)
  # the categorical format; the value printed
  st3 <- st
  st3$cat_format <- "nNpct"
  q <- builder_write(p, id, st3, was = st)
  expect_identical(cells(q)$categorical, "{n:.0f}/{N:.0f} ({p:.1f%})")
  expect_identical(names(cells(q))[1:3], c("default", "categorical", "continuous"))
  st4 <- st
  st4$value <- "stat_fmt"
  q <- builder_write(p, id, st4, was = st)
  expect_identical(cells(q)$value, "stat_fmt")
  # the rounding: the study's defaults.yml only, every report's
  q <- .spec_set_rounding(p, "r")
  expect_identical(changed_files(p$spec$text, q$spec$text), "spec/defaults.yml")
  expect_identical(unname(q$study[["rounding"]]), "r")
  expect_false(is.null(.spec_items(q, id)))
  expect_true(any(grepl("plan_digits(rounding = \"r\")", program_code(q, "T-14-3-1"), fixed = TRUE)))
  expect_identical(.spec_set_rounding(q, "sas")$spec$text, p$spec$text)
  # the card's write of it: the same
  pd <- .builder_args(builder_items(p, id), "plan_digits")
  q <- .builder_item_write(p, id, "plan_digits", utils::modifyList(pd, list(rounding = "r")))
  expect_identical(changed_files(p$spec$text, q$spec$text), "spec/defaults.yml")
  expect_match(as.character(.rounding_field("n", "sas")), "Applies to every report", fixed = TRUE)
})

test_that("the builder's header, order and pages are plan_col_header(), plan_sort(), plan_paginate_group() (#417)", {
  s <- model_sample()
  p <- s$planner
  id <- "T-14-3-1"
  rel <- "spec/outputs/T-14-3-1.yml"
  m <- list(hierarchy = c("AEBODSYS", "AEDECOD"), by = "TRT01A",
            keys = list(TRT01A = c("Drug X", "Control"), AEBODSYS = character(), AEDECOD = character()))
  arg <- function(q, fn) .builder_args(builder_items(q, id), fn)
  st <- builder_read(p, id, m)
  expect_null(st$overall_label)
  # the header's rows and back, the same item
  h <- arg(p, "plan_col_header")$header
  expect_identical(tflspec::tfl_args_to_r("plan_col_header", list(header = .header_list(.header_df(h)))),
                   tflspec::tfl_args_to_r("plan_col_header", list(header = h)))
  # a header line's text: plan_col_header(header = ) of that report only
  st1 <- st
  st1$header <- .header_df(h)
  st1$header$text[st1$header$line == "2" & st1$header$cols == "row_label"] <- "SOC"
  q <- builder_write(p, id, st1, was = st)
  expect_identical(changed_files(p$spec$text, q$spec$text), rel)
  expect_identical(arg(q, "plan_col_header")$header[[3L]]$text, "SOC")
  expect_false(is.null(.spec_items(q, id)))
  # whose {n}: plan_col_header(values = )
  st2 <- st
  st2$header_n <- "page"
  q <- builder_write(p, id, st2, was = st)
  expect_identical(arg(q, "plan_col_header")$values, list(n = "page"))
  # the order of each level: plan_sort(keys = )
  st3 <- st
  st3$sort[] <- c("freq", "freq")
  q <- builder_write(p, id, st3, was = st)
  expect_identical(as.vector(builder_read(q, id, m)$sort), c("freq", "freq"))
  expect_true("-n@group1" %in% arg(q, "plan_sort")$keys)
  # the outermost a page each: plan_paginate_group(col = "group1"); off: gone
  st4 <- st
  st4$row_pages <- TRUE
  q <- builder_write(p, id, st4, was = st)
  expect_identical(arg(q, "plan_paginate_group")$col, "group1")
  expect_true(builder_read(q, id, m)$row_pages)
  q2 <- builder_write(q, id, st, was = builder_read(q, id, m))
  expect_null(arg(q2, "plan_paginate_group"))
  expect_identical(q2$spec$text, p$spec$text)
})
