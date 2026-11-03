# Step 3's ai: block (confirm first, then assumptions), step 4's reason,
# and the data.yml helpers of step 1 / the ARD preview error (#513, #511, #515)

test_that("the confirm items come first, in a marked block, the assumptions after", {
  cf <- data.frame(output_id = "t1", where = "content", item = "plan_cells",
                   why = "is the % of the arm?", stringsAsFactors = FALSE)
  asm <- list(list(where = "content", item = "widths", assumption = "default widths"))
  h <- as.character(.ai_notes_block(asm, cf, "t1", TRUE, "ns-", identity))
  expect_match(h, "ai-confirm-block")
  expect_match(h, "border-warning")
  expect_true(regexpr("ai-confirm-block", h) < regexpr("ai-assumption-block", h))
  expect_true(regexpr("To confirm", h) < regexpr("What the agent assumed", h))
  expect_match(h, "ai-open")
  h2 <- as.character(.ai_notes_block(asm, cf, "t1", FALSE, "ns-", identity))
  expect_no_match(h2, "ai-open")
  expect_match(h2, "Copy it into the study first")
})

test_that("no confirm items: only the assumptions, no empty block", {
  asm <- list(list(where = "content", item = "widths", assumption = "default widths"))
  h <- as.character(.ai_notes_block(asm, NULL, "t1", TRUE, "ns-", identity))
  expect_no_match(h, "ai-confirm-block")
  expect_match(h, "ai-assumption-block")
  h0 <- as.character(.ai_notes_block(NULL, cf <- data.frame(), "t1", TRUE, "ns-", identity))
  expect_match(h0, "Nothing assumed")
  expect_no_match(h0, "ai-confirm-block")
})

test_that("a warning or an error shows its reason under the status; an old manifest shows none", {
  d <- file.path(tempfile("man"), "ai")
  dir.create(d, recursive = TRUE)
  study <- dirname(d)
  on.exit(unlink(study, recursive = TRUE))
  utils::write.csv(data.frame(request = "r1", toc = "1", output_id = c("a", "b", "c"),
                              title = "", split = "", listed = "", status = c("ok", "warning", "error"),
                              reason = c(NA, "no population <x> / no title", "SPEC does not read"),
                              stringsAsFactors = FALSE),
                   file.path(d, "manifest.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
  why <- .ai_manifest_reasons(study)
  expect_equal(unname(why), c("no population <x> / no title", "SPEC does not read"))
  cells <- .ai_status_cells(c("OK", "WARNING", "ERROR"), c("ok", "warning", "error"),
                            c("a", "b", "c"), why)
  expect_equal(cells[1], "OK")
  expect_match(cells[2], "WARNING<div class=\"small text-muted ai-reason\">no population &lt;x&gt; / no title</div>",
               fixed = TRUE)
  expect_match(cells[3], "SPEC does not read", fixed = TRUE)
  # an old manifest: no `reason` column
  utils::write.csv(data.frame(request = "r1", output_id = c("a", "b"), status = c("ok", "warning")),
                   file.path(d, "manifest.csv"), row.names = FALSE)
  expect_null(.ai_manifest_reasons(study))
  expect_equal(.ai_status_cells(c("OK", "WARNING"), c("ok", "warning"), c("a", "b"), NULL),
               c("OK", "WARNING"))
  expect_null(.ai_manifest_reasons(NULL))
  expect_null(.ai_manifest_reasons(tempfile()))
})

test_that("data_paths: the rows that need a look, and data.yml pointed at the files given", {
  dp <- data.frame(dataset = c("ADSL", "ADAE", "ADLB"),
                   in_data_yml = c("data/adam/adsl.rds", "data/adam/adae.sas7bdat", "data/adam/adlb.rds"),
                   given = c("C:/x/study/data/adam/adsl.sas7bdat", "C:/x/study/data/adam/adae.sas7bdat", NA),
                   exists = c(FALSE, TRUE, FALSE), differs = c(TRUE, FALSE, FALSE),
                   stringsAsFactors = FALSE)
  pr <- .data_paths_problems(dp)
  expect_equal(pr$dataset, c("ADSL", "ADLB"))
  expect_equal(nrow(.data_paths_problems(dp[2, ])), 0L)
  expect_equal(nrow(.data_paths_problems(NULL)), 0L)
  pl <- list(ard = list(datasets = data.frame(
    dataset = c("ADSL", "ADAE", "ADLB"),
    path = c("data/adam/adsl.rds", "data/adam/adae.sas7bdat", "data/adam/adlb.rds"),
    stringsAsFactors = FALSE)))
  x <- .data_paths_apply(pl, dp, "C:/x/study")
  expect_equal(x$planner$ard$datasets$path,
               c("data/adam/adsl.sas7bdat", "data/adam/adae.sas7bdat", "data/adam/adlb.rds"))
  expect_equal(x$changed$dataset, "ADSL")
  # a file given outside the study stays absolute
  dp$given[1] <- "D:/delivery/adsl.xpt"
  x <- .data_paths_apply(pl, dp, "C:/x/study")
  expect_equal(x$planner$ard$datasets$path[1], "D:/delivery/adsl.xpt")
  # nothing to change
  expect_equal(nrow(.data_paths_apply(pl, dp[2, ], "C:/x/study")$changed), 0L)
})

test_that("a file-open error names the datasets of data.yml whose file is missing", {
  st <- tempfile("study")
  dir.create(file.path(st, "data", "adam"), recursive = TRUE)
  on.exit(unlink(st, recursive = TRUE))
  file.create(file.path(st, "data", "adam", "adae.rds"))
  study <- list(path = st, planner = list(ard = list(datasets = data.frame(
    dataset = c("ADSL", "ADAE"), path = c("data/adam/adsl.rds", "data/adam/adae.rds"),
    stringsAsFactors = FALSE))))
  m <- .data_files_note("cannot open the connection", study)
  expect_equal(m, "cannot open the connection. ADSL: no file at data/adam/adsl.rds (spec/data.yml).")
  expect_match(.data_files_note("Error in file(con, \"rb\") : cannot open file 'x'.", study),
               "ADSL: no file at data/adam/adsl.rds (spec/data.yml).", fixed = TRUE)
  # another error: as it is
  expect_equal(.data_files_note("object 'x' not found", study), "object 'x' not found")
  expect_true(is.na(.data_files_note(NA_character_, study)))
  # all paths there: as it is
  file.create(file.path(st, "data", "adam", "adsl.rds"))
  expect_equal(.data_files_note("cannot open the connection", study), "cannot open the connection")
})
