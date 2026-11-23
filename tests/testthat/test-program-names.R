# A report's program, or an ARD program's name, that is a file tflplanner
# writes itself stops the save before anything is written (R/study_save.R
# .program_name_problems()), and the review says it as the program is typed

.pn_files <- function(root) {
  f <- list.files(root, recursive = TRUE, all.files = TRUE, full.names = TRUE)
  f[!dir.exists(f)]
}
.pn_md5 <- function(root) {
  f <- .pn_files(root)
  stats::setNames(unname(tools::md5sum(f)), sub(root, "", f, fixed = TRUE))
}
.pn_study <- function() {
  local_home(parent.frame())
  s <- create_study("PN1", planner = sample_planner())
  save_study(s)
}
.pn_program <- function(s, id, program) {
  s$planner <- set_sheet_rows(s$planner, "report", id, data.frame(program = program))
  s
}

test_that("a report program named like a generated file stops the save, nothing written", {
  skip_on_cran()
  s <- .pn_study()
  id <- s$planner$outputs$output_id[1]
  before <- .pn_md5(s$path)
  for (nm in c("report_setup.R", "Report_Setup.R", "fig_setup.R", "autoexec_report.R",
               "study_setup.R", "batch.R", "autoexec_all.R", "study_helpers.R",
               "ard_setup.R", "autoexec_ard.R")) {
    expect_error(save_study(.pn_program(s, id, nm)), "writes itself", info = nm)
    expect_identical(.pn_md5(s$path), before, info = nm)
  }
  # the setup stays where it is
  expect_true(file.exists(file.path(s$path, "programs", "study_setup.R")))
  expect_false(file.exists(file.path(s$path, "programs", "tfl", "study_setup.R")))
})

test_that("two report programs that differ only in case stop the save", {
  skip_on_cran()
  s <- .pn_study()
  ids <- s$planner$outputs$output_id
  before <- .pn_md5(s$path)
  s2 <- .pn_program(.pn_program(s, ids[1], "T1.R"), ids[2], "t1.R")
  expect_error(save_study(s2), "share the program")
  expect_identical(.pn_md5(s$path), before)
})

test_that("an output id whose ARD program is a generated file's stops the save", {
  skip_on_cran()
  s <- .pn_study()
  before <- .pn_md5(s$path)
  for (id in c("ard_setup", "Autoexec_ARD")) {
    s2 <- s
    s2$planner <- add_output(s2$planner, id, type = "table")
    s2 <- .pn_program(s2, id, "own_program.R")
    expect_error(save_study(s2), "ARD program", info = id)
    expect_identical(.pn_md5(s$path), before, info = id)
  }
})

test_that("a program name of its own still saves, and the review says the others", {
  skip_on_cran()
  s <- .pn_study()
  id <- s$planner$outputs$output_id[1]
  s2 <- .pn_program(s, id, "my_own_program.R")
  expect_no_error(s3 <- save_study(s2))
  expect_true(file.exists(file.path(s3$path, "programs", "tfl", "my_own_program.R")))
  # as it is typed: the review (the app's warning) has the row
  bad <- .pn_program(s, id, "Report_Setup.R")
  r <- review_problems(bad$planner, lang = "en")
  expect_true(any(r$rule == "R07" & r$output_id %in% id & grepl("writes itself", r$message)))
  ja <- review_problems(bad$planner, lang = "ja")
  expect_true(any(ja$rule == "R07" & grepl("tflplanner", ja$message) & !grepl("writes itself", ja$message)))
  expect_false(any(review_problems(s2$planner, lang = "en")$rule == "R07"))
})

test_that(".generated_program_files() lists every file a save writes besides the programs", {
  skip_on_cran()
  s <- .pn_study()
  root <- file.path(s$path, "programs")
  f <- list.files(root, recursive = TRUE, all.files = TRUE)
  f <- f[!dir.exists(file.path(root, f))]
  f <- f[!grepl("(^|/)\\.edited/", f)]
  own <- c(vapply(s$planner$outputs$output_id, function(id)
    file.path("tfl", report_info(s$planner, id)$program), ""),
    file.path("ard", .ard_prog_name(s$planner$outputs$output_id)))
  generated <- setdiff(f, own)
  expect_true(length(generated) > 0L)
  expect_true(all(basename(generated) %in% .generated_program_files()),
              info = paste(setdiff(basename(generated), .generated_program_files()),
                           collapse = ", "))
})
