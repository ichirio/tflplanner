# Extracted from test-spec-import.R:84

# prequel ----------------------------------------------------------------------
imp_study <- function(id = "IM-1") {
  p <- add_output(new_planner(), "T-1", type = "table", description = "one")
  p <- add_output(p, "T-2", type = "table", description = "two")
  p <- set_sheet_rows(p, "titles", "T-1", data.frame(line = "1", center = "Old title"))
  p <- set_sheet_rows(p, "titles", "T-2", data.frame(line = "1", center = "Other"))
  suppressMessages(create_study(id, planner = p))
}
edit_book <- function(f, sheet, fn) {
  wb <- openxlsx::loadWorkbook(f)
  d <- openxlsx::read.xlsx(wb, sheet)
  d2 <- fn(d)
  openxlsx::deleteData(wb, sheet, cols = seq_len(ncol(d)), rows = seq_len(nrow(d) + 1L),
                       gridExpand = TRUE)
  openxlsx::writeData(wb, sheet, d2)
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
}

# test -------------------------------------------------------------------------
home <- local_home()
s <- imp_study()
g <- file.path(home, "tables.xlsx")
file.copy(file.path(s$path, "spec", "report_spec.xlsx"), g)
edit_book(g, "report", function(d) d[setdiff(names(d), "output_id")])
pv <- preview_spec_import("IM-1", g)
expect_false(pv$ok)
