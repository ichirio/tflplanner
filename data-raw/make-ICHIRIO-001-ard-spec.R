# EXPERIMENTAL: ICHIRIO-001's ARD from an ard_spec.xlsx.
#
# Writes spec/ard_spec.xlsx for the three tables of the test study (made by
# make-ICHIRIO-001.R), the code it stands for (programs/make_ard.R), and the
# study ARD (output/ard/ard.rds); then checks that each table planned from
# its part of that ARD is the table its own data code gives.

devtools::load_all(quiet = TRUE)
library(rtfreporter)
s <- open_study("ICHIRIO-001")
root <- s$path

tbl <- function(...) {
  rows <- list(...)
  cols <- unique(unlist(lapply(rows, names)))
  as.data.frame(lapply(stats::setNames(cols, cols), function(cn)
    vapply(rows, function(r) as.character(r[[cn]] %||% NA), "")),
    stringsAsFactors = FALSE)
}

spec <- list(
  study = tbl(list(key = "id", value = "USUBJID"),
              list(key = "output", value = "output/ard/ard.rds")),
  datasets = tbl(
    list(dataset = "ADSL", path = "data/adam/adsl.rds"),
    list(dataset = "ADAE", path = "data/adam/adae.rds")),
  populations = tbl(
    list(population_id = "SAF", dataset = "ADSL", where = "SAFFL == \"Y\"",
         derive = "TRTA = TRT01A",
         note = "Safety set; TRTA so that AE counts share its arms")),
  analyses = tbl(
    # Demographics
    list(output_id = "T-14-1-1", analysis_id = "BIGN", label = "Subjects per arm",
         method = "categorical", population_id = "SAF", variables = "TRT01A"),
    list(output_id = "T-14-1-1", analysis_id = "TOTAL", label = "Subjects",
         method = "total_n", population_id = "SAF"),
    list(output_id = "T-14-1-1", analysis_id = "CONT", label = "Continuous",
         method = "continuous", population_id = "SAF", by = "TRT01A",
         variables = "AGE | WEIGHTBL | HEIGHTBL",
         statistics = "N | mean | sd | median | min | max"),
    list(output_id = "T-14-1-1", analysis_id = "CAT", label = "Categorical",
         method = "categorical", population_id = "SAF", by = "TRT01A",
         variables = "AGEGR1 | SEX | RACE", statistics = "n | p"),
    # Disposition
    list(output_id = "T-14-1-2", analysis_id = "BIGN", method = "categorical",
         population_id = "SAF", variables = "TRT01A"),
    list(output_id = "T-14-1-2", analysis_id = "TOTAL", method = "total_n",
         population_id = "SAF"),
    list(output_id = "T-14-1-2", analysis_id = "DISP",
         label = "Status at end of study", method = "categorical",
         population_id = "SAF", by = "TRT01A", variables = "DCDECOD",
         statistics = "n | p"),
    # TEAE by SOC / PT
    list(output_id = "T-14-3-1", analysis_id = "TEAE",
         label = "TEAE by SOC / PT", method = "hierarchical",
         dataset = "ADAE", population_id = "SAF", where = "TRTEMFL == \"Y\"",
         by = "TRTA", variables = "AEBODSYS | AEDECOD",
         args = "over_variables = TRUE")))

readme <- tbl(
  list(sheet = "study", column = "key / value",
       en = "id: the subject key (USUBJID); output: where the study ARD goes",
       ja = "id：被験者キー（USUBJID）、output：試験 ARD の保存先"),
  list(sheet = "datasets", column = "dataset / path",
       en = "a name for the data, and its file (relative to the study folder: .rds .xpt .sas7bdat .csv .parquet)",
       ja = "データの名前と、そのファイル（試験フォルダからの相対パス）"),
  list(sheet = "datasets", column = "derive",
       en = "new columns, NAME = R expression, | between them",
       ja = "追加する列。NAME = R の式、複数は | 区切り"),
  list(sheet = "populations", column = "population_id / dataset / where",
       en = "an analysis set: the subjects of `dataset` for which `where` (an R condition) holds",
       ja = "解析対象集団：dataset のうち where（R の条件式）を満たす被験者"),
  list(sheet = "populations", column = "derive",
       en = "columns added to the population (e.g. TRTA = TRT01A, so that AE counts and denominators share their arms)",
       ja = "集団に追加する列（例：TRTA = TRT01A。AE の集計と分母で群の変数名を揃える）"),
  list(sheet = "analyses", column = "output_id / analysis_id",
       en = "the output the analysis serves, and its id within it; both are columns of the study ARD",
       ja = "解析が属する帳票と、その中での ID。どちらも試験 ARD の列になる"),
  list(sheet = "analyses", column = "method",
       en = "continuous, categorical, dichotomous, hierarchical, total_n, proportion_ci, custom (sheet _methods)",
       ja = "手法（_methods シート参照）"),
  list(sheet = "analyses", column = "dataset / population_id / where",
       en = "the analysis data: `dataset` restricted to the population's subjects and to `where`; blank dataset = the population itself",
       ja = "解析データ：dataset を集団の被験者と where で絞ったもの。dataset 空欄 = 集団そのもの"),
  list(sheet = "analyses", column = "by / variables / statistics",
       en = "grouping variables, analysis variables, statistics; | between several; blank statistics = the method's default",
       ja = "グループ変数・解析変数・統計量（複数は |）。統計量空欄 = 手法の既定"),
  list(sheet = "analyses", column = "args / code",
       en = "further arguments of the call, as R (e.g. over_variables = TRUE); for custom, the whole call in `code` (data = the analysis data)",
       ja = "関数への追加引数（R の書き方）。custom は code に呼び出し全体（data = 解析データ）"))
f <- file.path(root, "spec", "ard_spec.xlsx")
writexl::write_xlsx(c(list(`_README` = readme), spec,
                      list(`_methods` = ard_methods())), f)
x <- read_ard_spec(f)
writeLines(ard_spec_code(x), file.path(root, "programs", "make_ard.R"))
ard <- build_ard(x, dir = root)
cat("study ARD:", nrow(ard), "rows;",
    paste(names(table(ard$output_id)), table(ard$output_id), collapse = ", "),
    "\n")

# the same tables?  Plan each from its part of the study ARD and from its
# own data code, and compare the pages.
same <- vapply(c("T-14-1-1", "T-14-1-2", "T-14-3-1"), function(id) {
  mine <- ard_data(s, id)                          # from the data code
  proc <- s$planner$outputs$process_code[s$planner$outputs$output_id == id]
  e <- new.env()
  e$ard <- ard_for(ard, id)
  eval(parse(text = if (is.na(proc)) "data <- ard_normalize(ard)" else proc),
       envir = e)
  a <- preview_pages(s$planner, id, mine)
  b <- preview_pages(s$planner, id, e$data)
  identical(lapply(a, `[[`, "data"), lapply(b, `[[`, "data")) &&
    identical(lapply(a, `[[`, "col_header"), lapply(b, `[[`, "col_header"))
}, NA)
print(same)
