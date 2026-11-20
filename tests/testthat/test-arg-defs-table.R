# The table field of a def whose input is "table" (R/arg_defs_table.R): each
# map<K,V> and rows<map<..>> tflspec's tfl_arg_defs() gives draws its table,
# offers its choices, shows the fixed rows read only, and reads back as the
# type says; without a def the field and its reading are today's.

html <- function(...) as.character(htmltools::tagList(...))

# the state a drawn table field carries (data-state), as its input sends it
tbl_state_of <- function(h) {
  m <- regmatches(h, regexpr("data-state=\"[^\"]*\"", h))
  x <- sub("^data-state=\"(.*)\"$", "\\1", m)
  for (p in list(c("&quot;", "\""), c("&#39;", "'"), c("&lt;", "<"), c("&gt;", ">"),
                 c("&#10;", "\n"), c("&#13;", "\r"), c("&#9;", "\t"), c("&amp;", "&"))) {
    x <- gsub(p[[1L]], p[[2L]], x, fixed = TRUE)
  }
  x
}

# value -> field -> its input -> builder_card_read()
round_trip <- function(type, def, value, ctx = NULL, fixed_rows = NULL) {
  h <- html(arg_field("c", "a", type, value = value, def = def, ctx = ctx, fixed_rows = fixed_rows))
  input <- stats::setNames(list(tbl_state_of(h)), "c__a")
  builder_card_read(input, "c", schema = list(args = list(a = c(def, list(type = type)))), args = "a")$a
}

ctx <- list(vars = list(adsl = c("AGE", "SEX", "TRT01A"), adae = c("AETERM", "AESEV")),
            args = list(from = "adsl", data = "adsl", variables = c("AGE", "SEX")),
            stats = c("n", "mean", "sd"),
            params = c("subgroup", "subgroup_lvl", "where"))

test_that("a map<column,r> (derive): a new column's name = R, the columns offered in the R", {
  d <- list(input = "table", choices = "vars(from_args(from))")
  h <- html(arg_field("c", "derive", "map<column,r>", def = d, ctx = ctx,
                      value = list(AGE2 = tflspec::tfl_r("AGE * 2"))))
  expect_match(h, "class=\"rp-tbl rp-tbl-map\"", fixed = TRUE)
  expect_match(h, "id=\"c__derive\"", fixed = TRUE)
  expect_match(h, "rp-field-table", fixed = TRUE)
  expect_match(h, "<th>New column</th>", fixed = TRUE)
  expect_match(h, "<th class=\"rp-mono\">R</th>", fixed = TRUE)
  # (the key typed, the value R with the data's columns as a datalist)
  expect_match(h, "value=\"AGE2\"", fixed = TRUE)
  expect_match(h, "rp-tbl-r\" value=\"AGE * 2\"", fixed = TRUE)
  expect_match(h, "<datalist id=\"c__derive__dl2\">", fixed = TRUE)
  expect_match(h, "<option value=\"TRT01A\"></option>", fixed = TRUE)
  expect_false(grepl("AETERM", h, fixed = TRUE))
  # (add and remove; a map not ordered has no up / down)
  expect_match(h, "rp-tbl-addrow", fixed = TRUE)
  expect_match(h, "+ Add a row", fixed = TRUE)
  expect_match(h, "rp-tbl-del", fixed = TRUE)
  expect_match(h, "<template class=\"rp-tbl-tpl\">", fixed = TRUE)
  expect_false(grepl("rp-tbl-up", h, fixed = TRUE))
  # (ordered: the rows move)
  expect_match(html(arg_field("c", "derive", "map<column,r>", def = c(d, ordered = TRUE), ctx = ctx)),
               "rp-tbl-up", fixed = TRUE)
  # (no R switch of the whole value: a map is not an R expression's type, its
  # cells are -- canon 17dfbb8; a whole value that is R keeps its switch)
  expect_false(grepl("c__derive__isr", h, fixed = TRUE))
  expect_match(html(arg_field("c", "derive", "map<column,r>", def = d, ctx = ctx,
                              value = tflspec::tfl_r("list(A = quote(x))"))),
               "c__derive__isr", fixed = TRUE)
})

test_that("a map<column,list<level>> (levels): a column picked, its values a select of several", {
  d <- list(input = "table", choices = "vars(from_args(data))")
  h <- html(arg_field("c", "levels", "map<column,list<level>>", def = d, ctx = ctx,
                      value = list(SEX = c("M", "F"))))
  expect_match(h, "<th>Column</th>", fixed = TRUE)
  expect_match(h, "<th>Values</th>", fixed = TRUE)
  expect_match(h, "<select class=\"form-select form-select-sm rp-tbl-cell\">", fixed = TRUE)
  expect_match(h, "<option value=\"SEX\" selected>SEX</option>", fixed = TRUE)
  expect_match(h, "<select class=\"form-select form-select-sm rp-tbl-cell rp-tbl-multi\" multiple data-placeholder=\"Add a value\">",
               fixed = TRUE)
  expect_match(h, "<option value=\"M\" selected>M</option>\n<option value=\"F\" selected>F</option>", fixed = TRUE)
  expect_identical(jsonlite::fromJSON(tbl_state_of(h))$rows[1L, ], c("SEX", "M\nF"))
  # (free: the key typed, the columns offered)
  h <- html(arg_field("c", "levels", "map<column,list<level>>", def = c(d, free = TRUE), ctx = ctx))
  expect_false(grepl("<select class=\"form-select form-select-sm rp-tbl-cell\"", h, fixed = TRUE))
  expect_match(h, "<datalist id=\"c__levels__dl1\">", fixed = TRUE)
  # (a key the choices lack is kept)
  h <- html(arg_field("c", "levels", "map<column,list<level>>", def = d, ctx = ctx,
                      value = list(RACE = "WHITE")))
  expect_match(h, "<option value=\"RACE\" selected>RACE</option>", fixed = TRUE)
})

test_that("a map<string,number|string> (formats) and a map<column,string> (analysis_ids)", {
  h <- html(arg_field("c", "formats", "map<string,number|string>",
                      def = list(input = "table", choices = "stats"), ctx = ctx,
                      value = list(mean = 1, sd = "xx.xx")))
  expect_match(h, "<th>Key</th>", fixed = TRUE)
  expect_match(h, "<option value=\"mean\" selected>mean</option>", fixed = TRUE)
  expect_match(h, "<option value=\"n\">n</option>", fixed = TRUE)
  expect_match(h, "rp-tbl-num\" value=\"xx.xx\"", fixed = TRUE)
  h <- html(arg_field("c", "analysis_ids", "map<column,string>",
                      def = list(input = "table", choices = "from_args(variables)"), ctx = ctx))
  expect_match(h, "<option value=\"AGE\">AGE</option>", fixed = TRUE)
  expect_false(grepl("TRT01A", h, fixed = TRUE))
  expect_match(h, "<th>Value</th>", fixed = TRUE)
})

test_that("a map<string, list<column>|string> (stack): the key typed, the value a list", {
  h <- html(arg_field("c", "stack", "map<string, list<column>|string>", def = list(input = "table"),
                      value = list(subgroup = c("SEX", "AGEGR1"))))
  expect_match(h, "<th>Key</th>", fixed = TRUE)
  expect_match(h, "<th>Values</th>", fixed = TRUE)
  expect_match(h, "value=\"subgroup\"", fixed = TRUE)
  expect_match(h, "<option value=\"SEX\" selected>SEX</option>\n<option value=\"AGEGR1\" selected>AGEGR1</option>",
               fixed = TRUE)
  # (selectize's script and style come with it)
  deps <- htmltools::findDependencies(arg_field("c", "stack", "map<string, list<column>|string>",
                                                def = list(input = "table")))
  expect_true("selectize" %in% vapply(deps, function(x) x$name, ""))
  # the choices offered to the value (a key that is a name, a value of
  # columns), a value they lack kept, in its order; the key typed
  h <- html(arg_field("c", "stack", "map<string, list<column>|string>", def = list(input = "table"),
                      choices = c("ALL", "SEX", "AGEGR1", "group2"),
                      value = list(subgroup = c("AGEGR1 * SEX", "ALL"))))
  expect_false(grepl("<select class=\"form-select form-select-sm rp-tbl-cell\">", h, fixed = TRUE))
  expect_match(h, paste0("<option value=\"AGEGR1 * SEX\" selected>AGEGR1 * SEX</option>\n",
                         "<option value=\"ALL\" selected>ALL</option>\n",
                         "<option value=\"SEX\">SEX</option>"), fixed = TRUE)
  expect_match(h, "<option value=\"group2\">group2</option>", fixed = TRUE)
  # (the def's `columns`, as tfl_arg_defs() gives them: the headings in the
  # language, and `new_row` a new row's values)
  cols <- list(key = list(label_en = "Name", label_ja = "名前"),
               value = list(label_en = "Layers", label_ja = "レイヤー"))
  d <- list(input = "table", columns = cols, new_row = list(key = "subgroup"))
  h <- html(arg_field("c", "stack", "map<string, list<column>|string>", def = d))
  expect_match(h, "<th>Name</th>
<th>Layers</th>", fixed = TRUE)
  tpl <- regmatches(h, regexpr("<template class=\"rp-tbl-tpl\">.*?</template>", h))
  expect_match(tpl, "value=\"subgroup\"", fixed = TRUE)
  expect_identical(.tbl_spec("map<string, list<column>|string>", def = d)$new, c("subgroup", ""))
  hj <- html(arg_field("c", "stack", "map<string, list<column>|string>", lang = "ja", def = d))
  expect_match(hj, "<th>名前</th>
<th>レイヤー</th>", fixed = TRUE)
  # (no Japanese label: the English one through the app's texts, as it is in en)
  d2 <- d
  d2$columns$key$label_ja <- NA_character_
  expect_identical(.tbl_spec("map<string, list<column>|string>", def = d2, lang = "ja")$heads[[1L]],
                   tr("Name", "ja"))
  # (no `columns` and no `new_row`: the generic headings, a blank new row)
  sp <- .tbl_spec("map<string, list<column>|string>", def = list(input = "table"))
  expect_identical(sp$heads, c("Key", "Values"))
  expect_identical(sp$new, c("", ""))
  # (a column with a label of one side only: the other keeps its generic heading)
  sp <- .tbl_spec("map<string,string>", def = list(columns = list(value = list(label_en = "Format"))))
  expect_identical(sp$heads, c("Key", "Format"))
  # (the labels of a map whose value is R: its own label, not "R")
  sp <- .tbl_spec("map<column,r>", def = list(columns = list(key = list(label_en = "Variable"),
                                                              value = list(label_en = "Value labels"))))
  expect_identical(sp$heads, c("Variable", "Value labels"))
  # (a `new_row` naming a value too)
  sp <- .tbl_spec("map<string,string>", def = list(new_row = list(key = "a", value = "b")))
  expect_identical(sp$new, c("a", "b"))
})

test_that("a map of maps: the headings from the outer and the inner `columns`, the outer key's new row", {
  lab <- function(en, ja) list(label_en = en, label_ja = ja)
  cols <- list(key = lab("Nested variable", "入れ子の変数"),
               value = c(lab("Parent", "親"),
                         list(columns = list(key = lab("Variable", "変数"),
                                             value = lab("Value", "値")))))
  d <- list(input = "table", columns = cols, new_row = list(key = "AGE"))
  ty <- "map<column,map<column,string>>"
  sp <- .tbl_spec(ty, def = d)
  expect_identical(sp$shape, "map2")
  expect_identical(sp$heads, c("Nested variable", "Variable", "Value"))
  expect_identical(sp$new, c("AGE", "", ""))
  sp <- .tbl_spec(ty, def = d, lang = "ja")
  expect_identical(sp$heads, c("入れ子の変数", "変数", "値"))
  # (the outer value's own label is not a column here: three columns, three headings)
  expect_length(sp$heads, 3L)
  # (no `columns`: the generic headings)
  expect_identical(.tbl_spec(ty, def = list(input = "table"))$heads, c("Column", "Second column", "Value"))
  # (the inner map only partly labelled)
  sp <- .tbl_spec(ty, def = list(columns = list(key = lab("Nested variable", "x"),
                                                value = list(columns = list(value = lab("Level", "y"))))))
  expect_identical(sp$heads, c("Nested variable", "Second column", "Level"))
})

test_that("a rows<...> table's headings: the columns' labels by field name, else the field name", {
  cd <- list(vars = list(label_en = "Variables", label_ja = "変数", type = "list<column>"),
             label = list(label_en = "Label", label_ja = "ラベル", type = "string"),
             width = list(type = "number"))
  ty <- "rows<vars,label,width>"
  sp <- .tbl_spec(ty, def = list(input = "table", columns = cd))
  expect_identical(sp$cols, c("vars", "label", "width"))
  expect_identical(sp$heads, c("Variables", "Label", "width"))
  sp <- .tbl_spec(ty, def = list(input = "table", columns = cd), lang = "ja")
  expect_identical(sp$heads, c("変数", "ラベル", "width"))
  # (the label drawn as it is; a field with none keeps its name, monospace)
  h <- html(arg_field("c", "a", ty, lang = "ja", def = list(input = "table", columns = cd)))
  expect_match(h, "<th>変数</th>", fixed = TRUE)
  expect_match(h, "<th class=\"rp-mono\">width</th>", fixed = TRUE)
  # (no labels at all: the field names, as before)
  expect_identical(.tbl_spec("rows<a, b>", def = list(input = "table"))$heads, c("a", "b"))
})

test_that("a rows<...> table's new row from the def's `new_row`", {
  sp <- .tbl_spec("rows<a, b>", def = list(input = "table", new_row = list(b = "x")))
  expect_identical(sp$new, c("", "x"))
  h <- html(arg_field("c", "r", "rows<a, b>", def = list(input = "table", new_row = list(b = "x"))))
  tpl <- regmatches(h, regexpr("<template class=\"rp-tbl-tpl\">.*?</template>", h))
  expect_match(tpl, "value=\"x\"", fixed = TRUE)
  expect_identical(.tbl_spec("rows<a, b>", def = list(input = "table"))$new, c("", ""))
})

test_that("a list cell reads back a list a row, one value kept a text (stack, combine)", {
  ty <- "map<string, list<column>|string>"
  v <- list(subgroup = c("ALL", "SEX", "AGEGR1", "AGEGR1 * SEX"), sub2 = "group2, group3")
  expect_identical(round_trip(ty, list(input = "table"), v), v)
  v <- list(worst_gr = c("ATOXGRL", "ATOXGRH"), base_gr = c("BTOXGRL", "BTOXGRH"))
  expect_identical(round_trip("map<string, list<column>>", list(input = "table"), v), v)
  # (a list read from the YAML, a list of texts: the same values)
  expect_identical(round_trip(ty, list(input = "table"), list(subgroup = list("ALL", "SEX"))),
                   list(subgroup = c("ALL", "SEX")))
  # the page's state: one value a line, a comma kept in its value
  st <- list(shape = "map", cols = list("key", "value"), kinds = list("text", "list"),
             rows = list(list("subgroup", "ALL\nAGEGR1 * SEX"), list("sub2", "group2, group3"),
                         list("", "SEX"), list("empty", "")))
  expect_identical(.tbl_read(as.character(jsonlite::toJSON(st, auto_unbox = TRUE))),
                   list(subgroup = c("ALL", "AGEGR1 * SEX"), sub2 = "group2, group3"))
})

test_that("a rows<map<string,string|r>> (conditions): the params as columns, where as R, the ALL row read only", {
  d <- list(input = "table", columns = "params")
  v <- list(list(subgroup = "SEX", subgroup_lvl = "M", where = tflspec::tfl_r("SEX == \"M\"")))
  all <- list(list(subgroup = "ALL", subgroup_lvl = "ALL", where = tflspec::tfl_r("TRUE")))
  h <- html(arg_field("c", "conditions", "rows<map<string,string|r>>", def = d,
                      ctx = c(ctx["params"], list(vars = c("SEX", "AGE"))), value = v, fixed_rows = all))
  expect_match(h, "class=\"rp-tbl rp-tbl-rows\"", fixed = TRUE)
  for (k in c("subgroup", "subgroup_lvl", "where")) {
    expect_match(h, paste0("<th class=\"rp-mono\">", k, "</th>"), fixed = TRUE)
  }
  expect_match(h, "rp-tbl-k-text", fixed = TRUE)
  expect_match(h, "rp-tbl-r\" value=\"SEX == &quot;M&quot;\"", fixed = TRUE)
  expect_match(h, "<option value=\"AGE\"></option>", fixed = TRUE)
  # (rows move)
  expect_match(h, "rp-tbl-up", fixed = TRUE)
  expect_match(h, "rp-tbl-down", fixed = TRUE)
  # (the ALL row: on top, read only, not in the state read back)
  expect_match(h, "<tbody class=\"rp-tbl-fixed\">", fixed = TRUE)
  expect_match(h, "From the template", fixed = TRUE)
  expect_lt(regexpr("rp-tbl-fixed", h), regexpr("rp-tbl-body", h))
  ro <- regmatches(h, regexpr("(?s)<tbody class=\"rp-tbl-fixed\">.*?</tbody>", h, perl = TRUE))
  expect_false(grepl("<input", ro, fixed = TRUE))
  expect_false(grepl("ALL", tbl_state_of(h), fixed = TRUE))
  # (the columns from the value when the def names none)
  h <- html(arg_field("c", "conditions", "rows<map<string,string|r>>", def = list(input = "table"),
                      value = v))
  expect_match(h, "<th class=\"rp-mono\">subgroup_lvl</th>", fixed = TRUE)
  # (a column whose value is R stays R)
  h <- html(arg_field("c", "conditions", "rows<map<string,string|r>>", def = list(input = "table"),
                      value = list(list(subgroup = tflspec::tfl_r("paste0(\"A\", 1)")))))
  expect_match(h, "rp-tbl-k-r", fixed = TRUE)
})

test_that("an optional column (\"params, analysis_id?\"): hidden until a row has it, its button adds it", {
  d <- list(input = "table", columns = "params, analysis_id?")
  v <- list(list(subgroup = "SEX", subgroup_lvl = "M", where = tflspec::tfl_r("SEX == \"M\"")))
  h <- html(arg_field("c", "conditions", "rows<map<string,string|r>>", def = d, ctx = ctx, value = v))
  # (no row has analysis_id: not a column, its button and cell template there)
  expect_false(grepl("<th class=\"rp-mono\">analysis_id</th>", h, fixed = TRUE))
  expect_match(h, "class=\"btn btn-link btn-sm rp-tbl-addcol\" data-col=\"analysis_id\" data-kind=\"text\"",
               fixed = TRUE)
  expect_match(h, "+ Add the column: analysis_id", fixed = TRUE)
  expect_match(h, "<template class=\"rp-tbl-coltpl\" data-col=\"analysis_id\">", fixed = TRUE)
  expect_false(grepl("analysis_id", tbl_state_of(h), fixed = TRUE))
  # (the browser added it: the state with the column reads back with the id)
  st <- list(shape = "rows", cols = list("subgroup", "subgroup_lvl", "where", "analysis_id"),
             kinds = list("text", "text", "r", "text"),
             rows = list(list("SEX", "M", "SEX == \"M\"", "ORR_SEX_M"), list("AGEGR1", "<65", "", "")))
  got <- .tbl_read(st)
  expect_identical(got[[1L]]$analysis_id, "ORR_SEX_M")
  expect_null(got[[2L]]$analysis_id)
  # (a row that has it: a column like the others, no button)
  v2 <- list(list(subgroup = "SEX", subgroup_lvl = "F", analysis_id = "ORR_SEX_F"))
  h <- html(arg_field("c", "conditions", "rows<map<string,string|r>>", def = d, ctx = ctx, value = v2))
  expect_match(h, "<th class=\"rp-mono\">analysis_id</th>", fixed = TRUE)
  expect_false(grepl("rp-tbl-addcol", h, fixed = TRUE))
  expect_identical(round_trip("rows<map<string,string|r>>", d, v2, ctx = ctx), v2)
  # (a list of columns, as tflspec#822 writes analysis.conditions: "params"
  # the params' columns there, where a condition, analysis_id required = FALSE)
  d3 <- list(input = "table", columns = list("params", where = list(type = "r", input = "condition"),
                                             analysis_id = list(type = "string", input = "text",
                                                                required = FALSE)))
  h <- html(arg_field("c", "conditions", "rows<map<string,string|r>>", def = d3, ctx = ctx, value = v))
  for (k in c("subgroup", "subgroup_lvl", "where")) {
    expect_match(h, paste0("<th class=\"rp-mono\">", k, "</th>"), fixed = TRUE)
  }
  expect_match(h, "rp-tbl-r\" value=\"SEX == &quot;M&quot;\"", fixed = TRUE)
  expect_match(h, "data-col=\"analysis_id\" data-kind=\"text\"", fixed = TRUE)
  expect_false(grepl("<th class=\"rp-mono\">analysis_id</th>", h, fixed = TRUE))
  expect_false(grepl("<th class=\"rp-mono\">params</th>", h, fixed = TRUE))
  # (a list without "params": its own columns only, the params not added)
  d4 <- list(input = "table", columns = list(id = list(type = "string"),
                                             note = list(type = "string", required = FALSE)))
  h <- html(arg_field("c", "x", "rows<map<string,string>>", def = d4, ctx = ctx,
                      value = list(list(id = "A"))))
  expect_match(h, "<th class=\"rp-mono\">id</th>", fixed = TRUE)
  expect_false(grepl("<th class=\"rp-mono\">subgroup</th>", h, fixed = TRUE))
  expect_match(h, "data-col=\"note\"", fixed = TRUE)
  # (optional = TRUE is read the same)
  d5 <- list(input = "table", columns = list(id = list(type = "string"),
                                             note = list(type = "string", optional = TRUE)))
  h <- html(arg_field("c", "x", "rows<map<string,string>>", def = d5, value = list(list(id = "A"))))
  expect_match(h, "data-col=\"note\"", fixed = TRUE)
  # (without optional columns: no button)
  h <- html(arg_field("c", "conditions", "rows<map<string,string|r>>", def = list(input = "table", columns = "params"),
                      ctx = ctx, value = v))
  expect_false(grepl("rp-tbl-addcol", h, fixed = TRUE))
})

test_that("a rows<f1, f2, ...> (a template's conditions, tflspec#823): its fields the columns, a data frame its rows", {
  ty <- "rows<tox_dir,param,worst,base,where>"
  d <- list(input = "table", columns = "params")
  df <- data.frame(tox_dir = c("high", "low"), param = c("ALT", "GLUC"), worst = "ATOXGR",
                   base = "BTOXGR", where = c("PARAMCD == \"ALT\"", NA), stringsAsFactors = FALSE)
  h <- html(arg_field("c", "conditions", ty, def = d, ctx = ctx, value = df))
  expect_match(h, "class=\"rp-tbl rp-tbl-rows\"", fixed = TRUE)
  for (k in c("tox_dir", "param", "worst", "base", "where")) {
    expect_match(h, paste0("<th class=\"rp-mono\">", k, "</th>"), fixed = TRUE)
  }
  # (the template's fields, not the ctx's params)
  expect_false(grepl("<th class=\"rp-mono\">subgroup</th>", h, fixed = TRUE))
  expect_match(h, "value=\"GLUC\"", fixed = TRUE)
  got <- .tbl_read(tbl_state_of(h))
  expect_length(got, 2L)
  expect_identical(got[[1L]]$param, "ALT")
  expect_s3_class(got[[1L]]$where, "tfl_r")
  expect_null(got[[2L]]$where)
  # (no columns in the def: the fields)
  h <- html(arg_field("c", "conditions", "rows<subgroup, subgroup_lvl, where>", def = list(input = "table")))
  expect_match(h, "<th class=\"rp-mono\">subgroup_lvl</th>", fixed = TRUE)
  # (a type of alternatives: its table)
  expect_identical(.tbl_type("rows<line, cols, text>|call<rtf_col_header>")$fields, c("line", "cols", "text"))
  expect_null(.tbl_type("call<rtf_col_header>"))
})

test_that("a rows<map<string,list<column>>> (ards): each column's own def, its choices to add", {
  col <- list(type = "list<column>", input = "pick", choices = "vars(from_args(data))")
  d <- list(input = "table", columns = list(by = col, variables = col))
  h <- html(arg_field("c", "ards", "rows<map<string,list<column>>>", def = d, ctx = ctx,
                      value = list(list(by = "TRT01A", variables = c("AGE", "SEX")))))
  expect_match(h, "<th class=\"rp-mono\">by</th>", fixed = TRUE)
  expect_match(h, "<th class=\"rp-mono\">variables</th>", fixed = TRUE)
  expect_match(h, "<option value=\"AGE\" selected>AGE</option>\n<option value=\"SEX\" selected>SEX</option>",
               fixed = TRUE)
  expect_match(h, "rp-tbl-multi", fixed = TRUE)
  expect_match(h, "<option value=\"TRT01A\">TRT01A</option>", fixed = TRUE)
})

test_that("the table's words in Japanese", {
  h <- html(arg_field("c", "derive", "map<column,r>", def = list(input = "table"), lang = "ja"))
  expect_match(h, "新しい列", fixed = TRUE)
  expect_match(h, "行を足す", fixed = TRUE)
  expect_match(h, "行を消す", fixed = TRUE)
  h <- html(arg_field("c", "ards", "rows<map<string,list<column>>>", lang = "ja",
                      def = list(input = "table", columns = list(by = list(type = "list<column>"))),
                      fixed_rows = list(list(by = "X"))))
  expect_match(h, "1 行に 1 つ", fixed = TRUE)
  expect_match(h, "テンプレートから", fixed = TRUE)
})

test_that("each table reads back the value it shows", {
  expect_identical(round_trip("map<column,r>", list(input = "table", choices = "vars(from_args(from))"),
                              list(AGE2 = tflspec::tfl_r("AGE * 2"), BMI = tflspec::tfl_r("WT / (HT / 100)^2")),
                              ctx),
                   list(AGE2 = tflspec::tfl_r("AGE * 2"), BMI = tflspec::tfl_r("WT / (HT / 100)^2")))
  expect_identical(round_trip("map<column,list<level>>", list(input = "table", choices = "vars(from_args(data))"),
                              list(SEX = c("M", "F"), RACE = "WHITE"), ctx),
                   list(SEX = c("M", "F"), RACE = "WHITE"))
  expect_identical(round_trip("map<string,number|string>", list(input = "table", choices = "stats"),
                              list(mean = 1, sd = "xx.xx", "AGE:n" = 0), ctx),
                   list(mean = 1, sd = "xx.xx", "AGE:n" = 0))
  expect_identical(round_trip("map<column,string>", list(input = "table", choices = "from_args(variables)"),
                              list(AGE = "age_id"), ctx),
                   list(AGE = "age_id"))
  expect_identical(round_trip("map<string, list<column>>", list(input = "table"),
                              list(grade = c("BTOXGRL", "BTOXGRH"))),
                   list(grade = c("BTOXGRL", "BTOXGRH")))
  v <- list(list(subgroup = "SEX", subgroup_lvl = "M", where = tflspec::tfl_r("SEX == \"M\"")),
            list(subgroup = "SEX", subgroup_lvl = "F", where = tflspec::tfl_r("SEX == 'F'")))
  expect_identical(round_trip("rows<map<string,string|r>>", list(input = "table", columns = "params"), v, ctx,
                              fixed_rows = list(list(subgroup = "ALL", subgroup_lvl = "ALL"))),
                   v)
  ards <- list(list(by = "TRT01A", variables = c("AGE", "SEX")), list(variables = "BMI"))
  expect_identical(round_trip("rows<map<string,list<column>>>",
                              list(input = "table",
                                   columns = list(by = list(type = "list<column>"),
                                                  variables = list(type = "list<column>"))),
                              ards, ctx),
                   ards)
  # (nothing in the table: nothing written)
  expect_null(round_trip("map<column,r>", list(input = "table"), NULL))
})

test_that("the state as the browser sends it: blank rows and cells left out, rows in their order", {
  def <- list(input = "table", type = "map<column,r>")
  rd <- function(json, d = def) builder_card_read(list(c__a = json), "c", schema = list(args = list(a = d)),
                                                  args = "a")$a
  j <- paste0("{\"shape\":\"map\",\"cols\":[\"key\",\"value\"],\"kinds\":[\"text\",\"r\"],",
              "\"rows\":[[\"B\",\" x + 1 \"],[\"\",\"y\"],[\"C\",\"\"],[\"A\",\"z\"]]}")
  expect_identical(rd(j), list(B = tflspec::tfl_r("x + 1"), A = tflspec::tfl_r("z")))
  d2 <- list(input = "table", type = "rows<map<string,string|r>>")
  j <- paste0("{\"shape\":\"rows\",\"cols\":[\"subgroup\",\"where\"],\"kinds\":[\"text\",\"r\"],",
              "\"rows\":[[\"\",\"\"],[\"\",\"AGE > 65\"],[\"SEX\",\"\"]]}")
  expect_identical(rd(j, d2), list(list(where = tflspec::tfl_r("AGE > 65")), list(subgroup = "SEX")))
  expect_null(rd("{\"shape\":\"map\",\"cols\":[\"key\",\"value\"],\"kinds\":[\"text\",\"r\"],\"rows\":[]}"))
  # (the R switch on: the whole value as R)
  input <- list(c__a = "{}", c__a__isr = TRUE, c__a__r = "list(A = quote(x))")
  expect_identical(builder_card_read(input, "c", schema = list(args = list(a = def)), args = "a")$a,
                   tflspec::tfl_r("list(A = quote(x))"))
})

test_that("without a def, or a table it cannot draw, the field and its reading are today's", {
  for (ty in c("map<column,r>", "rows<map<string,string|r>>", "map<string,number|string>")) {
    expect_false(grepl("rp-tbl", html(arg_field("c", "a", ty)), fixed = TRUE), info = ty)
    expect_identical(html(arg_field("c", "a", ty, def = list(input = "text"))), html(arg_field("c", "a", ty)),
                     info = ty)
  }
  # (a rows<...> with no columns to show: its R field, read as R)
  d <- list(input = "table", type = "rows<map<string,string|r>>")
  expect_identical(html(arg_field("c", "a", d$type, def = d)), html(arg_field("c", "a", d$type)))
  r <- builder_card_read(list(c__a = "list(list(a = 1))"), "c", schema = list(args = list(a = d)), args = "a")
  expect_identical(r$a, tflspec::tfl_r("list(list(a = 1))"))
  r0 <- builder_card_read(list(c__a = "list(list(a = 1))"), "c",
                          schema = list(args = list(a = list(type = d$type))), args = "a")
  expect_identical(r, r0)
  # (a map with no def: the aes map, read as before)
  sch <- list(args = list(mapping = list(type = "map<aes, column|r>")), aes = c("x", "y"))
  expect_identical(builder_card_read(list(c__mapping__x = "AGE"), "c", schema = sch, args = "mapping"),
                   list(mapping = list(x = "AGE")))
})

test_that("a schema function's table argument draws and reads through its tfl_arg_defs() row", {
  .arg_defs_reset()
  on.exit(.arg_defs_reset(), add = TRUE)
  sch <- list(label = "A function", args = list(derive = list(label = "Derived", type = "map<column,r>")))
  testthat::local_mocked_bindings(
    tfl_schema = function(fn = NULL) if (identical(fn, "plan_x")) sch,
    tfl_arg_defs = function(fn = NULL, template = NULL) {
      d <- data.frame(name = "derive", label_en = NA_character_, step = "data", input = "table",
                      type = "map<column,r>", required = FALSE, stringsAsFactors = FALSE)
      d$default <- I(list(NULL))
      d$default_kind <- NA_character_
      d$choices <- I(list("vars(from_args(from))"))
      d$values <- I(list(NULL))
      d$free <- NA
      d$ordered <- NA
      d$columns <- I(list(NULL))
      d$tier <- "usual"
      d$description <- "Columns made: name = R."
      d
    },
    .package = "tflspec")
  s <- .schema_of("plan_x")
  expect_identical(s$args$derive$input, "table")
  v <- list(AGE2 = tflspec::tfl_r("AGE * 2"))
  h <- html(arg_field("c", "derive", s$args$derive$type, value = v, def = s$args$derive,
                      ctx = list(vars = list(adsl = "AGE"), args = list(from = "adsl"))))
  expect_match(h, "rp-tbl-map", fixed = TRUE)
  expect_match(h, "<option value=\"AGE\"></option>", fixed = TRUE)
  input <- stats::setNames(list(tbl_state_of(h)), "c__derive")
  expect_identical(builder_card_read(input, "c", schema = s, args = "derive"), list(derive = v))
})

test_that("an empty table says (none) in a light line that is never read back", {
  d <- list(input = "table", choices = "vars(from_args(from))")
  empty_line <- "<tr class=\"rp-tbl-empty\">\\s*<td class=\"rp-tbl-none\" colspan=\"3\">\\(none\\)</td>\\s*</tr>"
  # (no rows: the line, one cell across the key, the value and the tools)
  h <- html(arg_field("c", "derive", "map<column,r>", def = d, ctx = ctx))
  expect_match(h, empty_line, perl = TRUE)
  expect_match(h, "data-none=\"(none)\"", fixed = TRUE)
  body <- regmatches(h, regexpr("(?s)<tbody class=\"rp-tbl-body\">.*?</tbody>", h, perl = TRUE))
  expect_match(body, "rp-tbl-empty", fixed = TRUE)
  expect_false(grepl("rp-tbl-row", body, fixed = TRUE))
  # (not in the row template Add a row copies)
  tpl <- regmatches(h, regexpr("(?s)<template class=\"rp-tbl-tpl\">.*?</template>", h, perl = TRUE))
  expect_false(grepl("rp-tbl-empty", tpl, fixed = TRUE))
  # (the state read back: nothing)
  st <- tbl_state_of(h)
  expect_false(grepl("(none)", st, fixed = TRUE))
  expect_null(.tbl_read(st))
  expect_null(round_trip("map<column,r>", d, NULL, ctx = ctx))
  # (Japanese)
  hj <- html(arg_field("c", "derive", "map<column,r>", def = d, ctx = ctx, lang = "ja"))
  expect_match(hj, tr("(none)", "ja"), fixed = TRUE)
  # (rows: no line)
  h <- html(arg_field("c", "derive", "map<column,r>", def = d, ctx = ctx,
                      value = list(AGE2 = tflspec::tfl_r("AGE * 2"))))
  expect_false(grepl("rp-tbl-empty", h, fixed = TRUE))
  # (a rows<...> with the template's fixed rows but none of its own: the
  # line under the fixed rows, across its columns)
  all <- list(list(subgroup = "ALL", subgroup_lvl = "ALL", where = tflspec::tfl_r("TRUE")))
  h <- html(arg_field("c", "conditions", "rows<map<string,string|r>>",
                      def = list(input = "table", columns = "params"),
                      ctx = c(ctx["params"], list(vars = c("SEX", "AGE"))), fixed_rows = all))
  expect_match(h, "<td class=\"rp-tbl-none\" colspan=\"4\">(none)</td>", fixed = TRUE)
  expect_lt(regexpr("rp-tbl-fixed", h), regexpr("rp-tbl-empty", h))
  expect_null(.tbl_read(tbl_state_of(h)))
})

test_that("the table's JS: tblValue reads only .rp-tbl-row, the (none) line comes and goes, the script parses", {
  js <- .builder_cards_js
  expect_match(js, "find('tbody.rp-tbl-body > tr.rp-tbl-row')", fixed = TRUE)
  # (Add a row removes the line, the last row's delete puts it back, an
  # optional column widens it)
  expect_match(js, "body.children('tr.rp-tbl-empty').remove();", fixed = TRUE)
  expect_match(js, "!body.children('tr.rp-tbl-row').length", fixed = TRUE)
  expect_match(js, "tr.rp-tbl-empty > td", fixed = TRUE)
  expect_match(js, "this.colSpan + 1", fixed = TRUE)
  node <- Sys.which("node")
  skip_if(!nzchar(node), "node is not available")
  f <- withr::local_tempfile(fileext = ".js")
  writeLines(js, f)
  out <- suppressWarnings(system2(node, c("--check", shQuote(f)), stdout = TRUE, stderr = TRUE))
  expect_null(attr(out, "status"), label = paste(out, collapse = "\n"))
})

test_that("a map of maps (tflspec_summary's nest, value_labels): a row each inner entry, read back nested", {
  d <- list(input = "table", choices = "vars(from_args(data))")
  # nest: map<column,map<column,string>> -- the nested variable, the variable over it, its level
  v <- list(AGE = list(SEX = "Female"), TRT01A = list(SEX = "Male"))
  h <- html(arg_field("c", "nest", "map<column,map<column,string>>", def = d, ctx = ctx, value = v))
  expect_match(h, "data-shape=\"map2\"", fixed = TRUE)
  expect_match(h, "<th>Column</th>", fixed = TRUE)
  expect_match(h, "<th>Second column</th>", fixed = TRUE)
  expect_match(h, "<option value=\"SEX\" selected>SEX</option>", fixed = TRUE)
  expect_identical(round_trip("map<column,map<column,string>>", d, v, ctx = ctx), v)
  # value_labels: map<column,map<string,string>> -- the code typed, its label
  v <- list(SEX = list(F = "Female", M = "Male"))
  h <- html(arg_field("c", "value_labels", "map<column,map<string,string>>", def = d, ctx = ctx, value = v))
  expect_match(h, "<th>Key</th>", fixed = TRUE)
  expect_identical(round_trip("map<column,map<string,string>>", d, v, ctx = ctx), v)
  # (a row with a blank key or value is left out; none left: nothing)
  st <- list(shape = "map2", cols = c("key", "key2", "value"), kinds = c("pick", "text", "text"),
             rows = list(c("SEX", "F", "Female"), c("SEX", "", "x"), c("", "M", "Male")))
  expect_identical(.tbl_read(st), list(SEX = list(F = "Female")))
  st$rows <- list(c("SEX", "", ""))
  expect_null(.tbl_read(st))
})

test_that("a list<map<string,string>> (tflspec_summary's col_header): its maps in order, a row each", {
  d <- list(input = "table")
  v <- list(list(row_label = "", .values = "{col}"), list(row_label = "", .values = "(N={n})"))
  h <- html(arg_field("c", "col_header", "list<map<string,string>>", def = d, ctx = ctx, value = v))
  expect_match(h, "data-shape=\"rows\"", fixed = TRUE)
  expect_match(h, "row_label</th>", fixed = TRUE)
  expect_match(h, ".values</th>", fixed = TRUE)
  # (a blank cell is left out, as a rows<...>'s is)
  expect_identical(round_trip("list<map<string,string>>", d, v, ctx = ctx),
                   list(list(.values = "{col}"), list(.values = "(N={n})")))
})
