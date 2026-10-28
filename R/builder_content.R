# The table builder's content as cards: a table's plan is its items --
# table_plan() and the plan_*() layers, one function call each, in the
# order the plan applies them -- and the builder shows one card an item
# (R/builder_cards.R).  The items are what tflspec makes from the sheets
# (its bridge, .plan_content_from_sheets()): the report's own over the
# study's defaults, each marked where it came from.  The sheets stay the
# planner's storage: a card's edits go through builder_write() (the form)
# or, as R, back through the bridge into the rows of the sheets that verb
# is written from.

# ---- meanings ---------------------------------------------------------------

# What each verb is for, the card's label beside its name, until the
# schema says (`label`); in English, translated by tr()
.plan_meaning <- c(
  normalize_ard = "Normalize the ARD",
  table_plan = "Table structure",
  plan_cells = "Statistics in the cells",
  plan_digits = "Decimals and rounding",
  plan_levels = "Level order",
  plan_labels = "Display names",
  plan_nest = "Rows under another value",
  plan_total = "Total column",
  plan_compare = "Comparison columns",
  plan_stat_groups = "Groups of statistics",
  plan_sort = "Row order",
  plan_filter = "Row filter",
  plan_hide = "Hidden columns",
  plan_col_header = "Column header",
  plan_stub = "Row label column",
  plan_blanks = "Blank rows",
  plan_paginate_rows = "Page breaks by rows",
  plan_paginate_group = "A page per group",
  plan_paginate_cols = "Column pages",
  plan_row_group = "Row groups",
  plan_columns = "Column widths and alignment",
  plan_style = "Table style",
  plan_cell_style = "Cell styles")

# ... and their main arguments'
.plan_arg_meaning <- list(
  normalize_ard = c(hierarchy = "Hierarchy", overall = "Overall row", keys = "Keys",
                    stack = "Stacked analyses", stack_sep = "Stack separator",
                    stack_nest = "Nest the stack", drop_contexts = "Contexts dropped",
                    drop_key_variables = "Drop the key variables"),
  table_plan = c(cols = "Columns", rows = "Row layers", label = "Row text column",
                 stat = "Statistic columns", show_keys = "Keys shown"),
  plan_cells = c(value = "Values used", stats = "Statistics as", na = "Text for missing",
                 default = "Every cell", continuous = "Continuous variables",
                 categorical = "Categorical variables", notes = "Notes of unused statistics"),
  plan_digits = c(rounding = "Rounding", .rows = "Decimals by row"),
  plan_levels = c(.drop_empty = "Hide empty values"),
  plan_sort = c(keys = "Order", stat = "Statistic counted", keep = "Print the sort columns"),
  plan_total = c(label = "Heading", position = "Place"),
  plan_filter = c(where = "Condition", any = "In any column", parents = "Parent rows",
                  by = "Rows judged together"),
  plan_compare = c(by = "By group", position = "Place", "..." = "Columns"),
  plan_stat_groups = c("..." = "Group and its statistics", .variables = "Variables split",
                       .others = "Group of the other variables"),
  plan_nest = c("..." = "Rows under a level"),
  plan_row_group = c(mode = "Group boundary", collapse = "A repeat printed once",
                     group_col = "Group column"),
  plan_stub = c(vars = "Variables", name = "Column name", indent = "Indent",
                group_summary = "Group summary rows", before = "Before the rows"),
  plan_blanks = c(where = "Where", first = "Before the first", last = "After the last",
                  counted = "Count blank rows"),
  plan_paginate_rows = c(max_rows = "Most rows a page", split = "Split rule",
                         page_by = "Page by", heading = "Heading",
                         cont_label = "Continued label", break_before = "Break before rows",
                         min_group_rows = "Fewest rows of a group on a page",
                         heading_at = "Where the heading prints"),
  plan_paginate_cols = c(every = "Columns a page", fit = "Fit to the page",
                         at = "Break before columns", keep = "Columns on every page",
                         cut_by = "Cut by", order = "Order of the pages",
                         allow_span_break = "A spanning header may break",
                         col_header = "Header of the column pages"),
  plan_paginate_group = c(col = "Column", keep = "Keep the column", heading = "Heading",
                          heading_at = "Where the heading prints"),
  plan_col_header = c(header = "Header lines", values = "Values of the tokens",
                      header_sep = "Header separator", col_header_align = "Alignment",
                      lines = "Header lines, a row at a time", span = "Cells over columns"),
  plan_columns = c(widths = "Widths", decimal = "Decimal alignment", row_title = "Row title",
                   auto_width = "Automatic width", sep = "Separator",
                   cell_format = "Cell formats", column_widths_twips = "Column widths (twips)"),
  plan_style = c(align_count_pct = "Align n (%)", border = "Borders",
                 font_size_half_points = "Font size (half points)", font = "Font",
                 row_height_twips = "Row height (twips)", row_height_exact = "Exact row height",
                 header_row_height_twips = "Header row height (twips)",
                 blank_row_height_twips = "Blank row height (twips)",
                 cell_padding_left_twips = "Cell padding, left (twips)",
                 cell_padding_right_twips = "Cell padding, right (twips)",
                 cell_valign = "Vertical alignment", table_align = "Table alignment",
                 markup = "Markup in the text", blank_row_normalize = "Blank rows as one",
                 border_header = "Header borders", border_spanning = "Spanning header borders",
                 border_body = "Body borders", border_first_row = "First row's border",
                 border_last_row = "Last row's border", header_align = "Header alignment",
                 header_bold = "Header bold", header_italic = "Header italic",
                 align = "Alignment", bold = "Bold", italic = "Italic", underline = "Underline",
                 table_width_twips = "Table width (twips)", table_width_pct = "Table width (%)",
                 table_width_pct_of_writable = "Table width (% of the writable width)"),
  plan_hide = c(cols = "Hidden columns"),
  plan_cell_style = c(cols = "Columns styled", header = "The column header",
                      where = "Rows styled", bold = "Bold", italic = "Italic",
                      align = "Alignment", color = "Text colour",
                      background = "Background colour", underline = "Underline",
                      indent_twips = "Indent (twips)", border = "Borders"))

# What each argument means, when it is used, an example: the (i) of a
# card's field (.arg_help()), where tflspec's schema says nothing.  In the
# function's order.
.plan_arg_help <- list(
  normalize_ard = c(
    hierarchy =
      "The nested row keys, outermost first: each level's own summary rows become its rows (a SOC's row over its PTs). Blank: step 1's hierarchical analysis's, which is usually right; another may not match the ARD. e.g. AEBODSYS, AEDECOD",
    overall =
      "The text of the row a hierarchical analysis makes over all its terms (the subjects with any event), put on the first hierarchy column. Blank: that row is left as it is. e.g. Any TEAE",
    keys =
      "The group variables made into columns of their own (TRT01A, SEX ...). Blank: every group of the ARD, which is what a table usually needs. e.g. TRT01A",
    drop_contexts =
      "The kinds of ARD rows (context) left out before the table. Blank: the attributes rows and the total_n row, which no cell prints (the total N is still kept for a header). e.g. attributes",
    drop_key_variables =
      "The rows that count a group variable itself (the subjects per arm): TRUE takes them out; blank (FALSE) keeps them out of the body for a header's (N=n). Usually left blank. e.g. TRUE",
    stack =
      "Stacks parallel subgroup analyses (ALL, SEX, AGEGR1 ...) as one pair of keys, subgroup and subgroup_level, for a subgroup table. e.g. subgroup = ALL, SEX, AGEGR1",
    stack_sep =
      "The text between a crossed layer's names and values (\" / \": AGEGR1 / SEX). e.g. \" x \"",
    stack_nest =
      "TRUE: a crossed layer's outer value goes to subgroup_outer (Age Group by Sex, 65-80, F as three levels). e.g. TRUE"),
  plan_filter = c(
    any =
      "TRUE: a row stays when any column's statistics meet the condition (PTs of 5% or more in any arm); FALSE: when every column's do. Blank: TRUE. e.g. FALSE",
    parents =
      "\"keep\": a parent row (a SOC) stays exactly when one of its children does; \"drop\": it is judged by its own counts. Blank: keep. e.g. \"drop\"",
    by =
      "The row key whose rows are judged together by the condition (with the keys outside it): a SOC kept or dropped as a whole. Blank: each row by itself. e.g. AEBODSYS"),
  plan_blanks = c(
    where =
      "Where blank rows go: \"between_groups\" between the outermost row groups, \"records\" after each record of a listing, or as R (the rows' numbers, 0 before the first). e.g. \"between_groups\"",
    first =
      "TRUE: a blank row at the top of every page. Blank: none. e.g. TRUE",
    last =
      "TRUE: a blank row at the bottom of every page. Blank: none. e.g. TRUE",
    counted =
      "TRUE: the blank rows count toward a page's rows (plan_paginate_rows()' most rows a page), so a page never overflows. Blank: only the table's rows are counted. e.g. TRUE"),
  plan_paginate_rows = c(
    max_rows =
      "The most rows a page holds (blank rows too when they are counted: plan_blanks()). Blank: no limit. e.g. 45",
    split =
      "What a page break may cut: \"group_safe\" whole groups to a page (a group that does not fit starts the next), \"group_force\" every most rows a page, a group cut with a continued label, \"rows\" before the rows given in break before rows. e.g. \"group_force\"",
    page_by =
      "The columns whose value starts a new page first, each page named after it; the most rows a page applies within each. e.g. \"PARAM\"",
    heading_at =
      "Where a page's name (its heading) prints: \"section\" the section header, \"title\" under the titles, \"none\" nowhere. Blank: the report's. e.g. \"title\"",
    break_before =
      "The row numbers a page starts at, for the split rule \"rows\". Used for breaks by hand. e.g. c(20, 40): pages 1-19, 20-39, 40-",
    min_group_rows =
      "The fewest rows of a group a page may end with: a group with fewer moves whole to the next page, so its heading is not left alone at the foot. Blank: 2; 0 turns it off. e.g. 3",
    cont_label =
      "The text after a group's heading on the pages it continues onto. Blank: \" (Cont.)\". e.g. \" (continued)\"",
    heading =
      "With page by one column: each page's name as a template, {value} the page's value and {n} its subjects. e.g. Treatment: {value} (N={n})"),
  plan_paginate_cols = c(
    at =
      "The columns a new column page starts before. One of these, cut by or columns a page is given. e.g. \"Drug Y\"",
    cut_by =
      "The column blocks as a list, or a separator in the columns' names (a block a key value). e.g. \"____\"",
    every =
      "A new column page every this many columns (the columns on every page not counted). e.g. 3",
    keep =
      "The columns every column page repeats. Blank: the row heading columns. e.g. \"row_label\"",
    fit =
      "TRUE: every page's columns on page 1's scale, filling the width; FALSE: each column keeps its width. e.g. FALSE",
    allow_span_break =
      "FALSE: a page is not cut inside a spanning header's cell. Blank: TRUE. e.g. FALSE",
    order =
      "The order the pages come out in: \"across\" a row's column pages one after another, \"down\" all the rows' pages of a block first. Blank: across. e.g. \"down\"",
    col_header =
      "What each column page's header becomes, as R. Used when the pages need a header other than the table's own. Blank: the table's header, cut with its columns."),
  plan_row_group = c(
    mode =
      "How a group's first row is found: \"value\" each run of equal values, \"indent\" a row whose cell is not indented, \"filled\" a row whose cell is not empty, \"auto\". e.g. \"value\"",
    collapse =
      "A repeated value printed once and then blank: TRUE in every row key, or in the columns named. e.g. TRUE",
    group_col =
      "The column whose runs of the same value are the groups, for a table already built (not from an ARD; an ARD table's groups are its outermost row key). e.g. \"V1\""),
  plan_stub = c(
    name =
      "The name of the one row label column the row keys are folded into. Blank: row_label. e.g. \"stub\"",
    indent =
      "The spaces a level of the folded rows is indented by. Blank: 4. e.g. 2",
    before =
      "TRUE: the rows are folded before the pages and groups are made, which plan_cell_style() needs to see the rows as printed; a page or group key folded away is then not found. Blank (FALSE): folded last. e.g. TRUE",
    vars =
      "The row keys folded into the one row label column, outermost first. Blank: worked out from the table's row layers, which is usually right. e.g. c(\"AEBODSYS\", \"AEDECOD\")",
    group_summary =
      "Which rows are a group's own summary, printed on the group's heading row: \"empty\" a blank leaf, \"parent\" a leaf equal to its parent. Blank: both; \"none\" turns it off. e.g. \"empty\""),
  plan_columns = c(
    widths =
      "The columns' relative widths, by column (.values every value column). e.g. row_label 5, .values 2",
    decimal =
      "The columns whose numbers line up at the decimal point (.values every value column). e.g. .values",
    row_title =
      "The row heading columns: left aligned (the others centred) and repeated on every column page. Blank: the first column. e.g. row_label",
    auto_width =
      "TRUE: each column sized to its content. e.g. TRUE",
    sep =
      "The text that joins several column keys in the value columns' names; the spanning header is cut at it. Blank: \"____\" (Placebo____F). Changed only when a value has \"____\" in it. e.g. \"__\"",
    cell_format =
      "A function that formats the cells' text, or a list of them one a column, as R. e.g. function(x) toupper(x)",
    column_widths_twips =
      "Each column's width in twips (1/1440 inch), as R; it replaces the widths above. e.g. c(3000, 1500, 1500)"),
  plan_style = c(
    border =
      "The table's borders: \"tfl\" (rules over and under the header and under the last row) or \"none\"; another style as R (rtf_border()). Blank: tfl. e.g. \"none\"",
    align_count_pct =
      "TRUE: the n and the (%) of a count cell line up from row to row. e.g. TRUE",
    font_size_half_points =
      "The table's font size in half points (18 = 9 pt). Blank: the document's. e.g. 16",
    font =
      "The table's font. Blank: the document's font. e.g. \"Arial\"",
    row_height_twips =
      "The height of the body's rows in twips (1/1440 inch). Blank: from the font size. e.g. 240",
    row_height_exact =
      "TRUE: the rows' height is exact (a longer text is cut); FALSE: it is the least height (a row grows). e.g. TRUE",
    header_row_height_twips =
      "The height of the column header's rows in twips. Blank: as the body's. e.g. 300",
    blank_row_height_twips =
      "The height of the blank rows in twips: lower ones make a table shorter. Blank: as the body's. e.g. 120",
    cell_padding_left_twips =
      "The space between a cell's left border and its text, in twips. Blank: 0. e.g. 60",
    cell_padding_right_twips =
      "The space between a cell's text and its right border, in twips. Blank: 0. e.g. 60",
    cell_valign =
      "Where a cell's text sits from top to bottom: \"bottom\", \"top\" or \"center\". Blank: bottom. e.g. \"top\"",
    table_align =
      "Where the table sits on the page: \"left\", \"center\" or \"right\". Blank: left. e.g. \"center\"",
    markup =
      "The marks in the cells' text turned into print: \"script\" ^{...} superscript and _{...} subscript, \"relational\" >= and <= as signs, \"all\", \"none\". Blank: the document's (script). e.g. \"all\"",
    blank_row_normalize =
      "How blank rows are tidied: \"detect\" an empty row printed as one blank row, \"collapse\" several in a row made one. Blank: both. e.g. \"detect\"",
    border_header =
      "The rules of the column header's rows, one kind of row's (with border_spanning ... border_last_row: a style of one's own in place of Borders). e.g. \"bottom\"",
    border_spanning =
      "The rules under a spanning header's cells (the header over several columns). e.g. \"bottom\"",
    border_body =
      "The rules of the body's rows. Blank: none between them. e.g. \"none\"",
    border_first_row =
      "The rule over the table's first row (the top of the table). e.g. \"top\"",
    border_last_row =
      "The rule under the table's last row (the foot of the table). e.g. \"bottom\"",
    header_align =
      "The column header's alignment: \"left\", \"center\" or \"right\". Blank: center. e.g. \"left\"",
    header_bold =
      "TRUE: the whole column header in bold. e.g. TRUE",
    header_italic =
      "TRUE: the whole column header in italics. e.g. TRUE",
    align =
      "The body's alignment: \"left\", \"center\" or \"right\" (one column's: Cell styles). e.g. \"center\"",
    bold =
      "TRUE: the whole body in bold (some cells only: Cell styles). e.g. TRUE",
    italic =
      "TRUE: the whole body in italics (some cells only: Cell styles). e.g. TRUE",
    underline =
      "TRUE: the whole body underlined (some cells only: Cell styles). e.g. TRUE",
    table_width_twips =
      "The table's width in twips (1/1440 inch); it wins over the widths in percent. Blank: the page's writable width. e.g. 12960",
    table_width_pct =
      "The table's width as a percent (0-100) of the page's writable width. e.g. 80",
    table_width_pct_of_writable =
      "The table's width as a fraction (0-1) of the page's writable width (as table_width_pct, by 0-1). e.g. 0.8"),
  plan_cell_style = c(
    cols =
      "The columns styled, by name (.values every value column). Blank: every column. e.g. .values",
    header =
      "TRUE: the column header is styled (not the body). e.g. TRUE",
    bold =
      "TRUE: the chosen cells in bold; FALSE: not. Blank: as they are. e.g. TRUE",
    italic =
      "TRUE: the chosen cells in italics; FALSE: not. Blank: as they are. e.g. TRUE",
    align =
      "The chosen cells' alignment: \"left\", \"center\" or \"right\". e.g. \"left\"",
    color =
      "The chosen cells' text colour. e.g. \"#CC0000\"",
    background =
      "The chosen cells' background colour. e.g. \"#F2F2F2\"",
    border =
      "The chosen cells' borders, as R (rtf_border()). e.g. rtf_border(bottom = \"single\")",
    underline =
      "The chosen cells underlined: TRUE / FALSE, or a condition as R. e.g. TRUE",
    indent_twips =
      "The left indent of the chosen cells' text in twips (not the header's). e.g. 240"),
  plan_cells = c(
    stats =
      "How the statistics are laid out: \"cells\" one template a cell (n (%) in one cell), \"rows\" each statistic a row of its own. Blank: cells. e.g. \"rows\"",
    value =
      "Which value a template's {x} reads: the number (stat), which the template formats, or the ARD's formatted text (stat_fmt). Blank: stat. e.g. \"stat_fmt\"",
    na =
      "The text of a cell no template could fill (a statistic the ARD does not have). Blank: empty. e.g. \"-\"",
    notes =
      "FALSE: no note of the ARD's statistics that no template uses. Blank (TRUE): such statistics are listed when the table is made. e.g. FALSE"),
  plan_total = c(
    label =
      "The heading of the Total column. Its cells are the ARD's overall rows (the analyses run over all subjects, not split by the column key). Blank: \"Total\". e.g. \"All Subjects\"",
    position =
      "Where the Total column goes among the column key's values: \"last\" or \"first\". Blank: last. e.g. \"first\""),
  plan_compare = c(
    by =
      "The column that splits the comparison records (tests, differences) into columns, one a level (each arm against placebo). Blank: one comparison a variable. e.g. \"COMPARISON\"",
    position =
      "Where the comparison columns go: after the column key's values (\"last\") or before them (\"first\"). Blank: last. e.g. \"first\""),
  plan_stat_groups = c(
    `.variables` =
      "The variables whose statistics are split into the groups. Blank: every variable. e.g. c(\"AGE\", \"BMI\")",
    `.others` =
      "The group the other variables' statistics go to whole. Blank: none. e.g. \"Summary\""),
  plan_sort = c(
    keep =
      "FALSE: the columns the order is read from (the sort keys) are not printed, with no plan_hide() for them. Blank (TRUE): they are printed as they are. e.g. FALSE"),
  plan_hide = c(
    cols =
      "The columns not printed, by name: a key a page break or an order reads stays in the data. e.g. TRT01AN"),
  plan_col_header = c(
    values =
      "What the header's tokens take: TRUE (list(n = TRUE)) reads each column's N from the ARD; numbers named by column give them by hand. e.g. list(n = TRUE)",
    header_sep =
      "The text a plain table's column names are cut at into spanning header rows. e.g. \"____\"",
    col_header_align =
      "The column header's alignment: \"left\", \"center\" or \"right\". Blank: center. e.g. \"left\"",
    span =
      "How a cell of the header lines over several columns is made: \"each\" a cell a column, \"one\" one cell over them all, or a key's name (a cell a value of that key: a spanner). Blank: each. e.g. \"one\""),
  plan_paginate_group = c(
    col =
      "The column whose value starts a new page, a page a value however long. Blank: the outermost row key. e.g. \"PARAM\"",
    keep =
      "FALSE: the page column is not printed (each page's name says its value). Blank (TRUE): it is printed. e.g. FALSE",
    heading =
      "Each page's name as a template, {value} the page's value and {n} its subjects. Blank: the value. e.g. Parameter: {value}",
    heading_at =
      "Where a page's name (its heading) prints: \"section\" the section header, \"title\" under the titles, \"none\" nowhere. Blank: the report's. e.g. \"title\""))

# a verb's meaning: the schema's label, else the table above; NULL for none
.verb_meaning <- function(fn) {
  lab <- .schema_of(fn)$label
  if (!is.null(lab)) return(lab)
  if (fn %in% names(.plan_meaning)) .plan_meaning[[fn]] else NULL
}

# an argument's meaning, likewise
.arg_meaning <- function(fn, arg) {
  lab <- .schema_of(fn)$args[[arg]]$label
  if (!is.null(lab)) return(lab)
  m <- .plan_arg_meaning[[fn]]
  if (!is.null(m) && arg %in% names(m)) m[[arg]] else NULL
}

# ---- the items ----------------------------------------------------------------

# a function of tflspec's own (the bridge), not exported by it
.tflspec_fn <- function(name) get(name, envir = asNamespace("tflspec"))

# The items of a report's table as the builder shows them, in the plan's
# order, the ARD's normalization (normalize_ard()) first: each a list --
# `fn`, `args`, `from` (where the item comes from: "own" the report says
# it; "defaults" the study's defaults; "pattern:<name>" a pattern), `arg_from`
# (each argument's: the same words), `below` (the item under the report's
# own: what a Revert goes back to; NULL none), `path` (the card's and the
# code's name for it: the verb, "#2" for its second call), `revert` (an
# item of the report's own over one of a layer: "Revert" removes the own),
# `present` (TRUE; FALSE for a card shown though the plan has no such item
# yet).  `args` are the plan's (the report's over its layers'), `own_args`
# what the report says itself (NULL: nothing).  Read from the spec (the
# report's YAML over its layers); from the sheets while they hold edits
# the spec has not taken yet.  normalize_ard() is required, and its
# hierarchy step 1's (`meta`: ard_meta(), for the sheets' way).
builder_items <- function(x, output_id, meta = NULL) {
  sp <- .spec_items(x, output_id)
  items <- if (is.null(sp)) .builder_items_sheets(x, output_id, meta) else {
    own_fn <- vapply(sp$own, function(it) it$fn, "")
    seen <- character()
    lapply(sp$items, function(it) {
      fn <- it$fn
      seen <<- c(seen, fn)
      k <- sum(seen == fn)
      at <- which(own_fn == fn)
      from <- if (identical(fn, "table_plan")) "own" else it$from %||% "own"
      list(fn = fn, args = it$args %||% list(),
           own_args = if (length(at) >= k) sp$own[[at[k]]]$args %||% list(),
           from = from, arg_from = it$arg_from, below = it$below,
           path = if (k > 1L) paste0(fn, "#", k) else fn,
           revert = identical(from, "own") && !is.null(it$below),
           present = TRUE)
    })
  }
  .builder_items_ard_first(items)
}

# ... from the sheets (the bridge): the report's and the defaults' items,
# and normalize_ard() from the ARD's hierarchy (`meta`) and the table's
# any-event row (tables$overall_label)
.builder_items_sheets <- function(x, output_id, meta = NULL) {
  sp <- .spec_object_last(x, table_sheets(), .study_keys$table)
  bridge <- .tflspec_fn(".plan_content_from_sheets")
  # the ARD's definition: no plan_levels the ARD already says (tflspec #358)
  ard <- tryCatch(suppressWarnings(.ard_spec(x$ard)), error = function(e) NULL)
  get <- function(...) tryCatch(bridge(sp, ..., ard_spec = ard), error = function(e) list())
  res <- get(output_id, own = FALSE)
  own <- get(output_id, own = TRUE)
  dfl <- get(NA)
  fns <- function(l) vapply(l, function(it) it$fn, "")
  own_fn <- fns(own)
  dfl_fn <- fns(dfl)
  seen <- character()
  items <- lapply(res, function(it) {
    fn <- it$fn
    seen <<- c(seen, fn)
    k <- sum(seen == fn)
    # own: the report says it -- table_plan() is always the report's own
    mine <- fn %in% own_fn || identical(fn, "table_plan")
    at <- which(own_fn == fn)
    list(fn = fn, args = it$args %||% list(),
         own_args = if (length(at) >= k) own[[at[k]]]$args %||% list(),
         from = if (mine) "own" else "defaults",
         path = if (k > 1L) paste0(fn, "#", k) else fn,
         revert = mine && fn %in% dfl_fn,
         present = TRUE)
  })
  if (any(fns(items) == "normalize_ard")) return(items)
  args <- list()
  hier <- meta$hierarchy %||% character()
  if (length(hier)) args$hierarchy <- as.character(hier)
  ov <- .rows_for(x, "tables", output_id)$overall_label[1L] %||% NA_character_
  if (length(hier) && !is.na(ov) && nzchar(ov)) args$overall <- ov
  c(list(list(fn = "normalize_ard", args = args, own_args = if (length(args)) args,
              from = if (length(args)) "own" else "defaults", path = "normalize_ard",
              revert = FALSE, present = TRUE)), items)
}

# The items with the ARD's normalization (normalize_ard(): what the plan
# is given) first -- one, required, and its arguments not edited as R
# here (its hierarchy is step 1's); a content with none has the defaults'
.builder_items_ard_first <- function(items) {
  na <- vapply(items, function(it) identical(it$fn, "normalize_ard"), NA)
  it <- if (any(na)) items[[which(na)[1L]]] else
    list(fn = "normalize_ard", args = list(), from = "defaults", path = "normalize_ard",
         present = TRUE)
  it$required <- TRUE
  it$fixed <- TRUE
  it$revert <- FALSE
  c(list(it), items[!na])
}

# The items with the cards shown though the plan has none yet (`always`:
# the verbs whose cards hold a form that makes the item -- the Total
# column's switch, the column header's lines), each placed after the last
# item that comes before it in the schema's order
.builder_items_shown <- function(items, always = character()) {
  have <- vapply(items, function(it) it$fn, "")
  rank <- function(fn) as.numeric(.schema_of(fn)$rank %||% 500)
  for (fn in setdiff(always, have)) {
    it <- list(fn = fn, args = list(), from = "own", path = fn, revert = FALSE,
               present = FALSE)
    r <- rank(fn)
    have <- vapply(items, function(it) it$fn, "")
    before <- which(vapply(have, rank, 0) <= r)
    at <- if (length(before)) max(before) else 0L
    items <- append(items, list(it), after = at)
  }
  items
}

# ---- the cards ------------------------------------------------------------------

# One item's arguments as they are, read only: a line an argument, its
# label "meaning (arg)" and its value as R.  The card of a verb the builder
# has no form for yet (the study's layout verbs ...): what the program says
# is on screen, and "Edit as R" changes it.
.item_args_view <- function(it, lang = "en") {
  a <- it$args
  if (!length(a)) {
    return(shiny::div(class = "small text-muted", tr("No arguments: the defaults of the function.", lang)))
  }
  nm <- names(a) %||% rep("", length(a))
  # (in the function's order; the ... where the function has it)
  at <- order(match(ifelse(nzchar(nm), nm, "..."), .fn_formals(it$fn, .schema_of(it$fn)),
                    nomatch = length(a) + 1000L), seq_along(a))
  a <- a[at]
  nm <- nm[at]
  shiny::tags$dl(
    class = "rp-args-view",
    lapply(seq_along(a), function(i) {
      arg <- if (nzchar(nm[[i]])) nm[[i]] else "..."
      shiny::tagList(
        shiny::tags$dt(builder_label(.arg_meaning(it$fn, arg), arg, "arg", lang)),
        shiny::tags$dd(shiny::tags$code(class = "rp-mono", .arg_to_r(a[[i]]))))
    }))
}

# The summary of a closed card: its arguments as "meaning (name) = value",
# each value short (a text bare, a vector's values by commas, a map's
# name: value, what is R as its R), each pair and the whole cut short
.item_summary <- function(it, width = 90L, lang = "en") {
  a <- it$args
  if (!length(a)) return("")
  cut <- function(s, n) if (nchar(s) > n) paste0(substr(s, 1L, n - 1L), "\u2026") else s
  nm <- names(a) %||% rep("", length(a))
  pairs <- vapply(seq_along(a), function(i) {
    arg <- if (nzchar(nm[[i]])) nm[[i]] else "..."
    v <- tryCatch(.summary_value(a[[i]]), error = function(e) "")
    paste0(.summary_label(it$fn, arg, lang), " = ", cut(gsub("[[:space:]]+", " ", v), 32L))
  }, "")
  cut(paste(pairs, collapse = "; "), width)
}

# ... an argument's "meaning (name)" as text (the name alone when it has
# no meaning, or its meaning says the name already: builder_label()'s rule)
.summary_label <- function(fn, arg, lang = "en") {
  m <- .label_text(.arg_meaning(fn, arg), lang)
  if (!is.null(m)) {
    for (tail in c(paste0("(", arg, ")"), paste0("\uff08", arg, "\uff09"))) {
      if (endsWith(m, tail)) m <- trimws(substr(m, 1L, nchar(m) - nchar(tail)))
    }
  }
  if (is.null(m) || !nzchar(m) || identical(m, arg)) arg else paste0(m, " (", arg, ")")
}

# ... a value, short
.summary_value <- function(v) {
  if (is.null(v)) return("NULL")
  if (inherits(v, c("tfl_r", "tfl_call"))) return(.arg_to_r(v))
  if (is.list(v)) {
    n <- names(v) %||% rep("", length(v))
    parts <- vapply(seq_along(v), function(i) {
      x <- .summary_value(v[[i]])
      if (nzchar(n[[i]])) paste0(n[[i]], ": ", x) else x
    }, "")
    return(paste(parts, collapse = ", "))
  }
  if (is.atomic(v)) {
    if (is.factor(v)) v <- as.character(v)
    x <- if (is.character(v)) v else format(v, trim = TRUE)
    if (!is.null(names(v)) && any(nzchar(names(v)))) {
      x <- ifelse(nzchar(names(v)), paste0(names(v), ": ", x), x)
    }
    return(paste(x, collapse = ", "))
  }
  .arg_to_r(v)
}

# The two groups of a table's items, by verb, in the order a person
# designs a table: the mapping (ARD -> table: which variables and
# statistics go to which rows, columns and cells -- the structure, the
# cells, the statistic groups, the Total, the comparisons, the row filter)
# and the finishing (the content and the look: the values' order, the
# labels, the nesting, the decimals, the row order, the column header, the
# layout).  normalize_ard() is neither: the ARD made ready for the plan,
# first, with no line of its own.  The schema's `stage` (mapping /
# finishing) says it when tflspec has it; else this table.
.plan_sections <- list(
  mapping = c("table_plan", "plan_cells", "plan_stat_groups", "plan_total", "plan_compare",
              "plan_filter"),
  finish = c("plan_levels", "plan_labels", "plan_nest", "plan_digits", "plan_sort",
             "plan_col_header", "plan_stub", "plan_blanks", "plan_paginate_rows",
             "plan_paginate_group", "plan_paginate_cols", "plan_row_group", "plan_hide",
             "plan_columns", "plan_style", "plan_cell_style"))

# (the words are the strings' (inst/i18n): swapped there)
.plan_section_labels <- c(mapping = "Table skeleton", finish = "Finishing", ard = "ARD")

# each group's line: what it is for, in a line
.plan_section_notes <- c(
  mapping = "Which ARD values go to which rows, columns and cells",
  finish = "Labels, order, decimals, pages: the look")

.plan_section <- function(fn) {
  if (identical(fn, "normalize_ard")) return("ard")
  st <- .schema_of(fn)$stage
  if (identical(st, "mapping")) return("mapping")
  if (identical(st, "finishing")) return("finish")
  for (k in names(.plan_sections)) if (fn %in% .plan_sections[[k]]) return(k)
  "finish"
}

# the cards seldom changed: closed when drawn (the cards' menu reaches them)
.plan_rare <- c("plan_stub", "plan_blanks", "plan_style", "plan_columns", "plan_cell_style",
                "plan_paginate_rows", "plan_paginate_group", "plan_paginate_cols")
