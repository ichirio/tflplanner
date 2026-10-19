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

# ---- which cards, and their switches ---------------------------------------------

# Three kinds of card: the required (always, no switch); the usual ones of
# the table's kind and the study defaults' (shown though the content has
# none: off, a switch turns them on); the rest only through "Add" (a card
# the content has is shown whatever its kind).
.plan_required <- c("normalize_ard", "table_plan", "plan_cells")

# (one list for every table, whatever its kind -- #417 Q2; `hier` unused)
.plan_usual <- function(hier = FALSE) {
  c("plan_levels", "plan_labels", "plan_sort", "plan_digits", "plan_col_header", "plan_total",
    "plan_filter")
}

.plan_study_cards <- c("plan_stub", "plan_blanks", "plan_paginate_rows", "plan_columns",
                       "plan_style")

# The items as cards: the content's, the required and usual ones it lacks
# (off), and those added in the builder (`added`: verbs on, waiting for a
# value; `off` those of them switched off, kept as cards) -- each in the
# schema's order among the content's; each told whether it has a switch
# (`switch`), whether it is on (`on`) and whether the menu's Remove takes
# it away (`removable`: any card not required -- a usual one comes back
# off when drawn again).  A
# switch only where its on / off really writes: off a card the content
# lacks (on writes it, or opens it to be filled); on a report's own item
# with no study default under it (off takes its rows away).  An item of
# the study's defaults has none (its Revert, or the study's defaults,
# change it).
#
# The Total column (plan_total()) reads the ARD's rows over all subjects
# and stops without them: an ARD known to have none (`ard_total` "none",
# .ard_total_kind()) leaves its card off with no switch, saying why
# (`blocked`); its own switch otherwise (on / off works from the study's
# defaults too).
.builder_items_cards <- function(items, hier = FALSE, added = character(), ard_total = NA,
                                off = character()) {
  usual <- c(.plan_usual(hier), .plan_study_cards)
  items <- .builder_items_shown(items, unique(c("table_plan", "plan_cells", usual, added)))
  lapply(items, function(it) {
    req <- it$fn %in% .plan_required || isTRUE(it$required)
    it$added <- !isTRUE(it$present) && it$fn %in% setdiff(added, off)
    it$removable <- !req && !isTRUE(it$fixed)
    it$switch <- !req && (!isTRUE(it$present) ||
                            (identical(it$from, "own") && !isTRUE(it$revert)))
    if (identical(it$fn, "plan_total")) {
      it$switch <- TRUE
      if (!isTRUE(it$present) && identical(ard_total, "none")) {
        it$switch <- FALSE
        it$blocked <- "The ARD has no overall rows."
      }
    }
    it$on <- isTRUE(it$present) || req || it$added
    it
  })
}

# The ARD's Total: "overall" when it has rows over all subjects (no
# column key: ard_stack(.overall = TRUE)), which plan_total() makes the
# Total column; "none" when it has none (plan_total() would stop); NA not
# known (an older saved meta without `overall`).  `meta`: ard_meta().
.ard_total_kind <- function(meta) {
  if (is.null(meta) || is.null(meta$overall)) return(NA_character_)
  if (isTRUE(meta$overall)) "overall" else "none"
}

# The usual cards the ARD itself tells the values of -- on by default (a
# new table writes them): the Total column when the ARD has rows over all
# subjects, the values' order when its variables or its key columns keep a
# factor's levels, the labels when it carries the variables' labels.
# `meta`: ard_meta().
.ard_on_cards <- function(meta, hier = FALSE) {
  if (is.null(meta)) return(character())
  usual <- .plan_usual(hier)
  v <- meta$variables
  on <- c(if (identical(.ard_total_kind(meta), "overall")) "plan_total",
          if (length(intersect(meta$factor_vars, v$variable)) ||
                length(meta$factor_keys)) "plan_levels",
          if (is.data.frame(v) && any(!is.na(v$label) & nzchar(v$label %||% ""))) "plan_labels")
  intersect(on, usual)
}

# The verbs "Add" offers: the plan's, in the schema's order, less those
# shown (a verb is one card) and the required
.plan_addable <- function(items) {
  shown <- vapply(items, function(it) it$fn, "")
  setdiff(names(.plan_meaning), c(.plan_required, shown))
}

# ... the schema of those verbs, for add_item_input() (NULL: none known)
.plan_add_schema <- function(items) {
  sch <- tryCatch(tflspec::tfl_schema(), error = function(e) NULL)
  if (is.null(sch)) return(NULL)
  sch[intersect(names(sch), .plan_addable(items))]
}

# What a card switched on (or added) is written with: its values from the
# ARD and the usual ones (`st`: builder_read()'s state); NULL for a verb
# with no such value (its card opens to be filled: its fields or R)
.plan_starter_args <- function(fn, st = NULL, hier = FALSE, meta = NULL) {
  v <- st$variables
  switch(
    fn,
    # the ARD's labels (none: the card opens to be filled)
    plan_labels = {
      if (!is.data.frame(v) || !nrow(v)) return(NULL)
      has <- !is.na(v$label) & nzchar(v$label)
      if (!any(has)) return(NULL)
      as.list(stats::setNames(v$label[has], v$variable[has]))
    },
    # the levels in their order (.level_order()): the variables' and the
    # key columns' (the columns' arms, a key's levels) -- those the ARD
    # keeps a factor of, when it keeps any
    plan_levels = {
      lv <- Filter(length, st$levels %||% list())
      fac <- c(meta$factor_vars, meta$factor_keys)
      if (length(fac)) {
        keys <- setdiff(meta$factor_keys, names(lv))
        kl <- stats::setNames(lapply(keys, function(k) st$arms[[k]] %||% meta$keys[[k]]), keys)
        lv <- Filter(length, c(lv, kl))
        lv <- lv[intersect(names(lv), fac)]
      }
      if (!length(lv)) NULL else lv
    },
    plan_digits = list(rounding = "sas"),
    plan_sort = if (hier && length(st$row_keys)) {
      list(keys = trimws(strsplit(.hier_sort(
        c(rep("alpha", length(st$row_keys) - 1L), "freq"),
        hier = st$row_keys %in% st$hierarchy), "|", fixed = TRUE)[[1L]]))
    },
    plan_col_header = {
      hp <- header_presets()
      ch <- .header_preset_choices(hier)
      if (!length(ch)) NULL else list(header = hp[[ch[[1L]]]])
    },
    plan_stub = list(name = "row_label"),
    plan_blanks = list(where = "between_groups"),
    plan_paginate_rows = list(max_rows = 30L, split = "group_safe"),
    plan_style = list(align_count_pct = TRUE),
    NULL)
}

# Where a variable's order comes from (.level_order()'s `source` and
# `differs`), in words: the same lines under the columns' order and on
# plan_levels()' card.  This table's own order says nothing; as first
# seen is a warning (check it).  A list: `text` (the sentences), `warn`.
.level_order_words <- c(
  ard = "The ARD's order (its factor levels).",
  codelist = "The code list's order (ARD step 1-1).",
  seen = "Check this order: the data do not give one (no factor, no numeric twin such as TRT01AN), so it is the ARD's.",
  differs = "The ARD's order differs from the code list's; the table uses the code list's.")

.level_order_text <- function(from, lang = "en") {
  src <- from$source %||% "seen"
  # (this table's own, or the plan's -- a stack's subgroups: nothing to say)
  if (src %in% c("table", "plan")) return(NULL)
  words <- .level_order_words[[if (src %in% names(.level_order_words)) src else "seen"]]
  txt <- tr(words, lang)
  if (isTRUE(from$differs)) txt <- paste(txt, tr(.level_order_words[["differs"]], lang))
  list(text = txt, warn = identical(src, "seen") || isTRUE(from$differs))
}

# ... as a line (NULL: none)
.level_order_note <- function(from, lang = "en") {
  w <- .level_order_text(from, lang)
  if (is.null(w)) return(NULL)
  shiny::div(class = paste("small rp-level-from", if (w$warn) "text-warning" else "text-muted"),
             w$text)
}

# A card's own line above its body, by verb (`r`: the card's Edit as R is
# offered beside it; `level_from`: the variables' orders' sources, for
# plan_levels()): what the card does not show yet, or where its values go
.plan_card_note <- function(fn, r = FALSE, lang = "en", level_from = NULL) {
  t <- function(x) tr(x, lang)
  switch(
    fn,
    # (said behind the (i): where the card writes -- plan_cells(), the
    # study's defaults.yml -- one card a function's exception)
    plan_digits = shiny::div(class = "rp-card-note",
                             help_tip(paste(t("The decimals are written into plan_cells()'s formats."),
                                            t("Applies to every report (the study's defaults.yml)")))),
    # (a line; what it does and one row before and after under the (i))
    normalize_ard = shiny::div(
      class = "rp-card-note",
      with_tip(t("Shapes the ARD for making the table."),
               t(paste("normalize_ard() shapes the cards ARD for making the table.",
                       "Before: group1 = TRT01A, group1_level = Drug X, variable = SEX,",
                       "variable_level = F, stat_name = n -> After: TRT01A = Drug X,",
                       "variable = SEX, .label = F, stat_name = n. The groups become columns",
                       "(TRT01A), the analysis variables stay values of variable, and a",
                       "category's value becomes .label (the row's text).")))),
    plan_levels = shiny::div(
      class = "rp-card-note",
      help_tip(t(paste("Sets the order of the levels of the column and row keys (the groups,",
                       "an analysis variable's values). The analysis variables' own order",
                       "(variable) is here too. Levels with no data can be hidden (.drop_empty).")))),
    plan_sort = shiny::div(
      class = "rp-card-note",
      with_tip(builder_label(.arg_meaning("plan_sort", "keys"), "keys", "arg", lang),
               t(paste("The keys, first to last: .overall the any-event row first;",
                       "group1, group2 ... the rows' outer levels, label the innermost",
                       "(the rows' text), each by name (a code list's order when it has one);",
                       ".depth each group's own row above the rows under it;",
                       "-n most frequent first (-n@group1: by the group's own count).")))),
    NULL)
}

# A card's mark: where its item comes from (normalize_ard() with no
# argument -- a table of no hierarchy -- none: nothing of anyone's on it)
.card_from <- function(it) {
  if (identical(it$fn, "normalize_ard") && !length(it$args)) NULL else it$from
}
# ... its mark and Revert as a card's head has them (sent after a write:
# the page puts them in the card's head, .builder_cards_js)
.card_mark <- function(it, id, lang = "en") {
  shiny::tagList(
    from_badge(.card_from(it), lang,
               own = if (isTRUE(it$present) && identical(it$from, "own")) "This table"),
    if (isTRUE(it$revert)) .card_revert(id, lang))
}

# The cards of a table's content, in its order: one builder_card() an
# item, with a thin line before the first item of each group (Mapping,
# Finishing; its one line of what it is for) -- the content's order is
# kept, the line only says where a group starts, and each card's head
# names its group (faint), so a group that comes back later is still told.
# normalize_ard() has no line: first, its head says ARD.  `bodies`: the
# forms the builder has for some (by path, or by verb); the rest show
# their arguments.  `id`: the card's input id of a path.  The required
# cards (.plan_required, and an item marked `required`) have the bar; the
# others a switch where the item says (`switch`, .builder_items_cards();
# else where `switches` names the verb), none elsewhere.  An item of the
# report's own is marked "This table".  The seldom changed cards
# (.plan_rare) are closed when drawn.  `new`: the path of a card just
# added (it takes the focus).  `level_from`: where each variable's order
# comes from (builder_read()'s), said on plan_levels()' card.
.table_cards <- function(items, bodies = list(), id = function(path) path,
                         switches = character(), lang = "en", new = NULL,
                         level_from = NULL) {
  t <- function(x) tr(x, lang)
  started <- character()
  out <- list()
  for (it in items) {
    body <- bodies[[it$path]] %||% bodies[[it$fn]]
    own_form <- !is.null(body)
    if (!own_form) body <- .item_args_view(it, lang)
    req <- it$fn %in% .plan_required || isTRUE(it$required)
    sw <- if (!is.null(it$switch)) isTRUE(it$switch) else it$fn %in% switches
    on <- if (!is.null(it$on)) isTRUE(it$on) else (isTRUE(it$present) || req || !sw)
    # a card that cannot be on (the ARD lacks what it reads): off, its
    # reason where its body would be
    blocked <- !is.null(it$blocked)
    if (blocked) on <- FALSE
    pending <- !isTRUE(it$present) && on
    # an item the content has not (its form would add it): no mark of its
    # own -- "not in the code yet"
    absent <- !isTRUE(it$present) && !blocked && (pending || (!sw && !req))
    tools <- c(if ((isTRUE(it$present) || pending) && !isTRUE(it$fixed)) "r",
               if (isTRUE(it$removable) && !req) "remove")
    sec <- .plan_section(it$fn)
    if (!sec %in% started) {
      started <- c(started, sec)
      if (sec %in% names(.plan_section_notes)) {
        out[[length(out) + 1L]] <- shiny::div(
          class = "rp-card-sep", `data-section` = sec,
          shiny::tags$span(class = "rp-card-sep-label", t(.plan_section_labels[[sec]])),
          shiny::tags$span(class = "rp-card-sep-note", t(.plan_section_notes[[sec]])))
      }
    }
    rare <- it$fn %in% .plan_rare
    note <- .plan_card_note(it$fn, r = "r" %in% tools, lang = lang, level_from = level_from)
    from <- .card_from(it)
    out[[length(out) + 1L]] <- builder_card(
      id(it$path), it$fn, .verb_meaning(it$fn), from = from, required = req,
      on = on, switchable = sw, revert = isTRUE(it$revert),
      # (off: no body -- the summary says what turning it on does)
      body = if (!on) NULL else if (is.null(note)) body else shiny::tagList(note, body),
      # (off: what turning it on does, said where its body would be)
      summary = if (isTRUE(it$present)) .item_summary(it, lang = lang) else
        if (blocked) t(it$blocked) else if (!on) t("On: its usual values are written."),
      tools = tools,
      # the study's defaults the report leaves as they are, the seldom
      # changed and the off: closed
      open = on && (pending || (!rare && (req || own_form || identical(it$from, "own")))),
      path = it$path, lang = lang,
      own_label = if (isTRUE(it$present) && identical(it$from, "own")) "This table",
      absent = absent, rare = rare, section = sec,
      tag = .plan_section_labels[[sec]],
      new = identical(it$path, new),
      also = if (identical(it$fn, "plan_digits")) "plan_cells")
  }
  out
}

# The bar above the cards: "Cards (plan)" -- every card in the content's
# order, by group, each a link that opens it and brings it into view (the
# seldom changed ones faint)
.table_cards_bar <- function(items, id = function(path) path, lang = "en") {
  t <- function(x) tr(x, lang)
  started <- character()
  entries <- list()
  for (it in items) {
    sec <- .plan_section(it$fn)
    if (!sec %in% started) {
      started <- c(started, sec)
      if (sec %in% names(.plan_section_notes)) {
        entries[[length(entries) + 1L]] <- shiny::tags$li(
          shiny::tags$h6(class = "dropdown-header", t(.plan_section_labels[[sec]])))
      }
    }
    entries[[length(entries) + 1L]] <- shiny::tags$li(shiny::tags$a(
      href = "#", class = paste("dropdown-item rp-layer-go",
                                if (it$fn %in% .plan_rare) "rp-layer-rare"),
      `data-target` = id(it$path),
      builder_label(.verb_meaning(it$fn), it$fn, "fn", lang)))
  }
  shiny::div(
    class = "rp-cards-bar",
    shiny::div(class = "dropdown",
               shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary dropdown-toggle rp-layers-btn",
                                  `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
                                  t("Cards (plan)")),
               shiny::tags$ul(class = "dropdown-menu", entries)))
}

# ---- the forms of the layout cards -------------------------------------------------

# The cards with a form of fields by the schema's types (arg_field()): the
# most used arguments on the card, the rest folded under "Other arguments"
.plan_form_main <- list(
  normalize_ard = c("overall", "stack"),
  plan_filter = c("any", "parents"),
  plan_blanks = c("where", "first", "last"),
  plan_paginate_rows = c("max_rows", "split", "page_by"),
  plan_paginate_cols = c("every", "fit", "at", "keep", "cut_by", "order", "allow_span_break"),
  plan_paginate_group = c("col", "keep", "heading"),
  plan_row_group = c("mode", "collapse"),
  plan_stub = c("name", "indent", "before"),
  plan_columns = c("widths", "decimal", "row_title", "auto_width"),
  plan_style = c("border", "align_count_pct", "font_size_half_points"),
  plan_hide = "cols",
  plan_cell_style = c("cols", "header", "where", "bold", "italic", "align", "color",
                      "background"))

# the types the form uses where the schema says none (or a looser one): a
# choice of the usual values, or a number; "widths" a number a column
# (.widths_field()).  plan_cell_style()'s looks are a choice of TRUE /
# FALSE with a blank (left alone: not written), not a check (an unticked
# check would say FALSE); its `where` the condition as R, over the
# table's columns (.formula_text()).
.plan_cell_look <- "enum[TRUE|FALSE]|logical"
.plan_form_types <- list(
  normalize_ard = c(hierarchy = "list<column>", keys = "list<column>", overall = "string", drop_contexts = "r",
                    drop_key_variables = "logical", stack = "r", stack_sep = "string",
                    stack_nest = "logical"),
  plan_hide = c(cols = "list<column>"),
  plan_cell_style = c(cols = "list<column>", where = "formula",
                      bold = .plan_cell_look, italic = .plan_cell_look,
                      underline = .plan_cell_look, align = "enum[left|center|right]",
                      color = "colour", background = "colour"),
  plan_blanks = c(where = "enum[between_groups|records]|r"),
  plan_columns = c(widths = "widths", row_title = "list<column>"),
  plan_paginate_cols = c(order = "enum[across|down|group|rows|cols]"),
  plan_style = c(border = "string", align_count_pct = "logical",
                 font_size_half_points = "integer"))

# the arguments of a form card given as R (no field of their own)
.plan_form_r <- list(
  plan_filter = "by",
  plan_cell_style = "border",
  plan_columns = c("cell_format", "column_widths_twips"),
  plan_paginate_cols = "col_header",
  plan_row_group = "group_col")

# a form card's schema: tflspec's, with the types above, its formals the
# function's own (not `plan`) and its visible arguments the main ones
.plan_form_schema <- function(fn) {
  sch <- .schema_of(fn) %||% list()
  for (a in names(.plan_form_types[[fn]])) {
    sch$args[[a]]$type <- .plan_form_types[[fn]][[a]]
  }
  for (a in .plan_form_r[[fn]]) sch$args[[a]]$type <- "r"
  fl <- tryCatch(formals(utils::getFromNamespace(fn, "rtfreporter")), error = function(e) NULL)
  fo <- names(fl)
  # a check's default the function's own (plan_paginate_group(keep = TRUE)):
  # the field ticked as the function would be
  for (a in intersect(fo, names(sch$args))) {
    d <- fl[[a]]
    if (identical(.arg_kind(sch$args[[a]]$type), "logical") && is.null(sch$args[[a]]$default) &&
        is.logical(d) && length(d) == 1L && !is.na(d)) {
      sch$args[[a]]$default <- d
    }
  }
  sch$formals <- setdiff(unique(c(fo, names(sch$args))), c("plan", ".plan", "..."))
  # (no argument is read only: normalize_ard()'s hierarchy is a field too,
  # marked the ARD's while it says the same, warned when it does not)
  sch$fixed <- character()
  # (the main fields in the function's order too)
  sch$visible <- .arg_order(.plan_form_main[[fn]], sch$formals)
  sch
}

# where an argument's value came from: the report's own, the study's
# defaults, a pattern (the item's arg_from, read from the spec); else a
# value the item has that the report does not say is the defaults'
.arg_from <- function(it, a) {
  af <- it$arg_from
  if (!is.null(af) && a %in% names(af)) return(af[[a]])
  if (a %in% names(it$own_args %||% list())) "own" else if (a %in% names(it$args)) "defaults" else "own"
}

# The function's arguments a card shows, in two groups (each in the
# function's order, a data argument -- x, plan -- none): `upper` those the
# call's code writes -- the required, the item's arguments
# (tflspec::tfl_item_written()), those the ARD decides (`ard`) -- and
# `lower` the rest.  The same for every report: no kind of table shows or
# hides an argument.
.data_args <- c("x", "plan", ".plan", "data", "ard", "...")

.fields_split <- function(fn, it, formals, ard = character()) {
  fo <- setdiff(formals, .data_args)
  fl <- tryCatch(formals(utils::getFromNamespace(sub("^.*::", "", fn), "rtfreporter")),
                 error = function(e) NULL)
  req <- names(fl)[vapply(fl, function(d) is.symbol(d) && !nzchar(as.character(d)), NA)]
  a <- it$args %||% list()
  wr <- if (length(a)) tryCatch(tflspec::tfl_item_written(tflspec::tfl_item(fn, a)),
                                error = function(e) names(a)) else character()
  # (a verb's positional ... keyed by an argument -- plan_hide(cols) -- by its key too)
  up <- unique(c(req, wr, names(a)[nzchar(names(a))], ard))
  list(upper = fo[fo %in% up], lower = fo[!fo %in% up])
}

# A form card's body: the function's arguments as fields (each marked where
# its value came from), those its code writes above, the rest below
# ("Not in the code", folded: a value given there moves its field up).
# `choices`: the columns offered (the row layers, the ARD's keys).
.plan_form_body <- function(cid, fn, it, choices = NULL, lang = "en", cols = NULL,
                            ard = list(), fixed_link = NULL) {
  sch <- .plan_form_schema(fn)
  a <- it$args %||% list()
  sp <- .fields_split(fn, it, sch$formals, ard = names(Filter(length, ard)))
  field <- function(k, value) {
    from <- if (k %in% names(a)) .arg_from(it, k) else "own"
    own <- if (k %in% names(it$own_args %||% list())) "This table"
    # an argument the ARD decides (normalize_ard()'s hierarchy: step 1's
    # analysis): an ordinary field, "From the ARD" while it says the same
    # (blank: the ARD's), a warning when another value is given
    ak <- ard[[k]]
    if (length(ak)) {
      same <- is.null(value) || identical(as.character(unlist(value)), as.character(ak))
      f <- arg_field(cid, k, sch$args[[k]]$type, label = .arg_meaning(fn, k), value = value,
                     default = paste(ak, collapse = ", "), choices = unique(c(ak, choices)),
                     from = if (same) "ard" else from, own = if (!same) own,
                     help = .arg_help(fn, k, sch, lang), lang = lang)
      return(.ard_checked(f, ak, same, fixed_link, lang))
    }
    if (identical(sch$args[[k]]$type, "widths")) {
      return(.widths_field(cid, k, cols %||% .widths_cols(value, choices), value,
                           label = .arg_meaning(fn, k), from = from, own = own,
                           help = .arg_help(fn, k, sch, lang), lang = lang))
    }
    arg_field(cid, k, sch$args[[k]]$type, label = .arg_meaning(fn, k), value = value,
              default = sch$args[[k]]$default, choices = choices,
              from = from, own = own, help = .arg_help(fn, k, sch, lang), lang = lang)
  }
  shiny::div(
    class = "rp-plan-args", `data-card` = cid,
    shiny::div(class = "rp-args-up", `data-formals` = paste(sch$formals, collapse = ","),
               arg_group(NULL, lapply(sp$upper, function(k) field(k, a[[k]])), lang = lang)),
    arg_more(cid, fn, schema = sch, lang = lang, rest = sp$lower,
             fields = lapply(sp$lower, function(k) field(k, NULL)), label = "Not in the code"))
}

# The cards of a form of their own (table_plan()'s roles, plan_cells()'
# rows and formats, plan_col_header()'s lines, plan_sort()'s order): the
# arguments their form says; the function's other arguments are fields as
# a form card's, below the form (#417: every argument on the card)
.special_covered <- list(table_plan = c("cols", "rows", "label"),
                         plan_cells = c("value", "categorical", "continuous", "default"),
                         plan_col_header = c("header", "values"),
                         plan_sort = c("keys", "stat"))
.special_args <- function(fn) {
  setdiff(.plan_form_schema(fn)$formals, c(.data_args, .special_covered[[fn]]))
}
.special_more <- function(cid, fn, it, lang = "en") {
  ex <- .special_args(fn)
  if (!length(ex)) return(NULL)
  sch <- .plan_form_schema(fn)
  a <- it$args %||% list()
  up <- ex[ex %in% names(a)]
  lo <- setdiff(ex, up)
  field <- function(k, value) {
    arg_field(cid, k, sch$args[[k]]$type, label = .arg_meaning(fn, k), value = value,
              default = sch$args[[k]]$default,
              from = if (k %in% names(a)) .arg_from(it, k) else "own",
              own = if (k %in% names(it$own_args %||% list())) "This table",
              help = .arg_help(fn, k, sch, lang), lang = lang)
  }
  shiny::div(
    class = "rp-plan-args rp-special-more", `data-card` = cid,
    shiny::div(class = "rp-args-up", `data-formals` = paste(ex, collapse = ","),
               arg_group(NULL, lapply(up, function(k) field(k, a[[k]])), lang = lang)),
    arg_more(cid, fn, schema = sch, lang = lang, rest = lo,
             fields = lapply(lo, function(k) field(k, NULL)), label = "Not in the code"))
}
# ... read back: those fields' values (`set`) and the fields emptied, or a
# check as the function's default (`unset`); NULL while they are not there
.special_read <- function(input, cid, fn) {
  ex <- .special_args(fn)
  seen <- ex[vapply(ex, function(k) !is.null(.input_value(input, paste0(cid, "__", k))), NA)]
  if (!length(seen)) return(NULL)
  sch <- .plan_form_schema(fn)
  r <- builder_card_read(input, cid, sch, args = seen, logical_false = TRUE)
  out <- list(set = list(), unset = character())
  for (k in seen) {
    v <- r[[k]]
    d <- sch$args[[k]]$default
    empty <- is.null(v) || !length(v) || (length(v) == 1L && (is.na(v) || identical(v, ""))) ||
      (is.logical(v) && (if (is.null(d)) isFALSE(as.logical(v)) else
        identical(as.logical(v), as.logical(d))))
    if (empty) out$unset <- c(out$unset, k) else out$set[[k]] <- .form_list_value(v, sch$args[[k]]$type)
  }
  out
}
# ... put into the item's arguments as they are now
.special_args_put <- function(args, r) {
  args <- args %||% list()
  for (k in r$unset) args[[k]] <- NULL
  for (k in names(r$set)) args[[k]] <- r$set[[k]]
  args
}

# A field of a value the ARD decides: its ARD value on it (`data-ard`, the
# page compares as it is changed), and a warning -- not a stop -- shown when
# the field says another (`same` FALSE), with the way to step 1 (`link`)
.ard_checked <- function(field, ard, same, link = NULL, lang = "en") {
  field <- htmltools::tagAppendAttributes(field, `data-ard` = paste(ard, collapse = "|"))
  field <- htmltools::tagAppendChild(field, shiny::div(
    class = "small text-warning rp-ard-warn", style = if (same) "display: none;",
    sprintf(tr("Not the ARD analysis's (%s): the table may fail or come out otherwise. The analysis is changed in step 1.", lang),
            paste(ard, collapse = " > "))))
  htmltools::tagAppendChild(field, shiny::div(class = "small rp-ard-link", link))
}


# the form read back: the arguments its fields say (an unticked check
# FALSE); NULL while its fields are not there yet.  `cols`: the columns
# of a widths field, as drawn (.widths_cols()).
.plan_form_read <- function(input, cid, fn, cols = NULL) {
  sch <- .plan_form_schema(fn)
  wa <- names(Filter(function(x) identical(x$type, "widths"), sch$args))
  seen <- vapply(.plan_form_main[[fn]], function(k) {
    if (k %in% wa) {
      return(length(cols) > 0L && !is.null(.input_value(input, paste0(cid, "__", k, "__1"))))
    }
    !is.null(.input_value(input, paste0(cid, "__", k))) ||
      isTRUE(.input_value(input, paste0(cid, "__", k, "__isr")))
  }, NA)
  if (!any(seen)) return(NULL)
  out <- builder_card_read(input, cid, sch, args = setdiff(sch$formals, c(wa, sch$fixed)),
                           logical_false = TRUE)
  for (k in intersect(names(out), names(sch$args))) {
    out[[k]] <- .form_list_value(out[[k]], sch$args[[k]]$type)
    if (identical(sch$args[[k]]$type, "formula")) out[[k]] <- .formula_text(out[[k]])
  }
  for (k in wa) {
    if (isTRUE(.input_value(input, paste0(cid, "__", k, "__isr")))) {
      txt <- .input_value(input, paste0(cid, "__", k, "__r"))
      if (!is.null(txt) && nzchar(trimws(txt))) out[[k]] <- tflspec::tfl_r(trimws(txt))
      next
    }
    w <- .widths_read(input, paste0(cid, "__", k), cols)
    if (length(w)) out[[k]] <- w
  }
  out
}

# A condition typed as R for a one-sided formula's argument
# (plan_cell_style(where = )): its text without the "~" the code writes
# itself (`~ is.na(label)` typed: `is.na(label)`); blank: none
.formula_text <- function(v) {
  if (!inherits(v, "tfl_r")) return(v)
  txt <- trimws(sub("^[[:space:]]*~", "", as.character(unclass(v))))
  if (!nzchar(txt)) NULL else tflspec::tfl_r(txt)
}

# A list field's values as the argument takes them, by its type: whole
# numbers (column positions: plan_paginate_cols(at = c(2, 4))) as
# integers, a lone TRUE / FALSE (plan_row_group(collapse = TRUE)) a
# logical, the rest the columns' names
.form_list_value <- function(v, type) {
  if (!is.character(v) || !length(v) || is.null(type)) return(v)
  alts <- .type_split(type, "|")
  if ("logical" %in% alts && length(v) == 1L && v %in% c("TRUE", "FALSE")) return(as.logical(v))
  if ("list<integer>" %in% alts && all(grepl("^[0-9]+$", v))) return(as.integer(v))
  v
}

# ---- plan_columns(widths = ): a number a column ---------------------------------

# the columns a widths field offers: those the widths name, then the
# columns printed (the row label column, .values -- every value column --
# and the value columns by name)
.widths_cols <- function(widths = NULL, choices = NULL) {
  nm <- if (is.list(widths) || is.numeric(widths)) names(widths) else NULL
  unique(c(nm[!is.na(nm) & nzchar(nm)], as.character(unname(choices))))
}

# A widths field: a small table, a column a row and its relative width (a
# number; blank: rtfreporter's), its `R` switch as every field has
.widths_field <- function(id, arg, cols, value = NULL, label = NULL, from = "own", own = NULL,
                          help = NULL, lang = "en") {
  t <- function(x) tr(x, lang)
  fid <- paste0(id, "__", arg)
  is_r <- inherits(value, "tfl_r")
  r_text <- if (is_r) as.character(unclass(value)) else ""
  w <- if (!is_r && (is.list(value) || is.numeric(value))) value else list()
  rows <- lapply(seq_along(cols), function(i) {
    v <- if (!is.null(names(w)) && cols[[i]] %in% names(w)) w[[cols[[i]]]]
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$code(cols[[i]])),
      shiny::tags$td(shiny::tags$input(
        type = "number", id = paste0(fid, "__", i), class = "form-control form-control-sm rp-number",
        value = if (!is.null(v) && length(v) && !is.na(v[[1L]])) as.character(v[[1L]]),
        step = "any", min = "0", inputmode = "decimal", style = "width: 7em;")))
  })
  shiny::div(
    class = paste("rp-field rp-field-widths", if (is_r) "rp-field-r",
                  if (length(w) || nzchar(r_text)) "rp-has-value"),
    `data-arg` = arg, `data-field` = fid,
    shiny::tags$label(class = "rp-field-label",
                      builder_label(label, arg, "arg", lang),
                      from_badge(from, lang, own = own), help_tip(help)),
    shiny::div(class = "rp-field-row",
               shiny::div(class = "rp-form-input",
                          shiny::tags$table(class = "rp-widths",
                                            shiny::tags$thead(shiny::tags$tr(
                                              shiny::tags$th(t("Column")),
                                              shiny::tags$th(t("Relative width")))),
                                            shiny::tags$tbody(rows)),
                          shiny::div(class = "small text-muted",
                                     t("Blank: rtfreporter's width. .values: every value column."))),
               shiny::tags$input(type = "text", id = paste0(fid, "__r"),
                                 class = "form-control form-control-sm rp-mono rp-r-input",
                                 value = r_text, placeholder = "R", spellcheck = "false"),
               shiny::tags$label(class = "rp-r-toggle", title = t("As R"),
                                 shiny::tags$input(type = "checkbox", id = paste0(fid, "__isr"),
                                                   class = "rp-isr",
                                                   checked = if (is_r) NA else NULL),
                                 shiny::tags$span("R"))))
}

# ... read back: the columns given a number, in the field's order (a named
# list, as the sheets give widths); none: NULL
.widths_read <- function(input, fid, cols) {
  out <- list()
  for (i in seq_along(cols)) {
    v <- .input_value(input, paste0(fid, "__", i))
    if (is.null(v) || !length(v)) next
    n <- suppressWarnings(as.numeric(v[[1L]]))
    if (length(n) == 1L && !is.na(n)) out[[cols[[i]]]] <- n
  }
  if (length(out)) out else NULL
}

# two values the same (a number as a number: 30 and 30L; a check's FALSE
# and no value)
.same_value <- function(a, b, logical = FALSE) {
  if (logical) return(isTRUE(as.logical(a)) == isTRUE(as.logical(b)))
  if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
  if (is.numeric(a) && is.numeric(b)) return(isTRUE(all.equal(as.numeric(a), as.numeric(b))))
  identical(.arg_to_r(a), .arg_to_r(b))
}

# The item's whole arguments after the form's edit: its arguments (the
# report's over its layers') with what the form says otherwise put in, a
# field emptied taking its argument away (an argument the item does not
# say is the function's default: a check ticked as that default is no
# change).  .builder_item_write() keeps of them what differs from the layers.
.plan_form_args <- function(read, it, fn) {
  sch <- .plan_form_schema(fn)
  eff <- it$args %||% list()
  # (a widths field not drawn -- no columns known -- reads nothing: no change)
  wa <- names(Filter(function(x) identical(x$type, "widths"), sch$args))
  for (a in setdiff(union(names(read), setdiff(intersect(names(eff), sch$formals),
                                              setdiff(wa, names(read)))), sch$fixed)) {
    lg <- identical(.arg_kind(sch$args[[a]]$type), "logical") && !inherits(read[[a]], "tfl_r")
    new <- read[[a]]
    cur <- eff[[a]]
    if (is.null(cur) && lg) cur <- sch$args[[a]]$default
    if (.same_value(new, cur, lg)) next
    eff[a] <- list(new)
    if (is.null(new)) eff[[a]] <- NULL
  }
  eff
}

# ---- plan_digits(), plan_levels(), plan_filter() -----------------------------------

# plan_digits(rounding = ): the study's rounding (one for every report: the
# study sheet's), a choice of the schema's
.rounding_field <- function(cid, value = NULL, lang = "en") {
  ty <- .schema_of("plan_digits")$args$rounding$type %||% "enum[sas|r|iec60559]"
  v <- if (length(value) == 1L && !is.na(value) && nzchar(value)) value
  shiny::tagList(
    arg_field(cid, "rounding", ty, label = .arg_meaning("plan_digits", "rounding"), value = v,
              from = if (!is.null(v)) "defaults" else "own",
              help = tr(paste("How a 5 is rounded: sas half away from zero, r half to even,",
                              "iec60559 as the machine stores the number. The study's: every",
                              "report's (the Study tab's Rounding); blank there: options(rtfreporter.rounding)."),
                        lang),
              lang = lang),
    # (a change here is the study's: every report's -- a mark, its words
    # in its title)
    shiny::span(class = "rp-badge rp-badge-defaults rp-every-report",
                title = tr("Applies to every report (the study's defaults.yml)", lang),
                tr("every report", lang)))
}

# Each variable's empty values hidden (plan_levels(.drop_empty = ):
# variables$empty_levels "hide"): a named logical, by variable
.empty_levels_of <- function(vr) {
  if (!is.data.frame(vr) || !nrow(vr) || !"empty_levels" %in% names(vr)) return(list())
  vr <- vr[!is.na(vr$variable) & !duplicated(vr$variable), , drop = FALSE]
  stats::setNames(as.list(!is.na(vr$empty_levels) & tolower(vr$empty_levels) == "hide"),
                  vr$variable)
}

# The plan_levels() card's body: each categorical variable's values in
# their order, dragged (`id(paste0("lv", i))`, the variable's place in the
# builder's variables), and its empty values hidden or not (`"lvdrop"`).
# `vars`: a summary table's block of variables, its items in their order
# (the `variable` entry, dragged: `id("vars")`), first; `cols`: the column
# variables' levels in order (the columns' order, drawn by the page), next.
# `empty`: the variables sheet's rows (.empty_levels_of()).  Each variable
# is a line until opened (.levels_var()).
.levels_body <- function(id, st, codelists = NULL, empty = NULL, lang = "en",
                         vars = NULL, cols = NULL) {
  t <- function(x) tr(x, lang)
  v <- st$variables
  hidden <- .empty_levels_of(empty)
  rows <- lapply(seq_len(nrow(v)), function(i) {
    nm <- v$variable[i]
    lv <- st$levels[[nm]]
    if (!identical(v$kind[i], "categorical") || !length(lv)) return(NULL)
    .levels_var(
      id(paste0("lv", i)), nm, lv,
      sortable::rank_list(text = NULL, labels = .levels_with_labels(lv, nm, codelists),
                          input_id = id(paste0("lv", i)), orientation = "horizontal"),
      ard = st$level_ard[[nm]], from = st$level_from[[nm]], lang = lang,
      extra = shiny::tags$label(
        class = "small rp-levels-drop",
        shiny::tags$input(type = "checkbox", id = id(paste0("lvdrop", i)),
                          class = "form-check-input me-1",
                          checked = if (isTRUE(hidden[[nm]])) NA else NULL),
        with_tip(builder_label(.arg_meaning("plan_levels", ".drop_empty"), ".drop_empty", "arg", lang),
                 t("Ticked: the variable's values no subject has (a code list's value, 0 in every column) are not printed. Unticked: printed with their zeros."))))
  })
  rows <- Filter(Negate(is.null), rows)
  # (the code's order: variable, the column keys, then the categorical
  # variables)
  shiny::tagList(
    if (length(vars)) shiny::div(
      class = "rp-b-sec rp-levels-var rp-levels-block", `data-variable` = "variable",
      shiny::div(class = "rp-b-sec-label",
                 with_tip(builder_label("Analysis variables' order (the levels of variable)", "variable", "arg", lang),
                          t("The order of the analysis variables' blocks, dragged. A variable nested under another's level goes with it."))),
      .levels_state(.levels_block_order(st), intersect(st$variable_ard, .levels_block_order(st)),
                    nm = "variable", lang = lang),
      sortable::rank_list(text = NULL, labels = vars, input_id = id("vars"))),
    cols,
    if (length(rows)) rows else if (!length(vars)) shiny::div(
      class = "small text-muted", t("No categorical variable to order.")))
}

# the analysis variables in their blocks' order, those nested under
# another's level left out (plan_levels()' `variable` names none of them)
.levels_block_order <- function(st) {
  v <- st$variables
  if (!is.data.frame(v) || !nrow(v)) return(character())
  un <- v$under %||% rep(NA_character_, nrow(v))
  v$variable[is.na(un) | !nzchar(un)]
}

# One variable's values on plan_levels()' card (#428): closed, one line --
# "RACE: White, Black or African American, ... (6)  > Reorder" -- and
# open, every value a chip, dragged.  The chips stay drawn while closed,
# cut to no height: never display: none nor a closed <details> -- the
# sortable list reads its items' text, which a list not drawn has not (it
# wrote empty levels).  Under the line, whether the code writes the order
# (.levels_state()).  `input_id`: the list's; `extra`: drawn under the
# chips (the empty values' tick).
.levels_var <- function(input_id, nm, lv, list_ui, ard = NULL, from = NULL,
                        extra = NULL, lang = "en") {
  t <- function(x) tr(x, lang)
  shiny::div(
    class = "rp-b-sec rp-levels-var", `data-variable` = nm, `data-input` = input_id,
    shiny::tags$a(
      href = "#", class = "rp-levels-toggle", role = "button",
      shiny::tags$code(nm), ": ",
      shiny::span(class = "rp-levels-sum", .levels_summary(lv)), " ",
      shiny::span(class = "rp-levels-n", sprintf("(%d)", length(lv))), " ",
      shiny::span(class = "rp-levels-go rp-closed-only", paste("\u25b8", t("Reorder"))),
      shiny::span(class = "rp-levels-go rp-open-only", paste("\u25be", t("Close")))),
    .levels_state(lv, ard, from, nm = nm, lang = lang),
    shiny::div(class = "rp-levels-body", list_ui, extra))
}

# the closed line's values: the first ones up to about 40 characters, then
# "..." (the page writes it again as they are dragged: .builder_content_js)
.levels_summary <- function(lv, width = 40L) {
  lv <- as.character(lv)
  if (!length(lv)) return("")
  n <- cumsum(nchar(lv) + 2L)
  k <- max(1L, sum(n <= width))
  paste(c(lv[seq_len(k)], if (k < length(lv)) "\u2026"), collapse = ", ")
}

# Whether the code writes a variable's order: the ARD's own (`ard`: the
# order the ARD keeps the variable in, a factor -- the code list applied
# when it was made) is not written (tflspec #355); another is, with the
# way back to the ARD's ("Back to the ARD's order": the page sends
# b_lv_reset).  With no order of the ARD's, where it comes from
# (.level_order_text(): the code list's, or as first seen -- check it).
.levels_state <- function(lv, ard = NULL, from = NULL, nm = NULL, lang = "en") {
  t <- function(x) tr(x, lang)
  line <- function(cls, ...) shiny::div(class = paste("small rp-levels-state", cls), ...)
  if (length(ard)) {
    # (both lines drawn, one shown: the page shows the other as the values
    # are dragged, by the ARD's order it is given)
    same <- identical(as.character(lv), as.character(ard))
    hide <- "display: none;"
    return(shiny::div(
      class = "rp-levels-states",
      `data-ard` = jsonlite::toJSON(as.character(ard)),
      shiny::div(class = "small rp-levels-state text-muted rp-st-same", style = if (!same) hide,
                 t("The ARD's order (not written in the code)")),
      shiny::div(class = "small rp-levels-state text-muted rp-st-own", style = if (same) hide,
                 t("This table's order (written in the code)"), " ",
                 shiny::tags$a(href = "#", class = "rp-levels-reset", `data-variable` = nm,
                               t("Back to the ARD's order")))))
  }
  if (is.null(from)) return(NULL)
  src <- from$source %||% "seen"
  if (identical(src, "plan")) return(NULL)
  w <- .level_order_text(from, lang)
  if (identical(src, "seen")) return(line("text-warning", w$text))
  line(if (isTRUE(w$warn)) "text-warning" else "text-muted",
       paste0(t("Written in the code"), if (!is.null(w)) paste0(": ", w$text)))
}

# plan_filter()'s `where` as one condition's R (several joined by &, as
# the sheet keeps them); NULL for none
.filter_where_text <- function(where) {
  if (is.null(where) || !length(where)) return(NULL)
  one <- function(w) if (inherits(w, "tfl_r")) as.character(unclass(w)) else .arg_to_r(w)
  if (inherits(where, "tfl_r")) return(one(where))
  if (is.list(where)) {
    parts <- vapply(where, one, "")
    return(if (length(parts) == 1L) parts else paste0("(", parts, ")", collapse = " & "))
  }
  if (is.character(where)) return(paste(where, collapse = " & "))
  NULL
}

# The columns a row filter's condition reads, for the condition builder:
# the ARD's keys with their values, the variables and their values, and
# each statistic a number (typed in).  `meta`: ard_meta(); NULL: none.
.filter_data <- function(meta) {
  if (is.null(meta)) return(NULL)
  cols <- c(meta$keys %||% list(),
            list(variable = meta$variables$variable %||% character()))
  lv <- unlist(lapply(meta$variables$levels %||% character(), .split_list))
  if (length(lv)) cols$variable_level <- unique(lv)
  cols <- Filter(length, cols)
  n <- max(c(1L, lengths(cols)))
  d <- as.data.frame(lapply(cols, function(x) rep_len(as.character(x), n)),
                     stringsAsFactors = FALSE, check.names = FALSE)
  for (s in setdiff(meta$stats %||% character(), names(d))) d[[s]] <- rep(NA_real_, n)
  d
}

# ---- normalize_ard() ------------------------------------------------------------

# (normalize_ard()'s card is a form card like the others: .plan_form_body(),
# its hierarchy a field marked the ARD's (.ard_checked()) -- and its arguments written
# into the report's YAML)

# ---- the rows: the variables shown --------------------------------------------------

# A summary table's variables not shown: the row filter the builder writes
# for them (tables$keep_where `!variable %in% c("AGE", ...)`) read back.
# NULL when the row filter says something else (it is the user's, kept).
.hidden_vars_read <- function(where) {
  if (length(where) != 1L || is.na(where) || !nzchar(trimws(where))) return(character())
  e <- tryCatch(str2lang(where), error = function(e) NULL)
  ok <- is.call(e) && identical(e[[1L]], as.name("!")) && is.call(e[[2L]]) &&
    identical(e[[2L]][[1L]], as.name("%in%")) && identical(e[[2L]][[2L]], as.name("variable"))
  if (!ok) return(NULL)
  v <- tryCatch(eval(e[[2L]][[3L]], envir = baseenv()), error = function(e) NULL)
  if (!is.character(v)) NULL else v
}

# ... and written: NA for none
.hidden_vars_write <- function(hidden) {
  if (!length(hidden)) return(NA_character_)
  sprintf("!variable %%in%% %s", paste(deparse(as.character(hidden), width.cutoff = 500L),
                                       collapse = " "))
}

# ---- code line -> card -------------------------------------------------------------

# The lines of a report's program that each item wrote: the plan's pipe
# (`plan <- table_plan(...) |> plan_*() ...`), an item's lines from the one
# its verb starts to the next one's, and the code lists at its head
# (`cl_<variable> <- c(...)`, plan_levels()'s).  The k-th call of a verb is
# its k-th item.  A data frame: `line`, `path`.
builder_code_map <- function(lines, items) {
  lines <- unlist(strsplit(paste(lines, collapse = "\n"), "\n", fixed = TRUE))
  empty <- data.frame(line = integer(), path = character(), stringsAsFactors = FALSE)
  start <- grep("^plan <- ", lines)
  if (!length(start)) return(empty)
  start <- start[1L]
  paths <- vapply(items, function(it) it$path, "")
  fns <- vapply(items, function(it) it$fn, "")
  out_l <- integer()
  out_p <- character()
  seen <- character()
  cur <- NULL
  pending <- integer()
  for (i in start:length(lines)) {
    s <- lines[[i]]
    if (!nzchar(trimws(s))) break
    m <- regmatches(s, regexec("^(plan <- [A-Za-z_.]+ \\|> |plan <- |\\s+)([A-Za-z_.]+)\\(", s))[[1L]]
    fn <- if (length(m)) m[3L] else NA_character_
    if (!is.na(fn) && fn %in% fns) {
      seen <- c(seen, fn)
      k <- sum(seen == fn)
      at <- which(fns == fn)
      cur <- if (k <= length(at)) paths[at[k]] else NULL
      if (!is.null(cur) && length(pending)) {
        out_l <- c(out_l, pending)
        out_p <- c(out_p, rep(cur, length(pending)))
      }
      pending <- integer()
    } else if (grepl("^\\s*#", s)) {
      # a note on the item below it ("# (study default)")
      pending <- c(pending, i)
      next
    }
    if (!is.null(cur)) {
      out_l <- c(out_l, i)
      out_p <- c(out_p, cur)
    }
  }
  # the code lists above the plan: plan_levels()'s
  lev <- paths[fns == "plan_levels"]
  if (length(lev)) {
    cl <- grep("^# the code lists", lines)
    cl <- cl[cl < start]
    if (length(cl)) {
      at <- seq.int(cl[length(cl)], start - 1L)
      at <- at[nzchar(trimws(lines[at]))]
      out_l <- c(out_l, at)
      out_p <- c(out_p, rep(lev[[1L]], length(at)))
    }
  }
  # the ARD's normalization above the plan: normalize_ard()'s card
  if ("normalize_ard" %in% paths) {
    na <- grep("normalize_ard\\(", lines)
    na <- na[na < start]
    if (length(na)) {
      i <- na[length(na)]
      at <- i
      # (its call over several lines: to the line that closes it)
      while (i < start - 1L && !grepl("\\)\\s*$", lines[[i]])) {
        i <- i + 1L
        at <- c(at, i)
      }
      out_l <- c(out_l, at)
      out_p <- c(out_p, rep("normalize_ard", length(at)))
    }
  }
  o <- order(out_l)
  data.frame(line = out_l[o], path = out_p[o], stringsAsFactors = FALSE)
}

# plan_cells()' entries of a variable of its own (not the default, the
# continuous or categorical rows, the value printed, a variable's own
# decimals `skip`), as the card says them: "prob (3 rows), time"; NULL none
.cells_own_vars <- function(pc, skip = character()) {
  k <- setdiff(names(pc %||% list()),
               c("default", "continuous", "categorical", "value", "stats", "na", "notes", skip))
  if (!length(k)) return(NULL)
  vapply(k, function(v) {
    x <- pc[[v]]
    if (inherits(x, "tfl_call")) x <- x$args
    n <- if (is.list(x) || length(x) > 1L) length(x) else 1L
    if (n > 1L) sprintf("%s (%d)", v, n) else v
  }, "", USE.NAMES = FALSE)
}
.cells_own_note <- function(pc, skip = character(), t = function(x) x) {
  v <- .cells_own_vars(pc, skip)
  if (is.null(v)) return(NULL)
  shiny::div(class = "small text-muted rp-cells-own mt-1",
             t("Per variable (edit them as R on this card):"), " ",
             shiny::tags$code(paste(v, collapse = ", ")))
}

# ---- writing an item back ------------------------------------------------------------

# One item of a table written into the report's YAML: the `k`-th item of
# `fn` given these arguments -- its whole arguments, as the card says them
# (.spec_set_item(): the report's own layer holds what differs from the
# layers below).  plan_digits()' rounding is the study's, one for every
# report: it stays the layers', and a new one is the study's rounding.
.builder_item_write <- function(x, output_id, fn, args, k = 1L) {
  if (identical(fn, "plan_nest")) {
    its <- builder_items(x, output_id)
    x <- .spec_set_item(x, output_id, fn, args, k)
    return(.nest_order_kept(x, output_id, its))
  }
  rd <- if (identical(fn, "plan_digits")) args$rounding
  x <- .spec_set_item(x, output_id, fn, args, k,
                      below_args = if (identical(fn, "plan_digits")) "rounding" else character())
  if (!is.null(rd) && !inherits(rd, "tfl_r") &&
      !identical(as.character(rd), unname(x$study[["rounding"]] %||% NA_character_))) {
    x <- .spec_set_rounding(x, as.character(rd))
  }
  x
}

# ... and the report's own `k`-th item of `fn` taken away: its layers' apply
# again (none: the verb is gone from the plan)
.builder_item_revert <- function(x, output_id, fn, k = 1L) {
  if (!identical(fn, "plan_nest")) return(.spec_revert_item(x, output_id, fn, k))
  its <- builder_items(x, output_id)
  .nest_order_kept(.spec_revert_item(x, output_id, fn, k), output_id, its)
}

# A variable no longer nested (its plan_nest() entry gone: changed, the
# card off or removed, #428) keeps its place among the variables: it was
# shown after the level's variable (and that variable's other nested ones),
# and plan_levels()' order of the variables, which leaves a nested one out,
# takes it there -- not after the others, where a variable the order does
# not name goes.  `its`: the items before the change.
.nest_order_kept <- function(x, output_id, its) {
  now <- builder_items(x, output_id)
  lv <- .builder_args(now, "plan_levels")
  ord <- as.character(unlist(lv$variable))
  if (!length(ord)) return(x)
  was <- names(.builder_args(its, "plan_nest") %||% list())
  freed <- setdiff(setdiff(was, names(.builder_args(now, "plan_nest") %||% list())), ord)
  if (!length(freed)) return(x)
  vr <- .builder_vars_rows(its)
  o <- suppressWarnings(as.numeric(vr$order))
  seq_was <- vr$variable[order(is.na(o), o)]
  new <- seq_was[seq_was %in% c(ord, freed)]
  lv$variable <- c(new, setdiff(ord, new))
  .spec_set_item(x, output_id, "plan_levels", lv, 1L)
}

# ---- style -----------------------------------------------------------------------------

.builder_content_css <- "
.rp-args-view { display: grid; grid-template-columns: max-content minmax(0, 1fr);
  gap: .1rem .6rem; margin: 0; font-size: 12.5px; }
.rp-args-view dt { font-weight: normal; }
.rp-args-view dd { margin: 0; overflow-wrap: anywhere; }
.rp-args-view code { color: #374151; background: none; padding: 0; font-size: 12px; }
.rp-card-switch .form-check-input:disabled { cursor: default; }
@media (min-width: 992px) {
  /* SPEC | Code | Result beside the cards stays in view as they scroll: the
     column is the sticky box (the tabs inside it are not), its own scroll */
  .rp-b-right { position: sticky; top: calc(var(--rp-top, 0px) + .5rem);
    max-height: calc(100vh - var(--rp-top, 0px) - 1rem); overflow-y: auto; }
  .rp-b-right .rp-tabs { position: static; max-height: none; overflow-y: visible; }
}
.rp-back-code { padding: 0 .3rem; font-size: 12px; text-decoration: none; }
/* a card's rows of names the user gives (comparison columns, groups of
   statistics, nests): the name narrow, its value the rest of the row */
.rp-dots-table { width: 100%; font-size: 12.5px; margin-top: .2rem; }
.rp-dots-table th { font-weight: normal; color: #6b7280; font-size: 12px; padding: 0 .4rem .1rem 0; }
.rp-dots-table td { padding: .1rem .4rem .1rem 0; vertical-align: middle; }
.rp-dots-table td:first-child { width: 1%; white-space: nowrap; }
.rp-dots-table td .form-group, .rp-dots-table td .shiny-input-container { margin-bottom: 0; width: 100% !important; }
.rp-b-sec { margin-top: .5rem; }
.rp-b-levels { display: flex; flex-wrap: wrap; gap: .3rem; align-items: center; }
.rp-b-level { font-family: Consolas, 'Courier New', monospace; font-size: 12px; color: #374151;
  background: #f3f4f6; border: 1px dashed #cbd5e1; border-radius: .3rem; padding: 0 .4rem; }
.rp-b-fixed-note { margin-top: .15rem; }
.rp-b-sec > .rp-b-sec-label { font-size: 12.5px; margin-bottom: .15rem; }
/* the chips of an order (the columns' levels ...): each its whole text,
   onto the next line when narrow -- never cut */
.rp-builder-cards .default-sortable.horizontal .rank-list { flex-wrap: wrap; gap: .25rem; }
.rp-builder-cards .default-sortable.horizontal .rank-list-item { flex: 0 0 auto;
  overflow: visible; white-space: nowrap; max-width: 100%; }
/* plan_levels()' chips: small, a line or two (#417) */
.rp-builder-cards .rp-levels-var .rank-list-item { font-size: 12px; padding: .05rem .4rem;
  line-height: 1.4; min-height: 0; }
.rp-builder-cards .rp-levels-var .rank-list { gap: .2rem; padding: .2rem; }
/* plan_levels()' variables (#428): a line each until opened; the chips
   cut to no height, not hidden (the list reads their text) */
.rp-levels-toggle { color: inherit; text-decoration: none; font-size: 12.5px; display: inline; }
.rp-levels-toggle code { color: #111827; background: none; padding: 0; font-size: 12.5px;
  font-weight: 600; }
.rp-levels-go { color: var(--bs-link-color, #0d6efd); white-space: nowrap; }
.rp-levels-n { color: #6b7280; }
.rp-levels-body { max-height: 0; overflow: hidden; }
.rp-levels-var.rp-open > .rp-levels-body { max-height: none; overflow: visible; }
.rp-levels-var:not(.rp-open) .rp-open-only, .rp-levels-var.rp-open .rp-closed-only { display: none; }
.rp-levels-state { font-size: 12px; margin-left: .6rem; }
.rp-levels-reset { white-space: nowrap; }
"

# the cards' buttons of the table builder, one input each: Revert, Edit as
# R, Remove (the card's path)
.builder_content_js <- "
$(document).on('click', '.rp-builder-cards .rp-revert', function() {
  var p = $(this).closest('.rp-card').data('path');
  Shiny.setInputValue('b_card_revert', {path: String(p), t: Date.now()}, {priority: 'event'});
});
$(document).on('click', '.rp-builder-cards .rp-tool-r', function() {
  var p = $(this).closest('.rp-card').data('path');
  Shiny.setInputValue('b_card_r', {path: String(p), t: Date.now()}, {priority: 'event'});
});
$(document).on('click', '.rp-builder-cards .rp-tool-remove', function() {
  var p = $(this).closest('.rp-card').data('path');
  Shiny.setInputValue('b_card_remove', {path: String(p), t: Date.now()}, {priority: 'event'});
});
// plan_levels()' variables: opened and closed (kept as the card is drawn
// again), their line written again as the values are dragged, and the way
// back to the ARD's order
(function() {
  var open = {};
  function summary(lv) {
    var out = [], n = 0;
    for (var i = 0; i < lv.length; i++) {
      n += String(lv[i]).length + 2;
      if (i > 0 && n > 40) { out.push('\\u2026'); break; }
      out.push(lv[i]);
    }
    return out.join(', ');
  }
  $(document).on('click', '.rp-levels-toggle', function(e) {
    e.preventDefault();
    var v = $(this).closest('.rp-levels-var');
    v.toggleClass('rp-open');
    open[v.data('input')] = v.hasClass('rp-open');
  });
  $(document).on('shiny:value', function() {
    setTimeout(function() {
      $('.rp-levels-var[data-input]').each(function() {
        if (open[$(this).data('input')]) $(this).addClass('rp-open');
      });
    }, 0);
  });
  $(document).on('shiny:inputchanged', function(e) {
    if (!e.name || !Array.isArray(e.value)) return;
    var v = $('.rp-levels-var[data-input=\"' + e.name + '\"]');
    if (!v.length) return;
    v.find('.rp-levels-sum').first().text(summary(e.value));
    v.find('.rp-levels-n').first().text('(' + e.value.length + ')');
    var st = v.find('.rp-levels-states').first();
    if (st.length) {
      var ard = JSON.parse(st.attr('data-ard') || '[]');
      var same = ard.length === e.value.length &&
        ard.every(function(x, i) { return x === String(e.value[i]); });
      st.find('.rp-st-same').toggle(same);
      st.find('.rp-st-own').toggle(!same);
    }
  });
  $(document).on('click', '.rp-levels-reset', function(e) {
    e.preventDefault();
    Shiny.setInputValue('b_lv_reset', {variable: String($(this).data('variable')), t: Date.now()},
                        {priority: 'event'});
  });
})();
"

# The fixed items of a selectize of layers (a hierarchy's levels, the block
# of variables): neither its x nor Backspace removes them -- selectize's
# onDelete refuses -- and their x is not drawn (.fixed_items_css())
.fixed_items_js <- function(fixed) {
  I(sprintf(
    "function(values) { var fixed = %s; return !values.some(function(v) { return fixed.indexOf(v) >= 0; }); }",
    jsonlite::toJSON(as.character(fixed))))
}

# ... their look: no x, a fixed chip (by the input's id and their values)
.fixed_items_css <- function(id, fixed) {
  if (!length(fixed)) return(NULL)
  sel <- function(v, tail = "") sprintf(
    "#%s ~ .selectize-control .item[data-value=\"%s\"]%s", id, v, tail)
  shiny::tags$style(shiny::HTML(paste0(
    paste(vapply(fixed, sel, "", tail = " .remove"), collapse = ", "),
    " { display: none !important; } ",
    paste(vapply(fixed, sel, ""), collapse = ", "),
    " { background: #f3f4f6 !important; color: #374151 !important; border-style: dashed !important; padding-right: 6px !important; }")))
}

# The row layers' chips, named "name (meaning)": the block of the analysis
# variables in words, a hierarchy's last level marked the rows' text
# (label), its other levels, a stack's keys (`stack`), the other keys; a
# constant heading as it is
.layer_names <- function(layers, hierarchy = NULL, lang = "en", stack = character()) {
  t <- function(x) tr(x, lang)
  vapply(layers, function(k) {
    if (identical(k, ".variable")) return(paste0(t("Analysis variables"), " (variable)"))
    if (length(hierarchy) && identical(k, utils::tail(hierarchy, 1L))) return(paste0(k, " (label)"))
    if (.is_heading(k)) return(k)
    if (k %in% hierarchy) return(sprintf(t("%s (hierarchy)"), k))
    if (k %in% stack) return(sprintf(t("%s (stack)"), k))
    sprintf(t("%s (key)"), k)
  }, "", USE.NAMES = FALSE)
}

# the columns' chips: "TRT01A (column key)"
.col_key_names <- function(keys, lang = "en") {
  stats::setNames(keys, sprintf(tr("%s (column key)", lang), keys))
}

# A constant heading typed into the row layers ("Overall, typed with a
# quote first): its text in double quotes, as tables$rows writes it
# (group1 = "Overall")
.heading_create_js <- paste0(
  "function(input) { var v = String(input).replace(/^[\\s\"']+|[\\s\"']+$/g, '').replace(/\"/g, '');",
  " if (!v.length) return false; v = '\"' + v + '\"'; return {value: v, text: v}; }")

# the k-th call of its verb a card's path names ("plan_cell_style#2": 2)
.path_k <- function(path) {
  if (!is.null(path) && grepl("#", path, fixed = TRUE)) as.integer(sub("^.*#", "", path)) else 1L
}

# the item a card's path names (NULL for none)
.builder_item_at <- function(items, path) {
  if (is.null(path)) return(NULL)
  for (it in items) if (identical(it$path, as.character(path))) return(it)
  NULL
}
