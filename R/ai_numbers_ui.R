# The AI drafts tab's step 1: the numbers in the shell sheets
# (tflspec::tfl_ai_prepare()$numbers, tflspec #497) as boxes -- ticked, a
# report the sheet's request makes; unticked, one it only refers to --
# changed here and read again (tfl_ai_prepare(numbers = )), as the
# sheets' roles are.  A TOC item with no shell can be put in a sheet's
# request from the TOC items tab too.

# The person's changes to the numbers: the rows given (sheet, number,
# make) -- a box set back to the rule's choice dropped from them
.ai_numbers_edit <- function(edits, sheet, number, make, rule = NA) {
  key <- paste(sheet, number, sep = "\r")
  keys <- if (is.null(edits)) character() else paste(edits$sheet, edits$number, sep = "\r")
  keep <- if (is.null(edits)) NULL else edits[keys != key, , drop = FALSE]
  if (!is.na(rule) && identical(as.logical(make), as.logical(rule))) return(keep)
  rbind(keep, data.frame(sheet = sheet, number = number, make = as.logical(make), stringsAsFactors = FALSE))
}

# The boxes' table: a row a number, its box, where its choice comes from
.ai_numbers_view <- function(n, ns, t) {
  src <- c(rule = t("rule"), guess = t("guess"), saved = t("saved"), given = t("given"))
  ck <- sprintf("<input type=\"checkbox\" class=\"ai-num-ck\" data-ns=\"%s\" data-sheet=\"%s\" data-number=\"%s\"%s>",
                ns(""), htmltools::htmlEscape(n$sheet, attribute = TRUE),
                htmltools::htmlEscape(n$number, attribute = TRUE), ifelse(n$make %in% TRUE, " checked", ""))
  by <- ifelse(is.na(src[n$source]), as.character(n$source), src[n$source])
  # (why a guess, as tflspec says it: beside the guess)
  if (!is.null(n$reason)) {
    why <- ifelse(n$source %in% "guess" & !is.na(n$reason) & nzchar(n$reason),
                  paste0(" (", n$reason, ")"), "")
    by <- paste0(by, why)
  }
  v <- data.frame(ck, n$sheet, n$number, ifelse(is.na(n$title), "", n$title),
                  ifelse(is.na(n$toc_id), "—", n$toc_id), by, stringsAsFactors = FALSE)
  names(v) <- c(t("Make"), t("sheet"), t("Number in the sheet"), t("Title in the sheet"),
                paste0(t("TOC item"), " (toc_id)"), t("Decided by"))
  v
}

#' @noRd
ai_numbers_ui <- function(ns, t) {
  # (in the normal flow, not filling the tab: the table above it fills)
  shiny::div(
    class = "flex-shrink-0 mt-2", style = "min-height: fit-content;",
    shiny::uiOutput(ns("numbers_head")),
    DT::DTOutput(ns("numbers_table"), height = "auto"),
    shiny::tags$script(shiny::HTML(
      "$(document).off('change.ainum').on('change.ainum', '.ai-num-ck', function() {
         Shiny.setInputValue(this.dataset.ns + 'num_toggle',
           {sheet: this.dataset.sheet, number: this.dataset.number, on: this.checked, n: Date.now()},
           {priority: 'event'});
       });")))
}

# its server part: `prepared()` the last read, `read_now()` reads again
# (with numbers_now() among its arguments)
#' @noRd
ai_numbers_server <- function(input, output, session, prepared, read_now, t) {
  ns <- session$ns
  # what the person changed, not yet applied; and what the reads are given
  pending <- shiny::reactiveVal(NULL)
  given <- shiny::reactiveVal(NULL)
  shiny::observeEvent(prepared(), pending(given()), ignoreNULL = FALSE)
  numbers <- shiny::reactive({
    p <- prepared()
    n <- if (!is.null(p)) p$numbers
    if (is.data.frame(n) && nrow(n)) n
  })
  rule_of <- function(sheet, number) {
    n <- numbers()
    k <- which(n$sheet == sheet & n$number == number)
    if (length(k) && !(n$source[k[1L]] %in% "given")) n$make[k[1L]] else NA
  }
  shiny::observeEvent(input$num_toggle, {
    x <- input$num_toggle
    pending(.ai_numbers_edit(pending(), x$sheet, x$number, isTRUE(x$on), rule_of(x$sheet, x$number)))
  })
  output$numbers_head <- shiny::renderUI({
    n <- numbers()
    if (is.null(n)) return(NULL)
    changed <- !identical(pending(), given())
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center small mt-3 mb-1",
      shiny::tags$b(class = "me-auto", with_tip(
        paste0(t("The numbers in the sheets"), " (numbers)"),
        t("Ticked: a report the sheet's request makes; unticked: one it only refers to. A TOC item with no shell whose number is in one sheet is ticked there as a guess. Change the boxes, then Apply and read again: the requests and the counts follow."))),
      sprintf(t("%d numbers, %d made"), nrow(n), sum(n$make %in% TRUE)),
      .btn(ns("numbers_apply"), t("Apply and read again"),
           class = paste("btn-sm", if (changed) "btn-primary" else "btn-outline-primary")),
      .btn(ns("numbers_rule"), t("Back to the rule")))
  })
  output$numbers_table <- DT::renderDT({
    n <- numbers()
    shiny::req(n)
    v <- .ai_numbers_view(n, ns, t)
    DT::datatable(v, rownames = FALSE, escape = -1L, selection = "none",
                  options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE))
  }, server = FALSE)
  shiny::observeEvent(input$numbers_apply, {
    given(pending())
    read_now()
  })
  # (every number the person gave, now or saved before, set back: make
  # NA, the rule's again)
  shiny::observeEvent(input$numbers_rule, {
    n <- numbers()
    mine <- if (!is.null(n)) n[n$source %in% c("given", "saved"), c("sheet", "number"), drop = FALSE]
    pending(NULL)
    given(if (!is.null(mine) && nrow(mine)) transform(mine, make = NA) else NULL)
    read_now()
  })
  # the TOC items tab: a TOC item with no shell put in a sheet's request
  output$toc_attach <- shiny::renderUI({
    p <- prepared()
    n <- numbers()
    if (is.null(p) || is.null(n) || is.null(p$toc)) return(NULL)
    no <- (p$toc$toc_id %||% p$toc$output_id)[!(p$toc$shell_found %in% TRUE)]
    if (!length(no)) return(NULL)
    sheets <- unique(n$sheet)
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-end small mt-2",
      shiny::selectInput(ns("attach_item"), t("A TOC item with no shell"), no, width = "14rem"),
      shiny::selectInput(ns("attach_sheet"), t("put in the request of the sheet"), sheets, width = "14rem"),
      shiny::div(class = "mb-3", .btn(ns("attach_ok"), t("Put it there and read again"))))
  })
  shiny::observeEvent(input$attach_ok, {
    item <- input$attach_item %||% ""
    sheet <- input$attach_sheet %||% ""
    if (!nzchar(item) || !nzchar(sheet)) return()
    g <- .ai_numbers_edit(pending(), sheet, item, TRUE)
    pending(g)
    given(g)
    read_now()
  })
  # what the reads are given (NULL: nothing)
  function() {
    g <- given()
    if (is.null(g) || !nrow(g)) NULL else g
  }
}
