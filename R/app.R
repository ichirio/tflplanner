# The app.  One planner object in a reactiveValues is the whole state; every
# grid shows a slice of one sheet (the rows of the report chosen in the
# sidebar) and writes the slice back when it is edited.  A grid is redrawn
# only when the slice it shows changes from outside (another report chosen,
# a workbook opened, a report copied), never because it was edited itself.

.all_rows <- "__all__"
.default_rows <- "__default__"

# Values a column takes, offered as a dropdown (anything else may still be
# typed: rtfreporter checks it when the workbook is read).
.bool <- c("TRUE", "FALSE")
.choices <- list(
  tables = list(stats = c("cells", "rows"), value = c("stat", "stat_fmt")),
  layout = list(pages_split = c("group_safe", "group_force"),
                blank_where = c("between_groups"),
                group_page = .bool, group_show = .bool, blank_first = .bool,
                blank_last = .bool, blank_counted = .bool,
                stub_before = .bool),
  columns = list(row_title = .bool, decimal_split = .bool, hide = .bool),
  style = list(align_count_pct = .bool, auto_width = .bool),
  col_header = list(bold = .bool, align = c("left", "center", "right"),
                    span = c("each")),
  report = list(type = c("table", "listing", "figure"),
                auto_section = .bool, auto_title = .bool,
                page_header = .bool, page_footer = .bool),
  page = list(orientation = c("portrait", "landscape"),
              paper_size = c("letter", "A4")))

.sheet_labels <- c(
  tables = "tables \u8868", variables = "variables \u5909\u6570",
  cells = "cells \u30bb\u30eb", layout = "layout \u30da\u30fc\u30b8\u5272",
  columns = "columns \u5217", style = "style \u66f8\u5f0f",
  col_header = "col_header \u5217\u898b\u51fa\u3057",
  report = "report \u5e33\u7968", page = "page \u7528\u7d19",
  header = "header \u30d8\u30c3\u30c0", footer = "footer \u30d5\u30c3\u30bf",
  titles = "titles \u30bf\u30a4\u30c8\u30eb",
  footnotes = "footnotes \u811a\u6ce8")

#' Start the rtfplanner app
#'
#' Opens the editor for a study's `table_spec.xlsx` and `report_spec.xlsx`.
#' From it the workbooks, one R program per report and
#' `autoexec_report.R` are written to a folder (or downloaded as a zip).
#'
#' @param path Workbook(s) to open at start, e.g.
#'   `c("spec/table_spec.xlsx", "spec/report_spec.xlsx")`; `NULL` starts
#'   empty.
#' @param dir The folder "write" writes to; defaults to the folder of
#'   `path`, else the working directory.
#' @param ... Passed to [shiny::runApp()] (e.g. `launch.browser`, `port`).
#' @return `planner_app()` returns a [shiny::shinyApp()] object;
#'   `run_app()` runs it.
#' @examples
#' \dontrun{
#' run_app()
#' run_app(c("spec/table_spec.xlsx", "spec/report_spec.xlsx"))
#' }
#' @export
run_app <- function(path = NULL, dir = NULL, ...) {
  shiny::runApp(planner_app(path, dir), ...)
}

#' @rdname run_app
#' @export
planner_app <- function(path = NULL, dir = NULL) {
  start <- if (is.null(path)) new_planner() else read_planner(path)
  if (is.null(dir)) dir <- if (is.null(path)) getwd() else dirname(path[1L])
  shiny::shinyApp(app_ui(dir), function(input, output, session)
    app_server(input, output, session, start))
}

# ------------------------------------------------------------------- UI

.code_css <- "
.rp-code textarea, .rp-code pre { font-family: Consolas, 'Courier New',
  monospace; font-size: 12.5px; }
.rp-code pre { max-height: 520px; overflow: auto; white-space: pre; }
.rp-help { font-size: 12.5px; }
.handsontable td, .handsontable th { font-size: 12.5px; }
"

.sheet_panel <- function(sheet) {
  bslib::nav_panel(
    .sheet_labels[[sheet]],
    rhandsontable::rHandsontableOutput(paste0("hot_", sheet)),
    shiny::tags$details(
      class = "rp-help mt-2",
      shiny::tags$summary("\u5217\u306e\u8aac\u660e (_README)"),
      DT::DTOutput(paste0("help_", sheet))))
}

.grid_note <- shiny::p(
  class = "text-muted small mb-1",
  "\u884c\u306e\u8ffd\u52a0\u30fb\u524a\u9664\u306f\u53f3\u30af\u30ea\u30c3",
  "\u30af\u3002Excel \u304b\u3089\u306e\u8cbc\u308a\u4ed8\u3051\u53ef\u3002",
  "\u7a7a\u6b04 = \u672a\u6307\u5b9a\u3002",
  "1 \u30bb\u30eb\u306b\u8907\u6570\u306f | \u533a\u5207\u308a\u3002")

app_ui <- function(dir) {
  bslib::page_navbar(
    title = "rtfplanner",
    theme = bslib::bs_theme(version = 5, preset = "shiny"),
    header = shiny::tags$style(.code_css),
    sidebar = bslib::sidebar(
      width = 260,
      shiny::radioButtons(
        "target", "\u5bfe\u8c61\u306e\u5e33\u7968",
        choices = c("(\u5168\u884c)" = .all_rows)),
      shiny::p(class = "text-muted small",
               "\u5404\u30b7\u30fc\u30c8\u306f\u9078\u3093\u3060\u5e33\u7968",
               "\u306e\u884c\u3060\u3051\u3092\u8868\u793a\u30fb\u7de8\u96c6",
               "\u3057\u307e\u3059\u3002\u300c\u65e2\u5b9a\u300d\u306f",
               "output_id \u304c\u7a7a\u6b04\u306e\u884c\uff08\u8a66\u9a13",
               "\u5168\u4f53\u306e\u30c7\u30d5\u30a9\u30eb\u30c8\uff09\u3002")),

    bslib::nav_panel(
      "\u5e33\u7968\u4e00\u89a7",
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
        bslib::card(
          bslib::card_header("\u5e33\u7968"),
          DT::DTOutput("outputs"),
          shiny::div(
            class = "d-flex flex-wrap gap-1",
            shiny::actionButton("add", "\u8ffd\u52a0", class = "btn-sm"),
            shiny::actionButton("copy", "\u8907\u88fd", class = "btn-sm"),
            shiny::actionButton("rename", "\u540d\u524d\u5909\u66f4",
                                class = "btn-sm"),
            shiny::actionButton("remove", "\u524a\u9664",
                                class = "btn-sm btn-outline-danger"),
            shiny::actionButton("up", "\u2191", class = "btn-sm"),
            shiny::actionButton("down", "\u2193", class = "btn-sm")),
          shiny::p(class = "text-muted small mt-1",
                   "\u9806\u756a = autoexec_report.R \u306e\u5b9f\u884c",
                   "\u9806\u3002\u8907\u88fd\u306f\u5168\u30b7\u30fc\u30c8",
                   "\u306e\u884c\u3054\u3068\u30b3\u30d4\u30fc\u3057\u307e\u3059\u3002")),
        bslib::navset_card_tab(
          bslib::nav_panel(
            "\u30c7\u30fc\u30bf\u6e96\u5099\u30b3\u30fc\u30c9",
            shiny::uiOutput("current_label"),
            shiny::textInput("description", "\u8aac\u660e", width = "100%"),
            shiny::div(
              class = "rp-code",
              shiny::textAreaInput(
                "data_code",
                "\u3053\u306e\u5e33\u7968\u306e\u30c7\u30fc\u30bf\u6e96\u5099 (`data` \u3092\u4f5c\u308b\u3002\u56f3\u306f `content`\u3002\u7a7a\u6b04 = TODO)",
                rows = 12, width = "100%", resize = "vertical"),
              shiny::textAreaInput(
                "setup",
                "\u5168\u5e33\u7968\u5171\u901a\u306e\u524d\u51e6\u7406 (library(), ADaM \u306e\u8aad\u307f\u8fbc\u307f\u306a\u3069)",
                rows = 5, width = "100%", resize = "vertical"))),
          bslib::nav_panel(
            "\u751f\u6210\u3055\u308c\u308b\u30d7\u30ed\u30b0\u30e9\u30e0",
            shiny::div(class = "rp-code",
                       shiny::verbatimTextOutput("program")))))),

    bslib::nav_panel(
      "\u8868\u306e\u5b9a\u7fa9 (table_spec)",
      .grid_note,
      do.call(bslib::navset_card_underline,
              lapply(table_sheets(), .sheet_panel))),

    bslib::nav_panel(
      "\u5e33\u7968\u306e\u4f53\u88c1 (report_spec)",
      .grid_note,
      do.call(bslib::navset_card_underline,
              lapply(report_sheets(), .sheet_panel))),

    bslib::nav_panel(
      "\u8a66\u9a13",
      bslib::card(
        bslib::card_header("study \u30b7\u30fc\u30c8\uff08\u8a66\u9a13\u3067 1 \u3064\u306b\u6c7a\u307e\u308b\u5024\uff09"),
        shiny::selectInput(
          "rounding", "rounding \u4e38\u3081\u65b9",
          c("(options(rtfreporter.rounding) \u306b\u5f93\u3046)" = "",
            "r \uff08\u5076\u6570\u4e38\u3081\uff09" = "r",
            "sas \uff08\u56db\u6368\u4e94\u5165\uff09" = "sas")),
        shiny::textInput("output_path",
                         "output_path RTF \u306e\u51fa\u529b\u5148",
                         width = "100%"),
        shiny::textInput("program_dir",
                         "program_dir \u30d7\u30ed\u30b0\u30e9\u30e0\u306e\u7f6e\u304d\u5834 ({PROGRAM} \u7528)",
                         width = "100%"))),

    bslib::nav_panel(
      "\u30d5\u30a1\u30a4\u30eb",
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
        bslib::card(
          bslib::card_header("\u958b\u304f"),
          shiny::fileInput(
            "open", "table_spec.xlsx / report_spec.xlsx (\u8907\u6570\u53ef)",
            multiple = TRUE, accept = ".xlsx"),
          shiny::div(
            class = "d-flex gap-2",
            shiny::actionButton("new", "\u65b0\u898f"),
            shiny::actionButton(
              "sample",
              "rtfreporter \u306e\u30b5\u30f3\u30d7\u30eb (5 \u5e33\u7968)"))),
        bslib::card(
          bslib::card_header("\u66f8\u304d\u51fa\u3059"),
          shiny::textInput("dir", "\u51fa\u529b\u5148\u30d5\u30a9\u30eb\u30c0",
                           value = dir, width = "100%"),
          shiny::textInput(
            "program_out",
            "\u30d7\u30ed\u30b0\u30e9\u30e0\u306e\u30d5\u30a9\u30eb\u30c0 (\u7a7a\u6b04 = \u540c\u3058)",
            width = "100%"),
          shiny::checkboxInput(
            "overwrite",
            "\u65e2\u5b58\u306e\u5e33\u7968\u30d7\u30ed\u30b0\u30e9\u30e0\u3092\u4e0a\u66f8\u304d\u3059\u308b (\u624b\u3067\u76f4\u3057\u305f\u5206\u306f\u6d88\u3048\u307e\u3059)"),
          shiny::div(
            class = "d-flex gap-2",
            shiny::actionButton("check", "\u30c1\u30a7\u30c3\u30af"),
            shiny::actionButton("export", "\u66f8\u304d\u51fa\u3059",
                                class = "btn-primary"),
            shiny::downloadButton("zip", "zip \u3067\u30c0\u30a6\u30f3\u30ed\u30fc\u30c9")))),
      bslib::card(
        bslib::card_header("\u7d50\u679c"),
        DT::DTOutput("result"))))
}

# ---------------------------------------------------------------- server

.target_value <- function(target) {
  if (is.null(target) || identical(target, .all_rows)) return("")
  if (identical(target, .default_rows)) return(NA_character_)
  target
}

.na_blank <- function(d) {
  d[] <- lapply(d, function(v) ifelse(is.na(v), "", v))
  d
}

.grid <- function(d, sheet, key) {
  d <- .na_blank(d)
  if (!nrow(d)) d[1L, ] <- ""
  h <- rhandsontable::rhandsontable(
    d, rowHeaders = TRUE, useTypes = FALSE, stretchH = "all",
    height = 460, minSpareRows = 1L, planner_key = key)
  h <- rhandsontable::hot_context_menu(h, allowRowEdit = TRUE,
                                       allowColEdit = FALSE)
  for (cn in names(.choices[[sheet]])) {
    if (cn %in% names(d)) {
      h <- rhandsontable::hot_col(h, cn, type = "dropdown",
                                  source = c("", .choices[[sheet]][[cn]]),
                                  strict = FALSE)
    }
  }
  h
}

.help_table <- function(sheet) {
  rd <- .readme()
  if (is.null(rd)) return(NULL)
  rd <- rd[!is.na(rd$sheet) & rd$sheet == sheet, , drop = FALSE]
  rd$sheet <- NULL
  rd
}

app_server <- function(input, output, session, start) {
  # `want`: the report to select once the sidebar knows it
  rv <- shiny::reactiveValues(p = start, ver = 0L,
                              want = output_ids(start)[1L])
  bump <- function() rv$ver <- rv$ver + 1L
  notify <- function(msg, type = "message") {
    shiny::showNotification(msg, type = type, duration = 6)
  }
  guarded <- function(expr) {
    tryCatch(expr, error = function(e) {
      notify(conditionMessage(e), "error")
      NULL
    })
  }

  # -- the report chosen in the sidebar --------------------------------
  shiny::observe({
    rv$ver
    ids <- output_ids(shiny::isolate(rv$p))
    cur <- shiny::isolate(input$target)
    ch <- c(stats::setNames(.all_rows, "(\u5168\u884c)"),
            stats::setNames(.default_rows, "(\u65e2\u5b9a = \u7a7a\u6b04)"),
            stats::setNames(ids, ids))
    want <- shiny::isolate(rv$want)
    rv$want <- NULL
    sel <- if (length(want) && !is.na(want) && want %in% ch) want else
      if (!is.null(cur) && cur %in% ch) cur else
        if (length(ids)) ids[1L] else .all_rows
    shiny::updateRadioButtons(session, "target", choices = ch, selected = sel)
  })
  target <- shiny::reactive(.target_value(input$target))
  current <- shiny::reactive({
    t <- target()
    if (!is.na(t) && nzchar(t) && t %in% rv$p$outputs$output_id) t else NULL
  })

  # -- sheet grids -------------------------------------------------------
  for (sheet in c(table_sheets(), report_sheets())) local({
    sh <- sheet
    out_id <- paste0("hot_", sh)
    key <- shiny::reactive(paste(sh, input$target, rv$ver, sep = "|"))
    output[[out_id]] <- rhandsontable::renderRHandsontable({
      t <- target()
      d <- sheet_rows(shiny::isolate(rv$p), sh, t)
      if (!identical(t, "")) d$output_id <- NULL
      .grid(d, sh, key())
    })
    shiny::observeEvent(input[[out_id]], {
      h <- input[[out_id]]
      if (is.null(h$changes$changes) &&
          !h$changes$event %in% c("afterCreateRow", "afterRemoveRow")) return()
      if (!identical(h$params$planner_key, key())) return()
      d <- rhandsontable::hot_to_r(h)
      rv$p <- set_sheet_rows(rv$p, sh, target(), d)
    })
    output[[paste0("help_", sh)]] <- DT::renderDT(
      .help_table(sh), rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  # -- report list -------------------------------------------------------
  outputs_view <- shiny::reactive({
    p <- rv$p
    o <- p$outputs
    if (!nrow(o)) {
      return(data.frame(output_id = character(), type = character(),
                        program = character(), rtf = character(),
                        data = character(), description = character()))
    }
    info <- lapply(o$output_id, function(id) report_info(p, id))
    data.frame(
      output_id = o$output_id,
      type = vapply(info, `[[`, "", "type"),
      program = vapply(info, `[[`, "", "program"),
      rtf = vapply(info, `[[`, "", "file"),
      data = ifelse(is.na(o$data_code), "TODO", "\u2713"),
      description = ifelse(is.na(o$description), "", o$description),
      stringsAsFactors = FALSE)
  })
  output$outputs <- DT::renderDT({
    v <- outputs_view()
    sel <- match(shiny::isolate(input$target), v$output_id)
    DT::datatable(v, rownames = FALSE,
                  selection = list(mode = "single",
                                   selected = if (!is.na(sel)) sel),
                  options = list(dom = "t", paging = FALSE,
                                 ordering = FALSE, scrollX = TRUE))
  })
  shiny::observeEvent(input$target, {
    v <- outputs_view()
    sel <- match(input$target, v$output_id)
    if (!identical(input$outputs_rows_selected, if (!is.na(sel)) sel)) {
      DT::selectRows(DT::dataTableProxy("outputs"), if (!is.na(sel)) sel)
    }
  })
  shiny::observeEvent(input$outputs_rows_selected, {
    v <- shiny::isolate(outputs_view())
    id <- v$output_id[input$outputs_rows_selected]
    if (length(id) && !identical(id, input$target)) {
      shiny::updateRadioButtons(session, "target", selected = id)
    }
  })

  # detail editors follow the chosen report; editing writes it back
  shiny::observeEvent(list(current(), rv$ver), {
    id <- current()
    o <- rv$p$outputs[rv$p$outputs$output_id %in% id, , drop = FALSE]
    val <- function(v) if (length(v) && !is.na(v)) v else ""
    shiny::updateTextInput(session, "description", value = val(o$description))
    shiny::updateTextAreaInput(session, "data_code", value = val(o$data_code))
  })
  output$current_label <- shiny::renderUI({
    id <- current()
    if (is.null(id)) {
      shiny::p(class = "text-muted",
               "\u5de6\u306e\u4e00\u89a7\u304b\u30b5\u30a4\u30c9\u30d0\u30fc\u3067\u5e33\u7968\u3092\u9078\u3093\u3067\u304f\u3060\u3055\u3044\u3002")
    } else shiny::h5(id)
  })
  set_field <- function(field, value) {
    id <- current()
    if (is.null(id)) return()
    value <- if (is.null(value) || !nzchar(trimws(value))) NA_character_ else
      value
    i <- which(rv$p$outputs$output_id == id)
    if (!identical(rv$p$outputs[[field]][i], value)) {
      rv$p$outputs[[field]][i] <- value
    }
  }
  shiny::observeEvent(input$description,
                      set_field("description", input$description),
                      ignoreInit = TRUE)
  shiny::observeEvent(input$data_code, set_field("data_code", input$data_code),
                      ignoreInit = TRUE)

  shiny::observeEvent(rv$ver, {
    s <- rv$p$setup
    shiny::updateTextAreaInput(session, "setup",
                               value = if (is.na(s)) "" else s)
  })
  shiny::observeEvent(input$setup, {
    v <- if (nzchar(trimws(input$setup))) input$setup else NA_character_
    if (!identical(rv$p$setup, v)) rv$p$setup <- v
  }, ignoreInit = TRUE)

  output$program <- shiny::renderText({
    id <- current()
    if (is.null(id)) return("")
    paste(program_code(rv$p, id, spec_dir = input$dir), collapse = "\n")
  })

  ask_id <- function(title, button, value = "") {
    shiny::showModal(shiny::modalDialog(
      title = title,
      shiny::textInput("modal_id", "output_id", value = value),
      footer = shiny::tagList(shiny::modalButton("\u53d6\u6d88"),
                              shiny::actionButton(button, "OK",
                                                  class = "btn-primary")),
      easyClose = TRUE))
  }
  after_id_change <- function(p, id) {
    if (is.null(p)) return()
    rv$p <- p
    rv$want <- id
    shiny::removeModal()
    bump()
  }
  shiny::observeEvent(input$add, ask_id("\u5e33\u7968\u3092\u8ffd\u52a0",
                                        "add_ok"))
  shiny::observeEvent(input$add_ok, {
    id <- trimws(input$modal_id)
    after_id_change(guarded(add_output(rv$p, id)), id)
  })
  shiny::observeEvent(input$copy, {
    if (is.null(current())) return(notify("\u5e33\u7968\u3092\u9078\u3093\u3067\u304f\u3060\u3055\u3044", "warning"))
    ask_id(paste(current(), "\u3092\u8907\u88fd"), "copy_ok",
           paste0(current(), "_2"))
  })
  shiny::observeEvent(input$copy_ok, {
    id <- trimws(input$modal_id)
    after_id_change(guarded(copy_output(rv$p, current(), id)), id)
  })
  shiny::observeEvent(input$rename, {
    if (is.null(current())) return(notify("\u5e33\u7968\u3092\u9078\u3093\u3067\u304f\u3060\u3055\u3044", "warning"))
    ask_id(paste(current(), "\u306e\u540d\u524d\u3092\u5909\u66f4"),
           "rename_ok", current())
  })
  shiny::observeEvent(input$rename_ok, {
    id <- trimws(input$modal_id)
    after_id_change(guarded(rename_output(rv$p, current(), id)), id)
  })
  shiny::observeEvent(input$remove, {
    if (is.null(current())) return()
    shiny::showModal(shiny::modalDialog(
      title = paste(current(), "\u3092\u524a\u9664"),
      "\u5168\u30b7\u30fc\u30c8\u304b\u3089\u3053\u306e\u5e33\u7968\u306e\u884c\u3092\u524a\u9664\u3057\u307e\u3059\u3002",
      footer = shiny::tagList(
        shiny::modalButton("\u53d6\u6d88"),
        shiny::actionButton("remove_ok", "\u524a\u9664",
                            class = "btn-danger"))))
  })
  shiny::observeEvent(input$remove_ok, {
    rv$p <- remove_output(rv$p, current())
    shiny::removeModal()
    bump()
  })
  move <- function(by) {
    id <- current()
    if (is.null(id)) return()
    o <- rv$p$outputs
    i <- which(o$output_id == id)
    j <- i + by
    if (j < 1L || j > nrow(o)) return()
    o[c(i, j), ] <- o[c(j, i), ]
    rownames(o) <- NULL
    rv$p$outputs <- o
  }
  shiny::observeEvent(input$up, move(-1L))
  shiny::observeEvent(input$down, move(1L))

  # -- study -------------------------------------------------------------
  shiny::observeEvent(rv$ver, {
    st <- rv$p$study
    v <- function(k) if (is.na(st[[k]])) "" else st[[k]]
    shiny::updateSelectInput(session, "rounding", selected = v("rounding"))
    shiny::updateTextInput(session, "output_path", value = v("output_path"))
    shiny::updateTextInput(session, "program_dir", value = v("program_dir"))
  })
  for (k in c("rounding", "output_path", "program_dir")) local({
    key <- k
    shiny::observeEvent(input[[key]], {
      v <- trimws(input[[key]])
      v <- if (nzchar(v)) v else NA_character_
      if (!identical(unname(rv$p$study[[key]]), v)) rv$p$study[[key]] <- v
    }, ignoreInit = TRUE)
  })

  # -- files -------------------------------------------------------------
  load <- function(p) {
    if (is.null(p)) return()
    rv$p <- p
    rv$want <- output_ids(p)[1L]
    bump()
    notify(paste0(nrow(p$outputs), " \u5e33\u7968\u3092\u8aad\u307f\u8fbc\u307f\u307e\u3057\u305f"))
  }
  shiny::observeEvent(input$open, {
    f <- input$open
    # keep the file names: a message then names the workbook it is about
    tmp <- file.path(tempfile("open"), f$name)
    dir.create(dirname(tmp[1L]))
    file.copy(f$datapath, tmp)
    load(guarded(read_planner(tmp)))
  })
  shiny::observeEvent(input$new, load(new_planner()))
  shiny::observeEvent(input$sample, {
    d <- system.file("extdata", "ard-spec", package = "rtfreporter")
    load(guarded(read_planner(file.path(d, c("report.xlsx", "study.xlsx")))))
  })

  result <- shiny::reactiveVal(NULL)
  output$result <- DT::renderDT(result(), rownames = FALSE,
                                options = list(dom = "t", paging = FALSE))
  shiny::observeEvent(input$check, {
    r <- check_planner(rv$p)
    result(r)
    if (all(r$ok)) {
      notify("\u554f\u984c\u306f\u3042\u308a\u307e\u305b\u3093")
    } else {
      notify("\u30a8\u30e9\u30fc\u304c\u3042\u308a\u307e\u3059 (\u7d50\u679c\u3092\u53c2\u7167)", "error")
    }
  })
  shiny::observeEvent(input$export, {
    dir <- trimws(input$dir)
    pd <- trimws(input$program_out)
    r <- guarded(export_planner(rv$p, dir,
                                program_dir = if (nzchar(pd)) pd else dir,
                                overwrite_programs = isTRUE(input$overwrite)))
    if (!is.null(r)) {
      result(r)
      notify(sprintf("%d \u30d5\u30a1\u30a4\u30eb\u3092\u66f8\u304d\u51fa\u3057\u307e\u3057\u305f (%d \u4ef6\u306f\u65e2\u5b58\u306e\u307e\u307e)",
                     sum(r$status == "written"), sum(r$status == "kept")))
    }
  })
  output$zip <- shiny::downloadHandler(
    filename = function() "rtfplanner.zip",
    content = function(file) {
      d <- tempfile("zip")
      export_planner(rv$p, d, overwrite_programs = TRUE, portable = TRUE)
      zip::zipr(file, list.files(d, full.names = TRUE))
    })
}
