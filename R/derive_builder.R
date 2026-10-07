# The columns an analysis data makes (analysis_data$derive: `NAME = R | NAME
# = R`), made by kind in 2-1's form: split by conditions (ifelse() /
# dplyr::case_when()), cut a number into groups (cut()), days between two
# dates, or any R.  The sheet keeps the R as it is: the form reads back these
# four forms only, the rest is an R expression -- and a column not opened in
# the form keeps its text exactly as written (13-2: nothing the form cannot
# draw is changed by a save).

# -- reading and writing a column ------------------------------------------

# "NAME = expr" -> list(name, kind, text, ...): `text` the piece as written
.drv_read <- function(text) {
  text <- trimws(text)
  m <- regmatches(text, regexec("^([A-Za-z.][A-Za-z0-9._]*)\\s*=(?!=)\\s*(.*)$",
                                text, perl = TRUE))[[1L]]
  if (!length(m)) return(list(name = "", kind = "r", expr = text, text = text))
  expr <- trimws(m[3L])
  e <- tryCatch(str2lang(expr), error = function(err) NULL)
  x <- if (!is.null(e)) .drv_read_cond(e) %||% .drv_read_cut(e) %||% .drv_read_days(e)
  if (is.null(x)) x <- list(kind = "r", expr = expr)
  c(list(name = m[2L]), x, list(text = text))
}

# A derive's columns, in order
.drv_read_all <- function(derive) {
  if (.is_blank_v(derive %||% NA)[1L]) return(list())
  lapply(.split_bar(derive), .drv_read)
}

# The derive of the columns: each as written, unless the form made it again
.drv_text <- function(items) {
  if (!length(items)) return(NA_character_)
  paste(vapply(items, function(x) x$text, ""), collapse = " | ")
}

.drv_deparse <- function(e) paste(trimws(deparse(e, width.cutoff = 500L)), collapse = " ")

# a value of a branch as the field shows it: a text without its quotes (with
# them when it would read as a number or NA), a number as it is; NULL when it
# is not one value
.drv_value_text <- function(v) {
  if (is.character(v) && length(v) == 1L) {
    if (.drv_bare(v)) return(encodeString(v, quote = "\""))
    return(v)
  }
  if (is.numeric(v) && length(v) == 1L && !is.na(v)) return(.drv_deparse(v))
  if (identical(v, NA) || identical(v, quote(NA_character_)) ||
      identical(v, NA_character_)) return("NA")
  NULL
}

# a field's text that is written without quotes: a number, NA, or already
# quoted
.drv_bare <- function(x) {
  x <- trimws(x)
  grepl("^-?[0-9]+(\\.[0-9]+)?$", x) || x %in% c("NA", "") ||
    grepl("^\"(.*)\"$", x)
}

# a field's text as R
.drv_value_r <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return("NA")
  if (.drv_bare(x)) x else encodeString(x, quote = "\"")
}

.drv_is_call <- function(e, fns) {
  is.call(e) && .drv_deparse(e[[1L]]) %in% fns
}

# ifelse(c, a, ifelse(c2, b, d)) / dplyr::case_when(c ~ a, ..., TRUE ~ d)
.drv_read_cond <- function(e) {
  br <- list()
  els <- ""
  if (.drv_is_call(e, c("ifelse", "base::ifelse"))) {
    repeat {
      if (length(e) != 4L || !is.null(names(e)) && any(nzchar(names(e)))) return(NULL)
      v <- .drv_value_text(e[[3L]])
      if (is.null(v)) return(NULL)
      br[[length(br) + 1L]] <- list(cond = .drv_deparse(e[[2L]]), value = v)
      e <- e[[4L]]
      if (!.drv_is_call(e, c("ifelse", "base::ifelse"))) break
    }
    els <- .drv_value_text(e)
    if (is.null(els)) return(NULL)
  } else if (.drv_is_call(e, c("case_when", "dplyr::case_when"))) {
    a <- as.list(e)[-1L]
    nm <- names(a) %||% rep("", length(a))
    for (i in seq_along(a)) {
      if (identical(nm[i], ".default")) {
        els <- .drv_value_text(a[[i]])
        if (is.null(els)) return(NULL)
        next
      }
      f <- a[[i]]
      if (nzchar(nm[i]) || !.drv_is_call(f, "~") || length(f) != 3L) return(NULL)
      v <- .drv_value_text(f[[3L]])
      if (is.null(v)) return(NULL)
      if (isTRUE(f[[2L]]) && i == length(a)) {
        els <- v
      } else {
        br[[length(br) + 1L]] <- list(cond = .drv_deparse(f[[2L]]), value = v)
      }
    }
    if (!length(br)) return(NULL)
  } else {
    return(NULL)
  }
  list(kind = "cond", branches = br, otherwise = els)
}

# cut(VAR, c(-Inf, 65, 75, Inf), c("<65", "65-74", ">=75"), right = FALSE)
.drv_read_cut <- function(e) {
  if (!.drv_is_call(e, c("cut", "base::cut"))) return(NULL)
  m <- tryCatch(match.call(function(x, breaks, labels = NULL, right = TRUE) NULL, e),
                error = function(err) NULL)
  if (is.null(m) || !is.symbol(m$x) || !isFALSE(m$right)) return(NULL)
  b <- m$breaks
  if (!.drv_is_call(b, "c")) return(NULL)
  b <- as.list(b)[-1L]
  n <- length(b)
  if (n < 3L || !identical(b[[1L]], quote(-Inf)) || !identical(b[[n]], quote(Inf))) return(NULL)
  mid <- b[-c(1L, n)]
  if (!all(vapply(mid, function(v) is.numeric(v) && length(v) == 1L, NA))) return(NULL)
  labs <- ""
  if (!is.null(m$labels)) {
    l <- m$labels
    if (!.drv_is_call(l, "c")) return(NULL)
    l <- as.list(l)[-1L]
    if (length(l) != n - 1L || !all(vapply(l, is.character, NA))) return(NULL)
    labs <- paste(unlist(l), collapse = " | ")
  }
  list(kind = "cut", var = as.character(m$x),
       breaks = paste(vapply(mid, .drv_deparse, ""), collapse = ", "), labels = labs)
}

# as.numeric(END - START) (+ 1)
.drv_read_days <- function(e) {
  plus1 <- FALSE
  if (.drv_is_call(e, "+") && length(e) == 3L && identical(e[[3L]], 1)) {
    plus1 <- TRUE
    e <- e[[2L]]
  }
  if (!.drv_is_call(e, "as.numeric") || length(e) != 2L) return(NULL)
  d <- e[[2L]]
  if (!.drv_is_call(d, "-") || length(d) != 3L || !is.symbol(d[[2L]]) ||
      !is.symbol(d[[3L]])) return(NULL)
  list(kind = "days", end = as.character(d[[2L]]), start = as.character(d[[3L]]),
       plus1 = plus1)
}

# A column made by the form: its piece "NAME = expr" (an error, in words,
# when it cannot be)
.drv_write <- function(x) {
  name <- trimws(x$name %||% "")
  if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", name)) {
    stop("A column made needs a name (letters, digits, . and _).", call. = FALSE)
  }
  expr <- switch(x$kind,
    cond = {
      br <- Filter(function(b) !.is_blank_v(b$cond %||% NA)[1L], x$branches)
      if (!length(br)) stop("Give at least one condition.", call. = FALSE)
      els <- .drv_value_r(x$otherwise)
      if (length(br) == 1L) {
        sprintf("ifelse(%s, %s, %s)", br[[1L]]$cond, .drv_value_r(br[[1L]]$value), els)
      } else {
        paste0("dplyr::case_when(",
               paste(vapply(br, function(b) sprintf("%s ~ %s", b$cond, .drv_value_r(b$value)), ""),
                     collapse = ", "),
               ", TRUE ~ ", els, ")")
      }
    },
    cut = {
      if (.is_blank_v(x$var %||% NA)[1L]) stop("Choose the variable to cut.", call. = FALSE)
      b <- trimws(strsplit(x$breaks %||% "", ",", fixed = TRUE)[[1L]])
      b <- b[nzchar(b)]
      nb <- suppressWarnings(as.numeric(b))
      if (!length(b) || anyNA(nb)) stop("The cut points are numbers, comma between them (65, 75).", call. = FALSE)
      if (is.unsorted(nb, strictly = TRUE)) stop("The cut points go up (65, 75).", call. = FALSE)
      l <- trimws(strsplit(x$labels %||% "", "|", fixed = TRUE)[[1L]])
      l <- l[nzchar(l)]
      if (length(l) && length(l) != length(b) + 1L) {
        stop(sprintf("%d cut points make %d groups: give %d labels, or none.",
                     length(b), length(b) + 1L, length(b) + 1L), call. = FALSE)
      }
      sprintf("cut(%s, c(-Inf, %s, Inf)%s, right = FALSE)", x$var, paste(b, collapse = ", "),
              if (length(l)) paste0(", c(", paste(encodeString(l, quote = "\""), collapse = ", "), ")") else "")
    },
    days = {
      if (.is_blank_v(x$end %||% NA)[1L] || .is_blank_v(x$start %||% NA)[1L]) {
        stop("Choose the two dates.", call. = FALSE)
      }
      sprintf("as.numeric(%s - %s)%s", x$end, x$start, if (isTRUE(x$plus1)) " + 1" else "")
    },
    r = {
      if (.is_blank_v(x$expr %||% NA)[1L]) stop("Write the R that makes the column.", call. = FALSE)
      trimws(x$expr)
    })
  if (is.null(tryCatch(str2lang(expr), error = function(err) NULL))) {
    stop("This does not read as R: ", expr, call. = FALSE)
  }
  paste(name, "=", expr)
}

# -- the editor ------------------------------------------------------------

.drv_kind_labels <- c(cond = "Split by conditions", cut = "Cut a number into groups",
                      days = "Days between two dates", r = "R expression")

# The derive field of 2-1's form: the columns made, one a line, the one being
# made, and the sheet's text (to read or write as R)
.derive_editor_ui <- function(t, label, derive) {
  shiny::div(
    class = "mb-2",
    shiny::tags$label(class = "form-label mb-1", label),
    shiny::uiOutput("drv_list"),
    shiny::uiOutput("drv_editor"),
    shiny::tags$details(
      class = "small",
      shiny::tags$summary(t("As R (NAME = R, | between them)")),
      shiny::textInput("adata_derive", NULL, derive, width = "100%",
                       placeholder = sprintf(t("e.g. %s"), "PHASE = APHASE"))))
}

# Its server.  `cols()` the columns a column may be made of; `data()` the data
# (the condition builder's values).  Returns list(load(derive), text()).
.derive_editor_server <- function(input, output, session, t, notify, cols, data,
                                  lang = "en") {
  items <- shiny::reactiveVal(list())
  open <- shiny::reactiveVal(NULL)         # NULL closed, 0 a new one, i the i-th
  gen <- shiny::reactiveVal(0L)            # the fields' ids, new each time
  br <- shiny::reactiveVal(list())         # a split's branches being made
  br_sel <- shiny::reactiveVal(1L)
  br_ver <- shiny::reactiveVal(0L)
  cond_value <- shiny::reactiveVal(NA_character_)
  cond_key <- shiny::reactiveVal(0L)
  seen <- new.env()
  seen$v <- ""
  fid <- function(x) paste0("drv_", x, "_", gen())
  as_text <- function(v) if (is.na(v)) "" else v
  # for shiny::testServer(): the fields' ids, the condition as the builder
  # gives it
  session$userData$drv <- list(fid = fid, cond_value = cond_value, cond_key = cond_key,
                                text = function() .drv_text(items()))

  set_items <- function(v) {
    items(v)
    seen$v <- as_text(.drv_text(v))
    shiny::updateTextInput(session, "adata_derive", value = seen$v)
  }
  # the sheet's text, written in the field: read again
  shiny::observeEvent(input$adata_derive, {
    v <- trimws(input$adata_derive %||% "")
    if (identical(v, trimws(seen$v))) return()
    seen$v <- v
    items(.drv_read_all(if (nzchar(v)) v else NA))
    open(NULL)
  }, ignoreInit = TRUE)

  cond <- condition_builder_server("drv_cond", data = data, value = cond_value,
                                   lang = lang, key = cond_key)
  load_cond <- function(k) {
    b <- shiny::isolate(br())
    cond_value(if (k <= length(b)) b[[k]]$cond %||% NA_character_ else NA_character_)
    cond_key(shiny::isolate(cond_key()) + 1L)
  }
  shiny::observeEvent(cond(), {
    if (is.null(open())) return()
    b <- br()
    k <- br_sel()
    if (k > length(b)) return()
    v <- cond()$expr
    if (identical(b[[k]]$cond, v)) return()
    b[[k]]$cond <- v
    br(b)
    br_ver(br_ver() + 1L)
  })

  output$drv_list <- shiny::renderUI({
    x <- items()
    o <- open()
    rows <- lapply(seq_along(x), function(i) {
      shiny::div(
        class = paste("d-flex align-items-center gap-2 small border-bottom py-1",
                      if (identical(o, i)) "bg-light"),
        shiny::span(class = "badge text-bg-light border", t(.drv_kind_labels[[x[[i]]$kind]])),
        shiny::tags$code(class = "text-truncate flex-grow-1", title = x[[i]]$text, x[[i]]$text),
        shiny::tags$button(type = "button", class = "btn btn-sm btn-link py-0",
                           onclick = sprintf("Shiny.setInputValue('drv_edit', %d, {priority: 'event'})", i),
                           t("Change")),
        shiny::tags$button(type = "button", class = "btn btn-sm btn-link py-0 text-danger",
                           onclick = sprintf("Shiny.setInputValue('drv_del', %d, {priority: 'event'})", i),
                           "\u00d7"))
    })
    shiny::tagList(
      if (!length(x)) shiny::p(class = "small text-muted mb-1", t("None: the data's columns as they are.")),
      rows,
      if (is.null(o)) .btn("drv_new", t("+ Make a column"), class = "btn-sm btn-outline-secondary py-0 mt-1"))
  })

  start <- function(i) {
    x <- if (i > 0L) items()[[i]] else
      list(name = "", kind = "cond", branches = list(list(cond = NA_character_, value = "")),
           otherwise = "")
    gen(gen() + 1L)
    b <- x$branches %||% list(list(cond = NA_character_, value = ""))
    br(b)
    br_sel(1L)
    load_cond(1L)
    session$userData$drv_x <- x
    open(i)
  }
  shiny::observeEvent(input$drv_new, start(0L))
  shiny::observeEvent(input$drv_edit, start(as.integer(input$drv_edit)))
  shiny::observeEvent(input$drv_del, {
    v <- items()
    i <- as.integer(input$drv_del)
    if (i < 1L || i > length(v)) return()
    set_items(v[-i])
    open(NULL)
  })
  shiny::observeEvent(input$drv_cancel, open(NULL))

  output$drv_editor <- shiny::renderUI({
    o <- open()
    if (is.null(o)) return(NULL)
    x <- shiny::isolate(session$userData$drv_x)
    shiny::div(
      class = "border rounded p-2 mb-2 bg-light",
      bslib::layout_columns(
        col_widths = c(4, 8),
        shiny::textInput(fid("name"), t("Name of the column"), x$name, width = "100%"),
        shiny::radioButtons(fid("kind"), t("Made by"), inline = TRUE,
                            stats::setNames(names(.drv_kind_labels), t(unname(.drv_kind_labels))),
                            selected = x$kind)),
      shiny::uiOutput("drv_kind_ui"),
      shiny::div(
        class = "d-flex gap-2 mt-2",
        .btn("drv_ok", if (o > 0L) t("Change the column") else t("Add the column"),
             class = "btn-sm btn-primary"),
        .btn("drv_cancel", t("Cancel"), class = "btn-sm btn-outline-secondary")))
  })

  output$drv_kind_ui <- shiny::renderUI({
    shiny::req(!is.null(open()))
    kind <- input[[fid("kind")]] %||% shiny::isolate(session$userData$drv_x)$kind
    x <- shiny::isolate(session$userData$drv_x)
    if (!identical(x$kind, kind)) x <- list(kind = kind)
    cc <- cols()
    pick <- function(id, label, sel) shiny::selectizeInput(
      fid(id), label, unique(c(stats::setNames("", t("Variable")), sel[nzchar(sel %||% "")], cc)),
      selected = sel %||% "", width = "100%", options = list(create = TRUE))
    switch(kind,
      cond = shiny::tagList(
        shiny::uiOutput("drv_branches"),
        shiny::div(class = "small text-muted mt-1",
                   t("The condition of the one chosen (\u25c9):")),
        condition_builder_ui("drv_cond", lang)),
      cut = shiny::tagList(
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          pick("var", t("The number"), x$var),
          shiny::textInput(fid("breaks"), t("Cut at (comma between)"), x$breaks %||% "",
                           width = "100%", placeholder = "65, 75"),
          shiny::textInput(fid("labels"), t("Groups' labels (| between)"), x$labels %||% "",
                           width = "100%", placeholder = "<65 | 65-74 | >=75")),
        shiny::p(class = "small text-muted mb-0",
                 t("A cut point starts the group above it: 65 is in 65-74. Labels blank: R's own ([65,75) ...)."))),
      days = shiny::tagList(
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          pick("end", t("The later date"), x$end),
          pick("start", t("The earlier date"), x$start),
          shiny::checkboxInput(fid("plus1"), t("+ 1 (the first day is day 1)"), isTRUE(x$plus1)))),
      r = shiny::tagList(
        shiny::textInput(fid("expr"), t("R (the column's value)"), x$expr %||% "", width = "100%",
                         placeholder = "round(AVAL / BASE * 100, 1)"),
        shiny::div(style = "max-width: 320px",
                   shiny::selectizeInput(fid("insert"), NULL,
                                         c(stats::setNames("", t("Put in a variable...")), cc),
                                         width = "100%"))))
  })
  # a variable put in the R (each editor's field has its own id: observed
  # through the one now)
  shiny::observe({
    v <- input[[fid("insert")]] %||% ""
    if (!nzchar(v)) return()
    shiny::isolate({
      now <- input[[fid("expr")]] %||% ""
      shiny::updateTextInput(session, fid("expr"),
                             value = paste0(now, if (nzchar(now) && !grepl("[ (]$", now)) " ", v))
      shiny::updateSelectizeInput(session, fid("insert"), selected = "")
    })
  })

  # a split's branches: each a condition (made below, the one chosen) and
  # the value it gives
  val_id <- function(k) fid(paste0("val", k))
  output$drv_branches <- shiny::renderUI({
    br_ver()
    b <- br()
    sel <- br_sel()
    shiny::tagList(
      lapply(seq_along(b), function(k) {
        cnd <- b[[k]]$cond
        shiny::div(
          class = "d-flex align-items-center gap-2 small mb-1",
          shiny::tags$button(
            type = "button", class = paste("btn btn-sm py-0", if (k == sel) "btn-primary" else "btn-outline-secondary"),
            onclick = sprintf("Shiny.setInputValue('drv_br_pick', %d, {priority: 'event'})", k),
            if (k == sel) "\u25c9" else "\u25cb"),
          shiny::span(t("If")),
          shiny::tags$code(class = "text-truncate", style = "max-width: 45%",
                           if (.is_blank_v(cnd %||% NA)[1L]) t("(the condition, below)") else cnd),
          shiny::span("\u2192"),
          shiny::div(style = "width: 30%",
                     shiny::textInput(val_id(k), NULL, shiny::isolate(input[[val_id(k)]]) %||% b[[k]]$value,
                                      width = "100%", placeholder = t("Value"))),
          if (length(b) > 1L) shiny::tags$button(
            type = "button", class = "btn btn-sm btn-link py-0 text-danger",
            onclick = sprintf("Shiny.setInputValue('drv_br_del', %d, {priority: 'event'})", k), "\u00d7"))
      }),
      shiny::div(
        class = "d-flex align-items-center gap-2 small",
        .btn("drv_br_add", t("+ Another condition"), class = "btn-sm btn-link py-0"),
        shiny::span(t("Otherwise")), shiny::span("\u2192"),
        shiny::div(style = "width: 30%",
                   shiny::textInput(fid("else"), NULL,
                                    shiny::isolate(input[[fid("else")]]) %||%
                                      (shiny::isolate(session$userData$drv_x)$otherwise %||% ""),
                                    width = "100%", placeholder = "NA"))))
  })
  values_now <- function(b) {
    for (k in seq_along(b)) b[[k]]$value <- input[[val_id(k)]] %||% b[[k]]$value
    b
  }
  shiny::observeEvent(input$drv_br_pick, {
    k <- as.integer(input$drv_br_pick)
    br(values_now(br()))
    br_sel(k)
    load_cond(k)
    br_ver(br_ver() + 1L)
  })
  shiny::observeEvent(input$drv_br_add, {
    b <- values_now(br())
    b[[length(b) + 1L]] <- list(cond = NA_character_, value = "")
    br(b)
    br_sel(length(b))
    load_cond(length(b))
    br_ver(br_ver() + 1L)
  })
  shiny::observeEvent(input$drv_br_del, {
    k <- as.integer(input$drv_br_del)
    b <- values_now(br())
    if (length(b) < 2L || k > length(b)) return()
    b <- b[-k]
    # the values move up with their conditions
    for (j in seq_along(b)) shiny::updateTextInput(session, val_id(j), value = b[[j]]$value)
    br(b)
    br_sel(1L)
    load_cond(1L)
    br_ver(br_ver() + 1L)
  })

  shiny::observeEvent(input$drv_ok, {
    o <- open()
    if (is.null(o)) return()
    kind <- input[[fid("kind")]] %||% "cond"
    x <- list(name = input[[fid("name")]] %||% "", kind = kind)
    x <- switch(kind,
      cond = c(x, list(branches = values_now(br()), otherwise = input[[fid("else")]] %||% "")),
      cut = c(x, list(var = input[[fid("var")]], breaks = input[[fid("breaks")]],
                      labels = input[[fid("labels")]])),
      days = c(x, list(end = input[[fid("end")]], start = input[[fid("start")]],
                       plus1 = isTRUE(input[[fid("plus1")]]))),
      r = c(x, list(expr = input[[fid("expr")]])))
    text <- tryCatch(.drv_write(x), error = function(e) {
      notify(t(conditionMessage(e)), "warning")
      NULL
    })
    if (is.null(text)) return()
    x$text <- text
    v <- items()
    others <- vapply(v[setdiff(seq_along(v), o)], function(y) y$name, "")
    if (x$name %in% others) {
      return(notify(sprintf(t("%s is made already: change that one, or give another name."), x$name), "warning"))
    }
    if (o > 0L) v[[o]] <- x else v[[length(v) + 1L]] <- x
    set_items(v)
    open(NULL)
  })

  list(
    load = function(derive) {
      v <- .drv_read_all(derive)
      items(v)
      seen$v <- as_text(.drv_text(v))
      open(NULL)
    },
    text = function() .drv_text(items()))
}
