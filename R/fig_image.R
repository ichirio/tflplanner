# A figure's render settings -- its size on the page, alignment, dpi and
# PNG device -- are its content's last item, tfl_image (tflspec#874, canon
# dev-rep 3e2d421): the content step's card, made from tflspec's
# definitions (spec_card()).  `page:` keeps only what tables and figures
# share.  A setting the item does not give is the study's (study.yml
# `settings: figure: <setting>`), else the built-in one (the figure's own
# size, 300 dpi, centred).  A content of R as a whole has no item: drawn
# at its own size.
#
# The sheets' view keeps the size in the report sheet's figure_width_in /
# figure_height_in (the programs, the page's sample and the designer read
# it there; tflspec's bridge takes those cells back into the tfl_image):
# .fig_image_view() fills them from the spec.

# the item's name, and the card's input id
.fig_image_fn <- "tfl_image"
.fig_image_cid <- "fimg"

# is an item tfl_image?
.fig_is_image <- function(it) {
  is.list(it) && !inherits(it, "tfl_plot") && identical(as.character(it$fn %||% ""), .fig_image_fn)
}

# A report's own tfl_image arguments (its content's last item): a list,
# empty when it has none; NULL when its content is R of its own (no item)
.fig_image_own <- function(output) {
  cn <- output$content
  if (inherits(cn, "tfl_r")) return(NULL)
  n <- length(cn)
  if (!n || !.fig_is_image(cn[[n]])) return(list())
  a <- cn[[n]]$args %||% list()
  a[!vapply(a, is.null, NA)]
}

# the settings tfl_image takes (tflspec's schema)
.fig_image_keys <- c("width_in", "height_in", "align", "dpi", "png_device", "format", "file")

# The study's figure settings (study.yml settings: figure:), the ones
# tfl_image takes
.fig_image_study <- function(study) {
  f <- study$study$settings$figure
  if (!is.list(f)) return(list())
  f[intersect(names(f), .fig_image_keys)]
}

# A report's tfl_image as the program has it: its own over the study's
# (a planner with no spec, or a content of R: none)
.fig_image_now <- function(x, output_id) {
  o <- x$spec$outputs[[output_id]]
  if (is.null(o)) return(list())
  own <- .fig_image_own(o)
  if (is.null(own)) return(list())
  st <- .fig_image_study(x$spec$study)
  for (k in setdiff(names(st), names(own))) own[k] <- list(st[[k]])
  own
}

# The view's report rows with each figure's size from its tfl_image (its
# own over the study's): `p` the planner made from `study` and `outputs`
.fig_image_view <- function(p, study, outputs) {
  st <- .fig_image_study(study)
  for (id in names(outputs)) {
    o <- outputs[[id]]
    if (!identical(o$kind, "figure")) next
    img <- .fig_image_own(o)
    if (is.null(img)) next
    for (k in setdiff(names(st), names(img))) img[k] <- list(st[[k]])
    for (k in c("width_in", "height_in")) {
      v <- img[[k]]
      if (is.null(v) || inherits(v, "tfl_unset")) next
      p <- .set_row_value(p, "report", paste0("figure_", k), as.character(v)[1L], id)
    }
  }
  p
}

# A report's tfl_image set to `args` (its whole arguments; none: the item
# taken away), written into its file and its view made again
.fig_image_set <- function(p, output_id, args) {
  p <- .spec_ensure(p)
  p <- .spec_sync(p, p$spec$meta)
  attr(p, "changed") <- NULL
  o <- p$spec$outputs[[output_id]]
  if (is.null(o)) stop("The spec has no report ", output_id, ".", call. = FALSE)
  if (inherits(o$content, "tfl_r")) {
    stop("The figure ", output_id, " is R of its own: it is drawn at its own size ",
         "(its content has no tfl_image).", call. = FALSE)
  }
  args <- args[!vapply(args, function(v) is.null(v) || (is.atomic(v) && length(v) == 1L &&
                                                         (is.na(v) || identical(v, ""))), NA)]
  cn <- o$content %||% list()
  n <- length(cn)
  has <- n > 0L && .fig_is_image(cn[[n]])
  if (!length(args)) {
    if (has) cn[[n]] <- NULL
  } else if (has) {
    cn[[n]]$args <- args
  } else {
    cn[[n + 1L]] <- tflspec::tfl_item(.fig_image_fn, args)
  }
  o["content"] <- list(if (length(cn)) cn)
  .spec_set_output(p, output_id, o)
}

# The content step's card of a figure report: its tfl_image's fields
# (tflspec's definitions), its size on the page in words, and the buttons
# that fit it to the page's body or give it back its own size
.fig_image_card_ui <- function(p, output_id, t, lang = "en") {
  head <- shiny::h6(class = "mb-1", t("The figure on the page"), " ",
                    shiny::span(class = "small text-muted", "(tfl_image)"))
  wrap <- function(...) shiny::div(class = "rp-fig-image border rounded p-2 mb-2", head, ...)
  sp <- .pgs_spec(p)
  o <- sp$outputs[[output_id]]
  if (is.null(o)) {
    return(wrap(shiny::p(class = "small text-muted mb-0", t("(no YAML spec)"))))
  }
  own <- .fig_image_own(o)
  if (is.null(own)) {
    return(wrap(shiny::p(class = "small text-muted mb-0",
                         t("A figure of your own R code is drawn at its own size: its content has no tfl_image."))))
  }
  # (the report's own: each value marked so)
  item <- list(fn = .fig_image_fn, args = own, own_args = own)
  sz <- tryCatch(.fig_size_of(p, output_id), error = function(e) NULL)
  native <- tryCatch(.fig_native_size(p, output_id), error = function(e) c(6.5, 4.5))
  said <- vapply(c("width_in", "height_in"), function(k) tolower(as.character(own[[k]] %||% "")), "")
  wrap(
    spec_card(.fig_image_cid, .fig_image_fn, item, lang = lang),
    shiny::div(
      class = "d-flex flex-wrap gap-2 align-items-center small mt-1",
      if (!is.null(sz)) shiny::span(class = "text-muted",
                                    sprintf(t("On the page: %s"), .fig_size_words(sz, t))),
      shiny::span(class = "text-muted", sprintf(t("(its own: %s)"),
                                                paste(format(native), collapse = " x "))),
      if (!all(said == "fit")) shiny::actionLink("fimg_fit", t("Fit to the page")),
      if (any(nzchar(said))) shiny::actionLink("fimg_native", t("Its own size")),
      help_tip(t("Width and height: blank, the study's (study.yml settings: figure:), else the figure's own size (the company's figure style for its kind), as rtfreporter places an image. A number: that size in inches. fit: the page's body (its width; its height less the titles and footnotes). The figure is drawn at this size, its text as its theme says; the dpi changes only its pixels."))))
}
