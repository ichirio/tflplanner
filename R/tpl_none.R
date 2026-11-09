# The template cards' "(none)" (`!unset`, tflspec::tfl_unset()): a template
# argument takes it as its default or as a report's value, and the field says
# so and sets it.  tpl_widget() (R/srv_add_report.R) draws the field as it
# always did, from a value that is NULL where the argument is unset; this
# puts "(none)" on it afterwards:
#   * a select / selectize: an item at the top, the selected one when unset;
#   * a text / number / list field: the value reads "(none)" when unset, and
#     the small "(none)" button under it writes it;
#   * a flag: the switch off with a "(none)" note, and the same button.
# Not on a required argument (it cannot be left out), nor on the columns
# field (its own table).  What an input then gives is read by .tpl_value().

.tpl_none_selects <- c("population", "data", "levels", "vars", "var", "chips", "choice", "choices")
.tpl_none_buttons <- c("text", "number", "flag", "list")

.tpl_none_wrap <- function(w, input_id, type, prm, none, t = identity) {
  if (isTRUE(prm$required)) return(w)
  if (type %in% .tpl_none_selects) {
    q <- htmltools::tagQuery(w)
    sel <- q$find("select")
    # (shiny writes a select's options as one piece of HTML: the item put
    # in front of it, the selected mark moved to it when unset)
    old <- paste(vapply(sel$selectedTags()[[1L]]$children, as.character, ""), collapse = "
")
    if (none) old <- gsub("(<option[^>]*?) selected(=\"[^\"]*\")?([ >])", "\\1\\3", old, perl = TRUE)
    item <- as.character(shiny::tags$option(value = .none_word, selected = if (none) NA, t(.none_word)))
    sel$empty()$append(shiny::HTML(paste(item, old, sep = "
")))
    return(q$allTags())
  }
  if (!type %in% .tpl_none_buttons) return(w)
  q <- htmltools::tagQuery(w)
  if (identical(type, "flag")) {
    q$addClass("tpl-flag-field")
  } else if (none) {
    if (identical(type, "list")) {
      q$find("textarea")$empty()$append(.none_word)
    } else {
      inp <- q$find("input")$removeAttrs(c("value", if (identical(type, "number")) "type"))
      inp$addAttrs(value = .none_word, type = if (identical(type, "number")) "text")
    }
  }
  row <- shiny::div(
    class = "none-row",
    .none_btn(t, onclick = sprintf("tplNone('%s')", input_id)),
    if (identical(type, "flag")) shiny::span(class = paste("none-note", if (none) "none-on"), .none_word))
  htmltools::tagList(q$allTags(), row)
}
