# Extracted from test-card-unset.R:170

# prequel ----------------------------------------------------------------------
U <- function() tflspec::tfl_unset()
.unset_prm <- function(listing_params) {
  prm <- listing_params
  p1 <- function(name, type, ...) structure(utils::modifyList(
    list(name = name, default = U(), required = FALSE, doc = name, type = type, multiple = FALSE),
    list(...)), class = "tfl_param")
  prm$by_break$default <- U()
  prm$by$default <- U()
  prm$txtx <- p1("txtx", "text")
  prm$numx <- p1("numx", "number")
  prm$flagx <- p1("flagx", "flag")
  prm$txty <- p1("txty", "text", default = "keep")
  prm
}
.unset_study <- function(id = "L-TPL") {
  home <- local_home(parent.frame())
  s <- suppressMessages(create_sample_study(run = FALSE, home = home))
  args <- list(data = "ADAE",
               columns = list(list(vars = "USUBJID", label = "Subject"),
                              list(vars = "AEDECOD", label = "Term", width = 30L)))
  o <- tflspec::tfl_output(id, "listing", label = "AE listing",
                           template = list(name = "listing", args = args))
  s$planner <- .spec_add_output(s$planner, o, label = "AE listing")
  suppressWarnings(suppressMessages(save_study(s, home = home)))
}
.tag_of <- function(html, id, tag) {
  regmatches(html, regexpr(sprintf('<%s[^>]*id="%s"[^>]*>', tag, id), html))
}
.select_of <- function(html, id) {
  regmatches(html, regexpr(sprintf('(?s)<select[^>]*id="%s".*?</select>', id), html, perl = TRUE))
}

# test -------------------------------------------------------------------------
skip_on_cran()
orig <- .tpl_params
prm <- .unset_prm(orig("listing"))
local_mocked_bindings(.tpl_params = function(name) prm, .package = "tflplanner")
local_home()
create_study("U1")
shiny::testServer(server_for("U1"), {
    session$setInputs(add = 1, modal_type = "listing", modal_how = "template", modal_tpl_listing = "listing",
                      modal_id = "L-AE", modal_desc = "AEs")
    # (the required ones, data and columns, are drawn with the dialog, apart from the others below:
    # a required field has no (none), as the wrapper's own test shows)
    # the others: the item and the button
    more <- as.character(output$tpl_modal_more$html)
    expect_match(.select_of(more, "mtpl_listing__by_break"), '<option value="(none)" selected>(none)</option>', fixed = TRUE)
    expect_match(more, "tplNone(&#39;mtpl_listing__txtx&#39;)", fixed = TRUE)
    expect_match(more, "tplNone(&#39;mtpl_listing__flagx&#39;)", fixed = TRUE)
    expect_false(grepl("mtpl_listing__data", more, fixed = TRUE))
    expect_false(grepl("mtpl_listing__columns", more, fixed = TRUE))
    # chosen in the dialog: written as !unset where the start was a value; a start that is unset is no change
    session$setInputs(mtpl_listing__data = "ADAE", mtpl_listing__columns = "USUBJID | Subject",
                      mtpl_listing__txty = "(none)", mtpl_listing__txtx = "(none)",
                      mtpl_listing__flagx__none = TRUE)
    args <- tpl_modal_args("listing")
    expect_s3_class(args$txty, "tfl_unset")
    expect_false("txtx" %in% names(args))
    expect_false("flagx" %in% names(args))
  })
