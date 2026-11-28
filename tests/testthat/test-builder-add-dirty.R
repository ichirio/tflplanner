# Adding a card is a change of the report (#827): the card is written to
# its SPEC at once, even with no argument (`plan_filter()`), the top bar
# says "Unsaved changes", Discard (or the card's Remove, then Undo) takes it
# back, Save writes it and it is there when the study is opened again.
# The rule is the same for every card "+ Add a card" offers.

# a study with one table and its ARD read (as the builder sees it)
add_dirty_study <- function(id) {
  ard <- cards::ard_stack(cards::ADSL, .by = TRT01A,
                          cards::ard_continuous(variables = AGE),
                          cards::ard_categorical(variables = SEX), .overall = TRUE)
  p <- add_standard_defaults(add_output(new_planner(), id, type = "table"), "BS")
  s <- create_study("BS", planner = p)
  data <- rtfreporter::normalize_ard(ard)
  m <- ard_meta(ard, data)
  f <- .meta_file(s, id)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(m, f)
  saveRDS(data, sub("[.]rds$", "_data.rds", f))
  s
}
add_dirty_open <- function(session, id) {
  session$setInputs(target = id, nav = "make", step = "content", content_nav = "content",
                    table_nav = "builder")
  bf <- session$userData$bform
  b <- function(x) paste0("b", bf$n, "_", x)
  v <- list()
  v[[b("key")]] <- "TRT01A"
  v[[b("vars")]] <- bf$st$variables$variable
  v[[b("value")]] <- "stat"
  v[[b("cat")]] <- bf$st$cat_format
  do.call(session$setInputs, v)
  session$elapse(1000)
}
# the verbs of the report's own content (its SPEC file's items), and of
# its content as the program has it (the study's defaults under it:
# plan_digits()' rounding is the study's)
own_fns <- function(p, id) .content_fns(.spec_output(p, id)$content)
item_fns <- function(p, id) vapply(builder_items(p, id), function(it) it$fn, "")

test_that("adding a card with no argument is an unsaved change; Discard takes it back (#827)", {
  skip_on_cran()
  local_home()
  add_dirty_study("T-AD")
  shiny::testServer(server_for("BS"), {
    add_dirty_open(session, "T-AD")
    rv <- session$userData$rv
    dirty <- session$userData$dirty
    # (the form's first edits written: the study saved as it is now)
    session$setInputs(save = 1)
    session$elapse(1000)
    expect_false(dirty())
    # every card "+ Add a card" offers: added, it is in the SPEC and the
    # study unsaved; Discard: the SPEC and the cards as saved
    addable <- .plan_addable(session$userData$bform$items)
    expect_true("plan_filter" %in% addable)
    for (fn in addable) {
      session$setInputs(b_add_pick = fn)
      session$elapse(1000)
      expect_true(fn %in% item_fns(rv$p, "T-AD"), info = fn)
      expect_true(dirty(), info = fn)
      expect_match(output$save_state$html, "Unsaved changes", fixed = TRUE, info = fn)
      session$setInputs(discard_ok = fn)
      session$elapse(1000)
      expect_false(dirty(), info = fn)
      expect_false(fn %in% item_fns(rv$p, "T-AD"), info = fn)
      expect_null(.builder_item_at(session$userData$bform$items, fn), label = fn)
    }
  })
})

test_that("an added empty card: Remove then Undo; Save writes it, there when opened again (#827)", {
  skip_on_cran()
  local_home()
  add_dirty_study("T-AD")
  shiny::testServer(server_for("BS"), {
    add_dirty_open(session, "T-AD")
    rv <- session$userData$rv
    dirty <- session$userData$dirty
    session$setInputs(save = 1)
    session$elapse(1000)
    expect_false(dirty())
    expect_false("plan_filter" %in% own_fns(rv$p, "T-AD"))
    # (the cards before; the form's first drawing is held -- no change till
    # an edit, #417 -- and written with the first one)
    before <- item_fns(rv$p, "T-AD")
    # added: plan_filter() of no argument, in the SPEC and the program
    session$setInputs(b_add_pick = "plan_filter")
    session$elapse(1000)
    expect_true(dirty())
    expect_true("plan_filter" %in% own_fns(rv$p, "T-AD"))
    cf <- .builder_item_at(session$userData$bform$items, "plan_filter")
    expect_true(isTRUE(cf$present))
    expect_length(cf$args %||% list(), 0L)
    # the card's Remove: the item taken away, the report's other cards
    # kept; its Undo puts the card back (unsaved)
    session$setInputs(b_card_remove = list(path = "plan_filter", t = 1))
    session$elapse(1000)
    expect_false("plan_filter" %in% own_fns(rv$p, "T-AD"))
    expect_true(all(before %in% item_fns(rv$p, "T-AD")))
    session$setInputs(b_card_undo = 1)
    session$elapse(1000)
    expect_true("plan_filter" %in% own_fns(rv$p, "T-AD"))
    expect_true(dirty())
    # Save: written; the study opened again has the card
    session$setInputs(save = 2)
    session$elapse(1000)
    expect_false(dirty())
  })
  again <- open_study("BS")$planner
  expect_true("plan_filter" %in% own_fns(again, "T-AD"))
  expect_true("plan_filter" %in% item_fns(again, "T-AD"))
  shiny::testServer(server_for("BS"), {
    add_dirty_open(session, "T-AD")
    expect_false(session$userData$dirty())
    cf <- .builder_item_at(session$userData$bform$items, "plan_filter")
    expect_false(is.null(cf))
    expect_true(isTRUE(cf$present))
  })
})
