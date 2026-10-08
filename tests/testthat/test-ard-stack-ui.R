test_that("the ARD tab: the outline, a stack's form, one inside it, grouping and ungrouping", {
  skip_if_not_installed("cards")
  local_home()
  s <- create_study("ST")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  adsl <- cards::ADSL
  saveRDS(adsl, file.path(s$path, "data/adam/adsl.rds"))
  s$planner <- first_table(s$planner, "T1", "data/adam/adsl.rds", adsl,
                           "SAFFL", "TRT01A", c("AGE", "SEX"), description = "Demographics")
  s$planner <- first_table(s$planner, "T2", "data/adam/adsl.rds", adsl,
                           "SAFFL", "TRT01A", c("AGE", "SEX"), stack = FALSE)
  save_study(s)
  shiny::testServer(server_for("ST"), {
    rv <- session$userData$rv
    session$setInputs(nav = "make", step = "ard", target = "T1")
    # the outline: the stack, the two inside it
    h <- output$ard_outline$html
    expect_match(h, "STACK")
    expect_match(h, "ard_ol_move")
    expect_true(regexpr("STACK", h) < regexpr(">CONT<", h))
    # the stack's form
    session$setInputs(ard_ol_pick = "STACK")
    h <- output$ard_stat_ui$html
    expect_match(h, "runs the analyses inside it together", fixed = TRUE)
    expect_match(h, ".total_n", fixed = TRUE)
    expect_match(h, "ard_stack_add")
    # its switches written back as args: .total_n off, .missing on
    n <- session$userData$st_env$n
    do.call(session$setInputs, stats::setNames(
      list(FALSE, TRUE, TRUE),
      paste0("st", n, "_fl", c(".total_n", ".missing", ".by_stats"))))
    session$setInputs(ard_stat_apply = 1)
    expect_identical(ard_rows(rv$p, "analyses", "T1")$args[1], ".missing = TRUE")
    # one inside: no data of its own, the way out offered
    session$setInputs(ard_ol_pick = "CAT")
    h <- output$ard_stat_ui$html
    expect_match(h, "Inside STACK", fixed = TRUE)
    expect_match(h, "ard_stack_out")
    expect_false(grepl(sprintf("st%d_data", session$userData$st_env$n), h, fixed = TRUE))
    # a variable CONT has already: refused
    n <- session$userData$st_env$n
    do.call(session$setInputs, stats::setNames(list(c("SEX", "AGE")), paste0("st", n, "_vars")))
    session$setInputs(ard_stat_apply = 2)
    expect_identical(ard_rows(rv$p, "analyses", "T1")$variables[3], "SEX")
    # moved up: CAT before CONT
    session$setInputs(ard_ol_move = list(id = "CAT", by = -1))
    expect_identical(ard_rows(rv$p, "analyses", "T1")$analysis_id, c("STACK", "CAT", "CONT"))
    # one added inside
    session$setInputs(ard_ol_pick = "STACK")
    session$setInputs(ard_stack_add = 1)
    a <- ard_rows(rv$p, "analyses", "T1")
    expect_identical(a$parent[nrow(a)], "STACK")
    # ungrouped, keeping the N: GROUPN back
    session$setInputs(ard_ol_pick = "STACK")
    session$setInputs(ard_stack_ungroup = 1)
    session$setInputs(ard_stack_keep_n = TRUE, ard_stack_ungroup_ok = 1)
    a <- ard_rows(rv$p, "analyses", "T1")
    expect_false("STACK" %in% a$analysis_id)
    expect_true("GROUPN" %in% a$analysis_id)
    expect_identical(a$by[a$analysis_id == "CONT"], "TRT01A")
    # T2 one by one: grouped again from CONT's form, GROUPN left as it is
    session$setInputs(target = "T2")
    session$setInputs(ard_ol_pick = "CONT")
    expect_match(output$ard_stat_ui$html, "ard_stack_group")
    session$setInputs(ard_stack_group = 1)
    session$setInputs(ard_stack_pick = c("CONT", "CAT"), ard_stack_label = "Demog",
                      ard_stack_group_ok = 1)
    a <- ard_rows(rv$p, "analyses", "T2")
    expect_identical(a$analysis_id, c("GROUPN", "STACK", "CONT", "CAT"))
    expect_identical(a$args[a$analysis_id == "STACK"], ".by_stats = FALSE")
    # the stack's form says why it does not count the subjects per group;
    # ticked, it says they would be counted twice, and the outline marks it
    # (the new stack is the one open; a click on it again would close it)
    expect_match(output$ard_stat_ui$html, "GROUPN counts them now", fixed = TRUE)
    n <- session$userData$st_env$n
    do.call(session$setInputs, stats::setNames(list(TRUE), paste0("st", n, "_fl.by_stats")))
    expect_match(output$ard_stack_n_note$html, "so does GROUPN", fixed = TRUE)
    session$setInputs(ard_stat_apply = 3)
    expect_match(output$ard_outline$html, "so does GROUPN", fixed = TRUE)
    # GROUPN deleted from the note: the stack counts them, nothing twice
    session$setInputs(ard_stack_del_bign = 1)
    a <- ard_rows(rv$p, "analyses", "T2")
    expect_false("GROUPN" %in% a$analysis_id)
    expect_true(is.na(a$args[a$analysis_id == "STACK"]))
    expect_identical(nrow(stack_n_twice(a)), 0L)
    # the code of one inside: the stack's one call
    session$setInputs(ard_ol_pick = "CAT")
    expect_match(output$ard_an_code, "ard_stack(", fixed = TRUE)
    # the stack deleted, its analyses kept
    session$setInputs(ard_ol_pick = "STACK")
    session$setInputs(ard_stack_delete = 1)
    session$setInputs(ard_stack_delete_how = "ungroup", ard_stack_keep_n = FALSE,
                      ard_stack_delete_ok = 1)
    expect_identical(ard_rows(rv$p, "analyses", "T2")$analysis_id, c("CONT", "CAT"))
  })
})

test_that("the new-table dialog says when a subject has no group (then one by one)", {
  skip_if_not_installed("cards")
  local_home()
  s <- create_study("SM")
  dir.create(file.path(s$path, "data/adam"), recursive = TRUE, showWarnings = FALSE)
  adsl <- cards::ADSL
  adsl$TRT01A[1:3] <- NA
  saveRDS(adsl, file.path(s$path, "data/adam/adsl.rds"))
  shiny::testServer(server_for("SM"), {
    session$setInputs(mf_data = "data/adam/adsl.rds", mf_pop = "SAFFL",
                      mf_group = "TRT01A", mf_stack = TRUE)
    expect_match(output$mf_stack_note$html, "3 subjects of the analysis set have no TRT01A",
                 fixed = TRUE)
    session$setInputs(mf_group = "ARM")
    expect_match(output$mf_stack_note$html, "One call computes", fixed = TRUE)
  })
})
