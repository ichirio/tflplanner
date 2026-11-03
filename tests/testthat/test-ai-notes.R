# Step 3's ai: block (confirm first, then assumptions), step 4's reason,
# and the data.yml helpers of step 1 / the ARD preview error (#513, #511, #515)

test_that("the confirm items come first, in a marked block, the assumptions after", {
  cf <- data.frame(output_id = "t1", where = "content", item = "plan_cells",
                   why = "is the % of the arm?", stringsAsFactors = FALSE)
  asm <- list(list(where = "content", item = "widths", assumption = "default widths"))
  h <- as.character(.ai_notes_block(asm, cf, "t1", TRUE, "ns-", identity))
  expect_match(h, "ai-confirm-block")
  expect_match(h, "border-warning")
  expect_true(regexpr("ai-confirm-block", h) < regexpr("ai-assumption-block", h))
  expect_true(regexpr("To confirm", h) < regexpr("What the agent assumed", h))
  expect_match(h, "ai-open")
  h2 <- as.character(.ai_notes_block(asm, cf, "t1", FALSE, "ns-", identity))
  expect_no_match(h2, "ai-open")
  expect_match(h2, "Copy it into the study first")
})

test_that("no confirm items: only the assumptions, no empty block", {
  asm <- list(list(where = "content", item = "widths", assumption = "default widths"))
  h <- as.character(.ai_notes_block(asm, NULL, "t1", TRUE, "ns-", identity))
  expect_no_match(h, "ai-confirm-block")
  expect_match(h, "ai-assumption-block")
  h0 <- as.character(.ai_notes_block(NULL, cf <- data.frame(), "t1", TRUE, "ns-", identity))
  expect_match(h0, "Nothing assumed")
  expect_no_match(h0, "ai-confirm-block")
})
