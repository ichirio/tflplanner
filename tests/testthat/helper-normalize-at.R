# The line before a table's own normalization (`data <- ard |> ...`) in a
# written program, found by its content: lines the program writes before it
# (a comment, the headings' N taken apart from the table's rows) do not move
# what a test compares.
.normalize_at <- function(code) {
  i <- which(startsWith(code, "data <- ard |>"))[1L]
  if (is.na(i)) NA_integer_ else i - 1L
}
