# How a field is drawn where its definition's input form is not drawn by
# arg_field() as it is: an `r` input on a type that is not R only is an R
# field showing the value as R source.  The rule is on the definition's
# (input, type), not on a function's name: nothing in tflplanner says how
# one function's field is drawn.

# an argument drawn by another type than its definition's: the type the
# field is drawn by, the definition's own kept as `def_type` (whether the
# field takes R is its say), no input form
.spec_drawn_as <- function(sch, a, type) {
  sch$args[[a]]["def_type"] <- list(sch$args[[a]]$type)
  sch$args[[a]]$type <- type
  sch$args[[a]]$input <- NULL
  sch
}

# The arguments whose input form is `r` (tflspec's: an R expression, canon
# 17dfbb8) drawn as R: an R-expression field, its R mark fixed.  A type
# that is R only is drawn so already; another (plan_cell_style(border = ),
# a call<rtf_border>) keeps its type as `def_type`, its value shown as R
# source and read back as the value it says (.spec_drawn_r_read()).
.spec_input_r <- function(sch) {
  for (a in names(sch$args)) {
    d <- sch$args[[a]]
    if (identical(.def_input(d), "r") && !.type_only_r(d$type)) sch <- .spec_drawn_as(sch, a, "r")
  }
  sch
}
