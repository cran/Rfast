
coldiffs <- function(x) {
  .Call("Rfast_col_diffs", PACKAGE = "Rfast",x)
}