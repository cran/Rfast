

colAny <- function(x) {
  .Call("Rfast_col_any",PACKAGE = "Rfast",x)
}