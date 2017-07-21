

rowAny <- function(x) {
  .Call('Rfast_row_any',PACKAGE = "Rfast",x)
}