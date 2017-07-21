
rowFalse <- function(x) {
  .Call('Rfast_row_false', PACKAGE = 'Rfast',x)
}