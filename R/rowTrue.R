
rowTrue <- function(x) {
  .Call('Rfast_row_true', PACKAGE = 'Rfast',x)
}