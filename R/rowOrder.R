
rowOrder <- function(x,stable=FALSE) {
  .Call('Rfast_row_order', PACKAGE = 'Rfast',x,stable)
}