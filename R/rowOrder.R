
rowOrder <- function(x,stable=FALSE,descending=FALSE) {
  .Call('Rfast_row_order', PACKAGE = 'Rfast',x,stable,descending)
}