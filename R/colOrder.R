
colOrder <- function(x,stable=FALSE,descending=FALSE) {
  .Call('Rfast_col_order', PACKAGE = 'Rfast',x,stable,descending)
}