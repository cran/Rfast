
colOrder <- function(x,stable=FALSE) {
  .Call('Rfast_col_order', PACKAGE = 'Rfast',x,stable)
}