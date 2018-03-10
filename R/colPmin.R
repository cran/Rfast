
colPmin <- function(x,y) {
  .Call('Rfast_col_pmin', PACKAGE = 'Rfast',x,y)
}