
colPmax <- function(x,y) {
  .Call('Rfast_col_pmax', PACKAGE = 'Rfast',x,y)
}