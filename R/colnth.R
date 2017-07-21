
colnth <- function(x,elems) {
  .Call('Rfast_col_nth', PACKAGE = 'Rfast',x,elems)
}