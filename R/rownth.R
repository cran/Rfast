
rownth <- function(x,elems) {
  .Call('Rfast_row_nth', PACKAGE = 'Rfast',x,elems)
}