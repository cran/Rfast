
colnth <- function(x,elems) {
  .Call('Rfast_colnth', PACKAGE = 'Rfast',x,elems)
}