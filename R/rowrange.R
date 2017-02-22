
rowrange <- function(x, cont = TRUE) {
  .Call('Rfast_rowrange', PACKAGE = 'Rfast',x,cont)
}