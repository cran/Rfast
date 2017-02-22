
is.symmetric <- function(x) {
  .Call('Rfast_symmetric', PACKAGE = 'Rfast',x)
}