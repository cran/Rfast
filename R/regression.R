
regression <- function(x,y) {
  .Call('Rfast_regression', PACKAGE = 'Rfast',x,y)
}