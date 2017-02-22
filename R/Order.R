
Order <- function(x,stable=FALSE) {
  .Call('Rfast_Order', PACKAGE = 'Rfast',x,stable)
}