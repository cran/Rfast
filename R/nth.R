
nth <- function(x,k) {
  .Call('Rfast_nth', PACKAGE = 'Rfast',x,k)
}