
which_isFactor <- function(x) {
  .Call('Rfast_which_isFactor', PACKAGE = 'Rfast',x)
}