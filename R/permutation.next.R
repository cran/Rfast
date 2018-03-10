
permutation.next <- function(x,nperm=gamma(length(x)+1)) {
  .Call('Rfast_permutation_next', PACKAGE = 'Rfast',x,nperm)
}