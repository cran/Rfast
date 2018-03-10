
permutation.prev <- function(x,nperm=gamma(length(x)+1)) {
  .Call('Rfast_permutation_prev', PACKAGE = 'Rfast',x,nperm)
}