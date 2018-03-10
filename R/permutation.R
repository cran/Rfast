
permutation <- function(x,nperm=gamma(length(x)+1)) {
  .Call('Rfast_permutation', PACKAGE = 'Rfast',x,nperm)
}