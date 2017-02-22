
permutation.prev <- function(x,all.prev=TRUE) {
  .Call('Rfast_permutation_prev', PACKAGE = 'Rfast',x,all.prev,gamma(length(x)+1))
}