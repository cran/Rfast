
permutation.next <- function(x,all.next=TRUE) {
  .Call('Rfast_permutation_next', PACKAGE = 'Rfast',x,all.next,gamma(length(x)+1))
}