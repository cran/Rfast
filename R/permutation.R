
permutation <- function(x,all=TRUE) {
  .Call('Rfast_permutation', PACKAGE = 'Rfast',x,all,gamma(length(x)+1))
}