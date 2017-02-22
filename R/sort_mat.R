
sort_mat <- function(x) {
  .Call('Rfast_sort_mat', PACKAGE = 'Rfast',x)
}