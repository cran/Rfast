
sort_index <- function(x, descending = FALSE) {
  .Call('Rfast_sort_index', PACKAGE = 'Rfast',x,descending)
}