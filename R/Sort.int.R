
Sort.int <- function(x) {
  .Call('Rfast_sort_int',PACKAGE = "Rfast",x)
}