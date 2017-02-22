
sort_unique <- function(x) {
  if(is.double(x)){
  	return (.Call('Rfast_sort_unique_double', PACKAGE = 'Rfast',x))
  }
  .Call('Rfast_sort_unique_int', PACKAGE = 'Rfast',x)
}