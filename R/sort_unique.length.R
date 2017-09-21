
sort_unique.length <- function(x) {
  if(is.double(x)){
  	.Call('Rfast_len_sort_unique_double', PACKAGE = 'Rfast',x)
  }else{
  	.Call('Rfast_len_sort_unique_int', PACKAGE = 'Rfast',x)
  }
}