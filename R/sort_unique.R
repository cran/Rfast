
sort_unique <- function(x) {
  if(is.double(x)){
  	.Call(Rfast_sort_unique_double,x)
  }else{
  	.Call(Rfast_sort_unique_int,x)
  }
}