

binary_search <- function(x, v, index=FALSE) {
	if(index){
  		return (.Call('Rfast_lowerbound', PACKAGE = 'Rfast',x,v))
	}
	.Call('Rfast_binarysearch', PACKAGE = 'Rfast',x,v)
}