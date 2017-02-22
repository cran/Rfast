

colMaxs <- function(x,value=FALSE) {
	if(value){
		return (.Call('Rfast_colmax', PACKAGE = 'Rfast',x))
	}
    .Call('Rfast_colmax_indices', PACKAGE = 'Rfast',x)
}