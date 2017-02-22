
colMins <- function(x,value=FALSE){
	if(value){
		return (.Call('Rfast_colmin', PACKAGE = 'Rfast',x))
	}
    .Call('Rfast_colmin_indices', PACKAGE = 'Rfast',x)
}