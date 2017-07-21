
colMins <- function(x,value=FALSE){
	if(value){
		return (.Call('Rfast_col_min', PACKAGE = 'Rfast',x))
	}
    .Call('Rfast_col_min_indices', PACKAGE = 'Rfast',x)
}