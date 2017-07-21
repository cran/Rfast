

colMaxs <- function(x,value=FALSE) {
	if(value){
		return (.Call('Rfast_col_max', PACKAGE = 'Rfast',x))
	}
    .Call('Rfast_col_max_indices', PACKAGE = 'Rfast',x)
}