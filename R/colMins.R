
colMins <- function(x,value=FALSE,parallel = FALSE) {
	if(parallel){
		.Call('Rfast_col_min_p', PACKAGE = 'Rfast',x)	
	}else if(value){
		.Call('Rfast_col_min', PACKAGE = 'Rfast',x)
	}else{
    	.Call('Rfast_col_min_indices', PACKAGE = 'Rfast',x)
	}
}