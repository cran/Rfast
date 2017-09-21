

colMaxs <- function(x,value=FALSE,parallel = FALSE) {
	if(parallel){
		.Call('Rfast_col_max_p', PACKAGE = 'Rfast',x)	
	}else if(value){
		.Call('Rfast_col_max', PACKAGE = 'Rfast',x)
	}else{
    	.Call('Rfast_col_max_indices', PACKAGE = 'Rfast',x)
	}
}