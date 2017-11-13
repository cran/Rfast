colMads <- function(x,parallel = FALSE) {
	if(parallel){
		.Call('Rfast_col_mads_p', PACKAGE = 'Rfast',x)
	}else{
		.Call('Rfast_col_mads', PACKAGE = 'Rfast',x)
	}
}