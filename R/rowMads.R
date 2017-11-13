rowMads <- function(x,parallel = FALSE) {
	if(parallel){
		.Call('Rfast_row_mads_p', PACKAGE = 'Rfast',x)
	}else{
		.Call('Rfast_row_mads', PACKAGE = 'Rfast',x)
	}
}