
colMedians <- function(x,parallel = FALSE) {
	if(parallel){
		.Call('Rfast_col_meds_p', PACKAGE = 'Rfast',x)
	}else{
		.Call('Rfast_col_meds', PACKAGE = 'Rfast',x)
	}
}