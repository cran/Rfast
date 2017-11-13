
rowMedians <- function(x,parallel = FALSE) {
  	if(parallel){
		.Call('Rfast_row_meds_p', PACKAGE = 'Rfast',x)
	}else{
		.Call('Rfast_row_meds', PACKAGE = 'Rfast',x)
	}
}