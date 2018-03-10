
colsums <- function(x,indices = NULL,parallel = FALSE) {
	if(parallel){
		.Call('Rfast_col_sums_p', PACKAGE = 'Rfast',x)
	}else{
  		.Call('Rfast_col_sums', PACKAGE = 'Rfast',x,indices)
	}
}