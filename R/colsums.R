
colsums <- function(x,parallel = FALSE) {
	if(parallel){
		.Call('Rfast_col_sum_p', PACKAGE = 'Rfast',x)
	}else{
  		as.vector(.Call('Rfast_col_sums', PACKAGE = 'Rfast',x))
	}
}