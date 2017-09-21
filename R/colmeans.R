
colmeans <- function(x,parallel = FALSE) {
	if(parallel){
		.Call('Rfast_col_mean_p', PACKAGE = 'Rfast',x)
	}else{
  		as.vector(.Call('Rfast_col_means', PACKAGE = 'Rfast',x))
  	}
}