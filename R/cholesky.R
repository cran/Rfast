
cholesky <- function(x,parallel = FALSE) {
	if(parallel){
		.Call('Rfast_cholesky_par', PACKAGE = 'Rfast',x)
	}else{
  		.Call('Rfast_cholesky', PACKAGE = 'Rfast',x)
	}
}