
rowsums <- function(x,indices = NULL,parallel = FALSE) {
	if(parallel){
  		.Call('Rfast_row_sums_p', PACKAGE = 'Rfast',x)
  	}else{
  		.Call('Rfast_row_sums', PACKAGE = 'Rfast',x,indices)
  	}
}