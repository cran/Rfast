
rowsums <- function(x,indices = NULL,parallel = FALSE) {
	if(parallel){
  		.Call(Rfast_row_sums_p,x)
  	}else{
  		.Call(Rfast_row_sums,x,indices)
  	}
}