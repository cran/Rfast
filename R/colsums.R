
colsums <- function(x,indices = NULL,parallel = FALSE) {
	if(parallel){
		.Call(Rfast_col_sums_p,x)
	}else{
  		.Call(Rfast_col_sums,x,indices)
	}
}