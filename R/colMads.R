colMads <- function(x,parallel = FALSE) {
	if(parallel){
		.Call(Rfast_col_mads_p,x)
	}else{
		.Call(Rfast_col_mads,x)
	}
}