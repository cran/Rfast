rowMads <- function(x,parallel = FALSE) {
	if(parallel){
		.Call(Rfast_row_mads_p,x)
	}else{
		.Call(Rfast_row_mads,x)
	}
}