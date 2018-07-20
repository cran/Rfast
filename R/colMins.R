
colMins <- function(x,value=FALSE,parallel = FALSE) {
	if(parallel){
		.Call(Rfast_col_min_p,x)	
	}else if(value){
		.Call(Rfast_col_min,x)
	}else{
    	.Call(Rfast_col_min_indices,x)
	}
}