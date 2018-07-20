

colMaxs <- function(x,value=FALSE,parallel = FALSE) {
	if(parallel){
		.Call(Rfast_col_max_p,x)	
	}else if(value){
		.Call(Rfast_col_max,x)
	}else{
    	.Call(Rfast_col_max_indices,x)
	}
}