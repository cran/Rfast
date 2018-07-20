

colAll <- function(x,parallel = FALSE) {
	if(parallel){
		.Call(Rfast_col_all_p,x)
	}else{
		.Call(Rfast_col_all,x)
	}
}