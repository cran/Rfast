

rowAll <- function(x,parallel = FALSE) {
	if(parallel){
		.Call(Rfast_row_all_p,x)
	}else{
		.Call(Rfast_row_all,x)
	}
}