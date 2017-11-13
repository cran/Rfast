

colAll <- function(x,parallel = FALSE) {
	if(parallel){
		.Call("Rfast_col_all_p",PACKAGE = "Rfast",x)
	}else{
		.Call("Rfast_col_all",PACKAGE = "Rfast",x)
	}
}