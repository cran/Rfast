

rowAll <- function(x,parallel = FALSE) {
	if(parallel){
		.Call("Rfast_row_all_p",PACKAGE = "Rfast",x)
	}else{
		.Call("Rfast_row_all",PACKAGE = "Rfast",x)
	}
}