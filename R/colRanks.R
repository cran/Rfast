

colRanks <- function(x,method = "average",descending = FALSE,stable = FALSE,parallel = FALSE) {
	if(parallel){
  		.Call("Rfast_col_ranks_p",PACKAGE = "Rfast",x,method,descending,stable)
	}else{
		.Call("Rfast_col_ranks",PACKAGE = "Rfast",x,method,descending,stable)
	}
}