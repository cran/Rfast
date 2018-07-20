

colRanks <- function(x,method = "average",descending = FALSE,stable = FALSE,parallel = FALSE) {
	if(parallel){
  		.Call(Rfast_col_ranks_p,x,method,descending,stable)
	}else{
		.Call(Rfast_col_ranks,x,method,descending,stable)
	}
}