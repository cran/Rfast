
colmeans <- function(x,parallel = FALSE) {
	if(parallel){
		.Call(Rfast_col_mean_p,x)
	}else{
  		as.vector(.Call(Rfast_col_means,x))
  	}
}