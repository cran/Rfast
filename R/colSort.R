
colSort <- function(x,descending=FALSE,stable=FALSE,parallel=FALSE) {
	if(parallel){
	  	if(stable){
			.Call(Rfast_stable_sort_mat_p,x,descending,FALSE)
		}else{
			.Call(Rfast_sort_mat_p,x,descending,FALSE)
		}
	}else{
		if(stable){
			.Call(Rfast_stable_sort_mat,x,descending,FALSE)
		}else{
			.Call(Rfast_sort_mat,x,descending,FALSE)
		}
	}
}