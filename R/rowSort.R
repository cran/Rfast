
rowSort <- function(x,descending=FALSE,stable=FALSE,parallel=FALSE) {
	if(parallel){
	  	if(stable){
			.Call(Rfast_stable_sort_mat_p,x,descending,TRUE)
		}else{
			.Call(Rfast_sort_mat_p,x,descending,TRUE)
		}
	}else{
		if(stable){
			.Call(Rfast_stable_sort_mat,x,descending,TRUE)
		}else{
			.Call(Rfast_sort_mat,x,descending,TRUE)
		}
	}
}