
sort_mat <- function(x,by.row=FALSE,descending=FALSE,stable=FALSE,parallel=FALSE) {
	if(by.row)
		.Deprecated("Rfast::rowSort")
	else
		.Deprecated("Rfast::colSort")

	if(parallel){
	  	if(stable){
			.Call(Rfast_stable_sort_mat_p,x,descending,by.row)
		}else{
			.Call(Rfast_sort_mat_p,x,descending,by.row)
		}
	}else{
		if(stable){
			.Call(Rfast_stable_sort_mat,x,descending,by.row)
		}else{
			.Call(Rfast_sort_mat,x,descending,by.row)
		}
	}
}