
sort_mat <- function(x,by.row=FALSE,descending=FALSE,stable=FALSE) {
	if(stable){
		if(by.row){
			return(.Call('Rfast_stable_sort_row',x,descending))
		}
	  	return(.Call('Rfast_stable_sort_col',x,descending))
	}
	if(by.row){
		return(.Call('Rfast_sort_row',x,descending))
	}
  	.Call('Rfast_sort_col',x,descending)
}