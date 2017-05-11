
Sort <- function(x,descending=FALSE,partial=NULL,stable=FALSE) {
	if(stable){
		return(.Call('Rfast_stable_sort',x,descending))
	}
	else if(length(partial)==1){
	 	return(.Call('Rfast_partial_sort',x,partial,descending))
	}else if(is.character(x)){
		return(.Call('Rfast_sort_string',x,descending))
	}
	.Call('Rfast_Sort',x,descending)
}