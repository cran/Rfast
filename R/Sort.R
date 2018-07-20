
Sort <- function(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL) {
	if(stable){
		.Call(Rfast_stable_sort,x,descending)
	}
	else if(!is.null(partial)){
	 	.Call(Rfast_partial_sort,x,partial,descending)
	}else if(is.character(x)){
		.Call(Rfast_sort_string,x,descending)
	}else{
		if(identical(na.last,FALSE)){
			.Call(Rfast_Sort_na_first,x,descending)
		}else{
			.Call(Rfast_Sort,x,descending,na.last)
		}
	}
}