
Sort <- function(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL) {
	if(stable){
		.Call('Rfast_stable_sort',PACKAGE = "Rfast",x,descending)
	}
	else if(!is.null(partial)){
	 	.Call('Rfast_partial_sort',PACKAGE = "Rfast",x,partial,descending)
	}else if(is.character(x)){
		.Call('Rfast_sort_string',PACKAGE = "Rfast",x,descending)
	}else{
		if(identical(na.last,TRUE)){
			.Call('Rfast_Sort_na_rm',PACKAGE = "Rfast",x,descending)
		}else{
			.Call('Rfast_Sort',PACKAGE = "Rfast",x,descending,na.last)
		}
	}
}