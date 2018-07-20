
Order<-function(x,stable=FALSE,descending=FALSE,partial=NULL){
	if(is.character(x)){
		x <- as.numeric(x)
	}
	if(is.null(partial)){
		.Call(Rfast_Order,x,stable,descending)
	}else{
		.Call(Rfast_partial_sort_index,x,partial,descending)
	}
}