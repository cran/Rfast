
rowMins <- function(x,value=FALSE) {
	if(value){
		.Call(Rfast_row_min,x)
	}else{
		.Call(Rfast_row_min_indices,x)
	}
}