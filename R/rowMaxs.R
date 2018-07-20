
rowMaxs <- function(x,value=FALSE) {
	if(value){
		.Call(Rfast_row_max,x)
	}else{
		.Call(Rfast_row_max_indices,x)
	}
}