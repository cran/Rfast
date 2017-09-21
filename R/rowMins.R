
rowMins <- function(x,value=FALSE) {
	if(value){
		.Call('Rfast_row_min', PACKAGE = 'Rfast',x)
	}else{
		.Call('Rfast_row_min_indices', PACKAGE = 'Rfast',x)
	}
}