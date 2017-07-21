
rowMaxs <- function(x,value=FALSE) {
	if(value){
		.Call('Rfast_row_max', PACKAGE = 'Rfast',x)
	}
	.Call('Rfast_row_max_indices', PACKAGE = 'Rfast',x)
}