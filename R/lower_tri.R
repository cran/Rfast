

lower_tri <- function(x,suma = FALSE) {
	if(is.vector(x) && length(x)==2){
		.Call('Rfast_lower_tri_b', PACKAGE = 'Rfast',x[1L],x[2L])
	}else if(suma){
		.Call('Rfast_sum_lower_tri', PACKAGE = 'Rfast',x)
	}else{
		.Call('Rfast_lower_tri', PACKAGE = 'Rfast',x)
	}
}