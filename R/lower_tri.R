

lower_tri <- function(x,suma = FALSE,diag = FALSE) {
	if(is.vector(x) && length(x)==2){
		.Call('Rfast_lower_tri_b', PACKAGE = 'Rfast',x[1L],x[2L],diag)
	}else if(suma){
		.Call('Rfast_sum_lower_tri', PACKAGE = 'Rfast',x,diag)
	}else{
		.Call('Rfast_lower_tri', PACKAGE = 'Rfast',x,diag)
	}
}