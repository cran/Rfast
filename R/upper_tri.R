
upper_tri <- function(x,suma = FALSE,diag = FALSE) {
  	if(is.vector(x) && length(x)==2){
		.Call(Rfast_upper_tri_b,x[1L],x[2L],diag)
	}else if(suma){
		.Call(Rfast_sum_upper_tri,x,diag)	
	}else{
		.Call(Rfast_upper_tri,x,diag)
	}
}