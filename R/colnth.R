
colnth <- function(x,elems,parallel = FALSE) {
	if(parallel){
  		.Call('Rfast_col_nth', PACKAGE = 'Rfast',x,elems)
	}else{
		.Call('Rfast_col_nth_p', PACKAGE = 'Rfast',x,elems)
	}
}