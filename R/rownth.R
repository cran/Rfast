
rownth <- function(x,elems,parallel = FALSE) {
	if(parallel){
		.Call('Rfast_row_nth_p', PACKAGE = 'Rfast',x,elems)		
	}else{
  		.Call('Rfast_row_nth', PACKAGE = 'Rfast',x,elems)
	}
}