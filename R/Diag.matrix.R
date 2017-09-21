
Diag.matrix <- function(len,v=0) {
	if(length(v)>1){
		.Call('Rfast_diag_matrix_fill_vec',PACKAGE = "Rfast",len,v)
	}else{
		.Call('Rfast_diag_matrix_fill_scalar',PACKAGE = "Rfast",len,v)
	}
}