
design_matrix <- function(x,ones=TRUE) {
	if(is.null(dim(x))){
		return(.Call('Rfast_design_matrix', PACKAGE = 'Rfast',x,ones))
	}
	.Call('Rfast_design_matrix_big', PACKAGE = 'Rfast',x)
}