
Diag.matrix <- function(len,v=0) {
	.Call('Rfast_diag_matrix',len,v)
}