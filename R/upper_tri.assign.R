

upper_tri.assign <- function(x,v,diag = FALSE) {
	.Call('Rfast_upper_tri_assign', PACKAGE = 'Rfast',x,v,diag)
}