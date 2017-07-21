
Diag.fill <- function(x,v=0) {
	.Call('Rfast_diag_fill',x,v)
}