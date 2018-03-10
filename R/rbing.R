
rbing <- function(n,lam) {
	.Call('Rfast_rbing', PACKAGE = 'Rfast',n,lam)
}