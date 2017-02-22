
cova <- function(x) {
	.Call('Rfast_cova', PACKAGE = 'Rfast',x)
}