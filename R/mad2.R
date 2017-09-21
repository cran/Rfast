
mad2 <- function(x,method = "median") {
	.Call('Rfast_mad2', PACKAGE = 'Rfast',x,method)
}