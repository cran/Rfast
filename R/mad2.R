
mad2 <- function(x,method = "median",na.rm = FALSE) {
	.Call('Rfast_mad2', PACKAGE = 'Rfast',x,method,na.rm)
}