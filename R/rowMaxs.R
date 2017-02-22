
rowMaxs <- function(x,value=FALSE) {
	.Call('Rfast_rowMaxs', PACKAGE = 'Rfast',x,value)
}