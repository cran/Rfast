
rowMins <- function(x,value=FALSE) {
	.Call('Rfast_rowMins', PACKAGE = 'Rfast',x,value)
}