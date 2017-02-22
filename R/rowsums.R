
rowsums <- function(x) {
  	as.vector(.Call('Rfast_rowsums', PACKAGE = 'Rfast',x))
}