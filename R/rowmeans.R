
rowmeans <- function(x) {
  	as.vector(.Call('Rfast_rowmeans', PACKAGE = 'Rfast',x))
}