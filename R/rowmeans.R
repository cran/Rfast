
rowmeans <- function(x) {
  	as.vector(.Call('Rfast_row_means', PACKAGE = 'Rfast',x))
}