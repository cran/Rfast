
colmeans <- function(x) {
  	as.vector(.Call('Rfast_colmeans', PACKAGE = 'Rfast',x))
}