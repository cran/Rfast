
colsums <- function(x) {
  	as.vector(.Call('Rfast_colsums', PACKAGE = 'Rfast',x))
}