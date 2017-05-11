
vecdist <- function(x) {
  	.Call('Rfast_vecdist', PACKAGE = 'Rfast',x)
}