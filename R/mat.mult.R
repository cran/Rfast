
mat.mult <- function(x,y) {
	.Call('Rfast_mat_mult_p',PACKAGE = "Rfast",t(x),y)
}