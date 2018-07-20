
mat.mult <- function(x,y) {
	.Call(Rfast_mat_mult_p,t(x),y)
}