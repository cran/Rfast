

mat.mat <- function(x, y) {
	.Call('Rfast_mat_mat', PACKAGE = 'Rfast',x,y)
}