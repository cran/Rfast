colMads <- function(x) {
	.Call("Rfast_col_mads",PACKAGE = "Rfast",x)
}