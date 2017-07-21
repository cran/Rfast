rowMads <- function(x) {
	.Call("Rfast_row_mads",PACKAGE = "Rfast",x)
}