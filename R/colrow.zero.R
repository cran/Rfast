
colrow.zero <- function(x) {
	.Call("Rfast_col_row_zero", PACKAGE = "Rfast",x)
}