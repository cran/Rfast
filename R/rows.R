
rows <- function(x,indices) {
	.Call(Rfast_rows,x,indices)
}