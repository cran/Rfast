
columns <- function(x,indices) {
	.Call(Rfast_columns,x,indices)
}