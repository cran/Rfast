
rowShuffle <- function(x) {
	.Call(Rfast_row_shuffle,x)
}