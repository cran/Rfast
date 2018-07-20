
colShuffle <- function(x) {
	dm <- dim(x)
	ind <- .Call(Rfast_col_shuffle,dm[1],dm[2])
	x <- x[ind]
	dim(x) <- dm
	x
}