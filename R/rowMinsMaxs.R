
rowMinsMaxs <- function(x) {
	x <- .Call(Rfast_row_min_max,x)
	rownames(x) <- c("min","max")
	x
}