
rowMinsMaxs <- function(x) {
	x <- .Call('Rfast_row_min_max', PACKAGE = 'Rfast',x)
	rownames(x) <- c("min","max")
	x
}