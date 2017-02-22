
colMinsMaxs <- function(x) {
	x <- .Call('Rfast_col_min_max', PACKAGE = 'Rfast',x)
	rownames(x) <- c("min","max")
	x
}