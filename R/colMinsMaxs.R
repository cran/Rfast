
colMinsMaxs <- function(x) {
	x <- .Call(Rfast_col_min_max,x)
	rownames(x) <- c("min","max")
	x
}