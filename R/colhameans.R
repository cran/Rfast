colhameans <- function(x, parallel = FALSE)  {
	dim(x)[1]/ Rfast::colsums(1/x, parallel = parallel)
}