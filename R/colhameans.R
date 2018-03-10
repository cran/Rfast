colhameans <- function(x, parallel = FALSE)  {
	dim(x)[1]/ colsums(1/x, parallel = parallel)
}