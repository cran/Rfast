rowhameans <- function(x)  {
	dim(x)[1]/ rowsums(1/x)
}