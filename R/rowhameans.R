rowhameans <- function(x)  {
	dim(x)[1]/ Rfast::rowsums(1/x)
}