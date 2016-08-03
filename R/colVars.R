colVars <- function(x, suma = NULL, std = FALSE) {
  ##  x is a matrix
  ##  if you want standard deviations set std = TRUE

  if ( !is.null(suma) ) {
    m <- suma
  } else {
    m <- as.vector(colsums(x))
  }

  n <- nrow(x)
  x2 <- as.vector(colsums(x^2))
  s <- ( x2 - m^2/n ) / (n - 1)

  if ( std == TRUE )  s <- sqrt(s)

  s

}