colVars <- function(x, suma = NULL, std = FALSE) {
  ##  x is a matrix
  ##  if you want standard deviations set std = TRUE

  if ( !is.null(suma) ) {
    m <- suma
  } else {
    m <- colsums(x)
  }

  n <- dim(x)[1]
  x2 <- colsums(x^2)
  s <- ( x2 - m^2/n ) / (n - 1)

  if ( std )  s <- sqrt(s)

  s

}