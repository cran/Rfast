rowVars <- function(x, suma = NULL, std = FALSE) {
  if ( !is.null(suma) ) {
    m <- suma
  } else  m <- Rfast::rowsums(x)
  n <- dim(x)[2]
  x2 <- Rfast::rowsums(x^2)
  s <- ( x2 - m^2/n ) / (n - 1)
  if ( std )  s <- sqrt(s)
  s
}