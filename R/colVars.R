colVars <- function(x, suma = NULL, std = FALSE, parallel = FALSE) {
  ##  x is a matrix
  ##  if you want standard deviations set std = TRUE
  if ( !is.null(suma) ) {
    m <- suma
  } else  m <- Rfast::colsums(x, parallel = parallel)
  n <- dim(x)[1]
  x2 <- Rfast::colsums(x^2, parallel = parallel)
  s <- ( x2 - m^2/n ) / (n - 1)
  if ( std )  s <- sqrt(s)
  s
}