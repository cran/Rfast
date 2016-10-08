rowVars <- function(x, suma = NULL, std = FALSE) {
  
  if ( !is.null(suma) ) {
    m <- suma
  } else {
    m <- rowsums(x)
  }
  
  n <- dim(x)[1]
  x2 <- rowsums(x^2)
  s <- ( x2 - m^2/n ) / (n - 1)
  
  if ( std )  s <- sqrt(s)
  
  s
  
}