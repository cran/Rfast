check_data <- function(x, ina = NULL) {

  if ( !is.matrix(x) )  x <- data.frame.to_matrix(x)
  if ( is.null(ina) ) {
    a <- colrange(x)
    b <- which(a == 0)
  } else {
    ina <- as.numeric(ina)
    k <- max(ina)
    a <- matrix( nrow = k, ncol = dim(x)[2] )
    for (i in 1:k) a[i, ] <- colrange( x[ina == i, ] )
    b <- which( colsums(a == 0) > 0 )
  }    
  
  b
}      
  