allbetas <- function(y, x, pvalue = FALSE) {

  if ( min(y) > 0 & max(y) < 1) {
    y <- log( y / (1 - y) )
  } 
  
  r <- as.vector( cov(y, x) )
  n <- length(y)
  my <- sum(y) / n
  m <- colMeans(x)
  sx <- colVars(x)
  be <- r / sx
  a <- my - be * m

  if ( pvalue == FALSE )  {

    result <- cbind(a, be)

    if ( is.null( colnames(x) ) ) {
      rownames(result) <- paste("X", 1:ncol(x), sep = "" )
    } else {
      rownames(result) <- colnames(x)
    }


  } else {

    sy <- sd(y)
    rho <- r / ( sqrt(sx) * sy)
    sqdof <- sqrt(n - 2)
    ta <- rho * sqdof / sqrt(1 - rho^2)
    pvalue <- 2 * pt( abs(ta), n - 2, lower.tail = FALSE)

    result <- cbind(a, be, rho, pvalue)

    if ( is.null( colnames(x) ) ) {
      rownames(result) <- paste("X", 1:ncol(x), sep = "" )
    } else {
      rownames(result) <- colnames(x)
    }

  }


  result

}
