ttests <- function(x, y = NULL, ina, paired = FALSE, logged = FALSE) {

  if ( !paired ) {

    if ( is.null(y) ) {
      x1 <- x[ ina == 1, ]
      x2 <- x[ ina == 2, ]
      n1 <- sum( ina == 1 )
      n2 <- length(ina) - n1
    } else {
      x1 <- x     ;    n1 <- dim(x1)[1]
	  x2 <- y     ;    n2 <- dim(x2)[1]
    }

    m1 <- colmeans(x1)
    m2 <- colmeans(x2)
    f1 <- colVars(x1, suma = n1 * m1) / n1
    f2 <- colVars(x2, suma = n2 * m2) / n2
    fac <- f1 + f2
    dof <- fac^2 / ( f1^2 / (n1 - 1) + f2^2 / (n2 - 1) )
    stat <- ( m1 - m2 ) / sqrt(fac)
    if ( logged ) {
      pvalue <- log(2) + pt( abs(stat), dof, lower.tail = FALSE, log.p = TRUE )
    } else  pvalue <- 2 * pt( abs(stat), dof, lower.tail = FALSE )  
    result <- cbind(stat, pvalue, dof)

  } else {
    n <- dim(x)[1]
    if ( is.null(y) ) {
      z <- x[ ina == 1, ] - x[ ina == 2, ]
    } else  z <- x - y    
    m <- colmeans(z)
    s <- colVars(z, suma = n * m, std = TRUE)
    stat <- sqrt(n) * m / s
    if ( logged ) {
      pvalue <- log(2) + pt( abs(stat), n - 1, lower.tail = FALSE, log.p = TRUE )  
    } else  pvalue <- 2 * pt( abs(stat), n - 1, lower.tail = FALSE )    	
    result <- cbind(stat, pvalue)
  }

  result
}
