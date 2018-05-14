gammaregs <- function(y, x, tol = 1e-08, logged = FALSE, maxiters = 100) {
  
    dm <- dim(x)
    n <- dm[1]
    D <- dm[2]
    devi <- phi <- numeric(D)
    ind <- 1:D 
    ly <- log(y)
    b1 <- sum(ly) / n
    m <- exp( - b1 )
    com <- y * m 
    der2 <- sum(com)
    der <-  - der2 + n
    b2 <- b1 - der/der2
    while ( sum( abs(b2 - b1) ) > tol ) {
      b1 <- b2
      m <- exp(- b1)
      com <- y * m 
      der2 <- sum(com)
      der <-  - der2 + n
      b2 <- b1 - der/der2
    }
    ini <-  - 2 * sum( log(com) ) + 2 * der2 - 2 * n

    sx <- Rfast::colsums(x)
    x2 <- x^2 
    be <- Rfast::allbetas(ly, x )
    for (j in ind) {
      X <- x[, j]
      sxj <- sx[j]
      x2j <- x2[, j]
      aold <- be[j, ] 
      a <- aold[1]   ;  b <- aold[2]    
      m <- exp(- a - b * X)
      dera2 <- sum(y * m)
      dera <-  - dera2 + n
      derab <- sum(y * X * m)
      derb <-  - derab + sxj
      derb2 <- sum(y * x2j * m)
      anew <- aold - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
      i <- 2
	  while ( sum( abs(anew - aold) ) > tol | i < maxiters ) {
        i <- i + 1
		aold <- anew
        a <- aold[1]   ;    b <- aold[2]
        m <- exp(- a - b * X)
        dera2 <- sum(y * m)
        dera <-  - dera2 + n
        derab <- sum(y * X * m)
        derb <-  - derab + sxj
        derb2 <- sum(y * x2j * m)
        anew <- aold - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
      }
      com <- y * m
      phi[j] <- sum( (com - 1)^2 ) / (n - 2)
      devi[j] <-  - 2 * sum( log(com) ) + 2 * dera2 - 2 * n
    }     
    stat <- (ini - devi)/phi
    pvalue <- pf(stat, 1, n - 2, lower.tail = FALSE, log.p = logged)
    cbind(stat, pvalue)
}



  
