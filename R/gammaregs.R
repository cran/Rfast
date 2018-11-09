gammaregs <- function(y, x, tol = 1e-07, logged = FALSE, maxiters = 100) {
  
    dm <- dim(x)
    n <- dm[1]
    D <- dm[2]
    devi <- phi <- numeric(D)
    ind <- 1:D 

    sx <- n
    ly <- log(y)
    sy <- sum(y)
    be <- sum(ly)/n
    m <- exp(-be)
    der2 <- sy * m
    d1 <- sum(ly) + n * log(m) - der2
    der <-  - der2 + sx
    be <- be - der/der2
    m <- exp(-be)
    d2 <- sum(ly) + n * log(m) - der2
    while ( abs(d2 - d1) > tol) {
      d1 <- d2
      der2 <- sy * m
      der <-  - der2 + sx
      be <- be - der/der2
      m <- exp(-be)
      d2 <- sum(ly) + n * log(m) - der2
    }
    common <- y * m
    phi <- sum( (common - 1)^2 )/(n - 1)
    ini <-  - 2 * d2 - 2 * n
    b1 <- be 
   
    sx <- Rfast::colsums(x)
    x2 <- x^2 
    for (j in ind) {
      d1 <- ini
      X <- x[, j]
      sxj <- sx[j]
      x2j <- x2[, j]
      m <- exp(-b1)
      dera2 <- sy * m
      dera <-  - dera2 + n
      derab <- sum(common * X)
      derb <-  - derab + sxj
      derb2 <- sum(common * x2j)
      be <- c(b1, 0) - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
      a <- be[1]   ;    b <- be[2]
      m <- exp(- a - b * X)
      com <- y * m
      d2 <-  - 2 * sum( log(com) ) + 2 * sum(com) - 2 * n
      i <- 2
      while ( abs(d2 - d1) > tol  &  i < maxiters ) {
        i <- i + 1
	    d1 <- d2
        dera2 <- sum(com)
        dera <-  - dera2 + n
        derab <- sum(com * X)
        derb <-  - derab + sxj
        derb2 <- sum(y * x2j * m)
        be <- be - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
        a <- be[1]   ;    b <- be[2]
        m <- exp(- a - b * X)
        com <- y * m
        d2 <-  - 2 * sum( log(com) ) + 2 * sum(com) - 2 * n
      }
      com <- y * m
      phi[j] <- sum( (com - 1)^2 ) / (n - 2)
      devi[j] <-  d2
    }     
    stat <- (ini - devi)/phi
    pvalue <- pf(stat, 1, n - 2, lower.tail = FALSE, log.p = logged)
    cbind(stat, pvalue)
}



  
