invgauss.regs <- function(y, x, tol = 1e-08, logged = FALSE, maxiters = 100) {
  
  dm <- dim(x)
  n <- dm[1]
  sy <- sum(1 / y)
  ly <- log(y)
  y2 <- 2 * y
  me <- sy/n
  D <- dm[2]
  phi <- dev <- numeric(D)
  con <- log(n)
  d0 <- n/Rfast::invgauss.mle(y)$param[2]

  be <- Rfast::allbetas( ly, x )

  for (j in 1:D) {
    X <- x[, j]
	X2 <- X^2 
    a <- be[j, 1]   ;   b <- be[j, 2]
    mi <- exp(a + b * X)
    mi2 <- mi^2
    dera <- sum( (mi - y) / mi2)
    dera2 <- sum( (y2 - mi ) / mi2 )
    derb <- sum(X * (mi - y)/mi2 )
    derb2 <- sum(X2 * (y2 - mi) / mi2 )
    derab <- sum(X * (y2 - mi) / mi2 )
    be1 <- c(a, b)
    be2 <- be1 - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
	i <- 2  
    while ( sum( abs(be1 - be2) ) > tol  |  i < maxiters ) {
	  i <- i + 1
      be1 <- be2
      a <- be1[1]   ;   b <- be1[2]
      mi <- exp(a + b * X)
	  mi2 <- mi^2
      dera <- sum( (mi - y) / mi2)
      dera2 <- sum( (y2 - mi ) / mi2 )
      derb <- sum(X * (mi - y)/mi2 )
      derb2 <- sum(X2 * (y2 - mi) / mi2 )
      derab <- sum(X * (y2 - mi) / mi2 )
      be2 <- be1 - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
    }
    
    dev[j] <- n * ( me - sum( 1 / mi) / n )
    phi[j] <- sum( (y - mi)^2 / ( mi^3) )/(n - 2) 
  } 
  stat <- (d0 - dev)/phi
  pvalue <- pf(stat, 1, n -2, lower.tail = FALSE, log.p = logged)
  cbind(stat, pvalue)
}

