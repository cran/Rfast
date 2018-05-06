ztp.mle <- function(x, tol = 1e-09) {
  sx <- sum(x)
  n <- length(x)
  lam1 <- sx / n
  explam <- exp(lam1)
  a1 <- sx / lam1
  a2 <- n * explam / (explam - 1)
  f <- a1 - a2
  f2 <-  - a1 / lam1 + a2 / (explam - 1)
  lam2 <- lam1 - f / f2 
  i <- 2
  while ( abs( lam1 - lam2 ) > tol ) {
    i <- i + 1
    lam1 <- lam2
    explam <- exp(lam1)
    a1 <- sx / lam1
    a2 <- n * explam / (explam - 1)
    f <- a1 - a2
    f2 <-  - a1 / lam1 + a2 / (explam - 1)
    lam2 <- lam1 - f / f2 
  }

  loglik <- sx * log(lam2) - n * log( exp(lam2) - 1 ) -  sum( Lgamma(x + 1) )
  list(iters = i, loglik = loglik, lambda = lam2)
}

