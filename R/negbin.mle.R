negbin.mle <- function(x, type = 1, tol = 1e-09) {

  n <- length(x)
  sx <- sum(x)
  m <- sx / n
  m2 <- sum(x^2) / n
  p <- 1 - m / (m2 - m^2)
  expr1 <- m / p - m 
  mess <- NULL

  if ( expr1 < 0 )  mess <- c("Negative estimate of number of failures. A geometric or a binomial distribution is perhaps more suitable.")
  
  expr1 <- abs(expr1)  
  if ( type == 1 ) {
    r1 <- log(expr1)
    a <- x + expr1
    f <- sum( Digamma(a) ) * expr1 - n * Digamma(expr1) * expr1 + 
         n * expr1 * r1 - n * expr1 * log(expr1 + m)  
    f2 <- f + sum( Trigamma(a) ) * expr1^2 - n * Trigamma(expr1) * expr1^2 + n * expr1 -
          n * expr1^2 / (expr1 + m)
    r2 <- r1 - f / f2
    i <- 2
    while ( abs(r1 - r2) > tol  & r2 < 15) {
      i <- i + 1
      expr1 <- exp(r2)
      r1 <- r2
      a <- x + expr1
      f <- sum( Digamma(a) ) * expr1 - n * Digamma(expr1) * expr1 + 
           n * expr1 * r1 - n * expr1 * log(expr1 + m)  
      f2 <- f + sum( Trigamma(a) ) * expr1^2 - n * trigamma(expr1) * expr1^2 + n * expr1 -
            n * expr1^2 / (expr1 + m)
      r2 <- r1 - f / f2
    }

  } else {
    z <- sort_unique(x) 
    nz <- as.vector( table(x) )  
    r1 <- log(expr1)
    a <- z + expr1
    f <- sum( Digamma(a) * nz) * expr1 - n * Digamma(expr1) * expr1 + 
         n * expr1 * r1 - n * expr1 * log(expr1 + m)  
    f2 <- f + sum( Trigamma(a)* nz ) * expr1^2 - n * Trigamma(expr1) * expr1^2 + n * expr1 -
          n * expr1^2 / (expr1 + m)
    r2 <- r1 - f / f2
    i <- 2
    while ( abs(r1 - r2) > tol  &  r2 < 15) { 
      i <- i + 1
      expr1 <- exp(r2)
      r1 <- r2
      a <- z + expr1
      f <- sum( Digamma(a) * nz ) * expr1 - n * Digamma(expr1) * expr1 + 
           n * expr1 * r1 - n * expr1 * log(expr1 + m)  
      f2 <- f + sum( Trigamma(a) * nz ) * expr1^2 - n * Trigamma(expr1) * expr1^2 + n * expr1 -
            n * expr1^2 / (expr1 + m)
      r2 <- r1 - f / f2
    }
  }
  
    expr2 <- exp(r2)
    p <- expr2 / (expr2 + m)
    param <- c( p, expr2, m )
    names(param) <- c("success probability", "number of failures", "mean")
    loglik <- sum( Lgamma(x + expr2) ) - sum( Lgamma(x + 1) ) - n * lgamma(expr2) +
              sx * log( 1- p) + n * expr2 * log(p)  
  
  list(mess = mess, iters = i, loglik = loglik, param = param)
}





