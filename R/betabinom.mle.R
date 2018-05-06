betabinom.mle <- function(x, N, tol = 1e-07) {
  x1 <- x    ;    x2 <- N - x
  z1 <- Rfast::sort_unique(x1)
  n1 <- as.vector( table(x1) )
  z2 <- Rfast::sort_unique(x2) 
  n2 <- as.vector( table(x2) )
  n <- length(x1)
  m1 <- sum(x1) / n    ;    m2 <- sum(x1^2) / n
  down <- N * m2 / m1 - N * m1 - N +m1
  a <- (N * m1 - m2) / down
  b <- (N - m1) * (N - m2/m1) / down
  a <- abs(a)    ;    b <- abs(b)
  co <-  - n * digamma(N + a + b) + n * digamma(a + b)
  dera <- sum( digamma(z1 + a) * n1 ) + co - n * digamma(a)
  derb <- sum( digamma(z2 + b) * n2 ) + co - n * digamma(b)
  derab <-  - n * trigamma(N + a + b) + n * trigamma(a + b)
  dera2 <- sum( trigamma(z1 + a) * n1 ) + derab - n * trigamma(a)
  derb2 <- sum( trigamma(z2 + b) * n2 ) + derab - n * trigamma(b) 
  aold <- c(a, b)
  anew <- aold - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
  
  i <- 2
  while ( sum( abs(aold - anew) ) > tol ) {
    i <- i + 1
    aold <- anew     
    a <- anew[1]     ;      b <- anew[2] 
    co <-  - n * digamma(N + a + b) + n * digamma(a + b)
    dera <- sum( digamma(z1 + a) * n1 ) + co - n * digamma(a)
    derb <- sum( digamma(z2 + b) * n2 ) + co - n * digamma(b)
    derab <-  - n * trigamma(N + a + b) + n * trigamma(a + b)
    dera2 <- sum( trigamma(z1 + a) * n1 ) + derab - n * trigamma(a)
    derb2 <- sum( trigamma(z2 + b) * n2 ) + derab - n * trigamma(b) 
    anew <- aold - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
  }

   a <- anew[1]    ;     b <- anew[2]
   loglik <- n * Rfast::Lgamma(N + 1) - sum( Rfast::Lgamma(z1 + 1) * n1 ) - sum( Rfast::Lgamma(z2 + 1) * n2 ) +
          sum( Rfast::Lgamma(z1 + a) * n1 ) + sum( Rfast::Lgamma(z2 + b) * n2 ) - n * lgamma(N + a + b) - n * lbeta(a, b)
   pa <- c(a, b) 
   names(pa) <- c( "alpha", "beta" )
   list(iters = i, param = c(a, b), loglik = loglik)
}
