betaprime.mle <- function(x, tol = 1e-09) {

  n <- length(x)
  slx <- sum( log(x) )
  slx2 <- sum( log1p(x) )
  m <- sum(x) / n    ;   s <- sum(x^2) - m^2
  b <- ( m^2 + m ) / s + 2 
  a <- abs( m * b - m )
  lik1 <- (a - 1) * slx - (a + b) * slx2 - n * lbeta(a, b)
  
  dera <- n * digamma(a + b) - n * digamma(a) + slx - slx2
  derb <- n * digamma(a + b) - n * digamma(b) - slx2 
  derab <- n * trigamma(a + b)
  dera2 <- derab - n * trigamma(a)
  derb2 <- derab - n * trigamma(b)
  anew <- c(a, b) - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
  a <- anew[1]      ;      b <- anew[2]
  lik2 <- (a - 1) * slx - (a + b) * slx2 - n * lbeta(a, b)
  
  i <- 2
  while ( lik2 - lik1 > tol ) {
    i <- i + 1
	lik1 <- lik2
    a <- anew[1]      ;      b <- anew[2]
    dera <- n * digamma(a + b) - n * digamma(a) + slx - slx2
    derb <- n * digamma(a + b) - n * digamma(b) - slx2 
    derab <- n * trigamma(a + b)
    dera2 <- derab - n * trigamma(a)
    derb2 <- derab - n * trigamma(b)
    anew <- anew - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
    a <- anew[1]      ;      b <- anew[2]
    lik2 <- (a - 1) * slx - (a + b) * slx2 - n * lbeta(a, b)
  }  
    
   names(anew) <- c("alpha", "beta")
   loglik <- (a - 1) * slx - (a + b) * slx2 - n * lbeta(a, b)
   list(iters = i, loglik = loglik, param = anew)   
}






