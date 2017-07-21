betaprime.mle <- function(x, tol = 1e-09) {

  n <- length(x)
  slx <- sum( log(x) )
  slx2 <- sum( log1p(x) )
  m <- sum(x) / n    ;   s <- sum(x^2) - m^2
  b1 <- ( m^2 + m ) / s + 2 
  a1 <- abs( m * b1 - m )
  dera <- n * digamma(a1 + b1) - n * digamma(a1) + slx - slx2
  derb <- n * digamma(a1 + b1) - n * digamma(b1) - slx2 
  derab <- n * trigamma(a1 + b1)
  dera2 <- derab - n * trigamma(a1)
  derb2 <- derab - n * trigamma(b1)
  aold <- c(a1, b1)
  anew <- aold - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
  
  i <- 2
  while ( sum( abs(anew - aold) ) > tol ) {
    i <- i + 1
    aold <- anew
    a1 <- aold[1]      ;      b1 <- aold[2]
    dera <- n * digamma(a1 + b1) - n * digamma(a1) + slx - slx2
    derb <- n * digamma(a1 + b1) - n * digamma(b1) - slx2 
    derab <- n * trigamma(a1 + b1)
    dera2 <- derab - n * trigamma(a1)
    derb2 <- derab - n * trigamma(b1)
    anew <- aold - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
  }  
    
   a <- anew[1]    ;    b <- anew[2] 
   loglik <- (a - 1) * slx - (a + b) * slx2 - n * lbeta(a, b)
   list(iters = i, loglik = loglik, param = anew)   
}






