betageom.mle <- function(x, tol = 1e-07) {

  n <- length(x)
  m1 <- sum(x) / n
  m2 <- sum(x^2) / n
  b <- abs( 2 * (m2 - m1^2) / (m2 - m1 - 2 * m1^2) )
  a <- abs( m1 * (b - 1) )
  ya <- a + x
  y <- ya + b + 1

  com <- n * digamma(a + b) - sum( Digamma(y) )
  dera <- sum( Rfast::Digamma(ya) ) + com - n * digamma(a)
  derb <- n * digamma(b + 1) + com - n * digamma(b)
  derab <- n * trigamma(a + b) - sum( Rfast::Trigamma( y ) )
  dera2 <- sum( Rfast::Trigamma(ya) ) + derab - n * trigamma(a)
  derb2 <- n * trigamma(b + 1) + derab - n * trigamma(b)
  
  aold <- c(a, b)
  anew <- aold - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
  
  i <- 2
  while ( sum( abs(anew - aold) ) > tol ) {
    i <- i + 1
    aold <- anew
    a <- aold[1]    ;    b <- aold[2]
    ya <- a + x
    y <- ya + b + 1

    com <- n * digamma(a + b) - sum( Digamma(y) )
    dera <- sum( Rfast::Digamma(ya) ) + com - n * digamma(a)
    derb <- n * digamma(b + 1) + com - n * digamma(b)
    derab <- n * trigamma(a + b) - sum( Rfast::Trigamma( y ) )
    dera2 <- sum( .Call("Rfast_Trigamma", PACKAGE = "Rfast", ya) ) + derab - n * trigamma(a)
    derb2 <- n * trigamma(b + 1) + derab - n * trigamma(b)
    anew <- aold - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
  }
  
  list(iters = i, anew = anew)

}
  

