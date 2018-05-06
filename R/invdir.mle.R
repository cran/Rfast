invdir.mle <- function(x, tol = 1e-09) {

  n <- dim(x)[1]
  p <- dim(x)[2]
  zx <- t( log(x) )
  sx2 <- sum( log1p( Rfast::rowsums(x) ) )
  com <- c( Rfast::rowsums(zx) - sx2, -sx2 )
  a <- Rfast::colmeans(x)   
  b <- Rfast::colVars(x, suma = n * a)
  D <- p + 1 
  aD <- 0.5 * ( mean(a)^2 + mean(a) ) / mean(b) + 1
  a1 <- abs( c( a * (aD - 1), aD) ) / 2
  phi <- sum(a1)
  f1 <- n * digamma( phi ) - n * digamma(a1) + com
  f2 <- matrix(n * trigamma(phi), D, D)
  diag(f2) <- diag(f2) - n * trigamma(a1)
  a2 <- a1 - solve(f2, f1)
  i <- 2
  while ( sum( abs(a2 - a1) ) > tol ) {   
    i <- i + 1
    a1 <- a2
    phi <- sum(a1)
    f1 <- n * digamma( phi ) - n * digamma(a1) + com
    f2 <- matrix(n * trigamma(phi), D, D)
    diag(f2) <- diag(f2) - n * trigamma(a1)
    a2 <- a1 - solve(f2, f1)
  }
  
  phi <- sum(a2)
  lik <-  n * lgamma( phi ) - n * sum( lgamma(a2) ) +
      sum( zx * (a2[1:p] - 1) ) - phi * sx2   
  list(iters = i, loglik = lik, param = a2 )
} 





