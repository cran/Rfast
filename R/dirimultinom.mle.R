dirimultinom.mle <- function(x, tol = 1e-07) {
  dm <- dim(x)
  p <- dm[2]  ## dimensionality
  n <- dm[1]  ## sample size
  rs <- rowsums(x)
  a1 <- colmeans(x) 
  
  x <- t(x)
  y <- x + a1
  sa <- sum(a1)
  lik1 <- n * lgamma( sa ) - sum( Rfast::Lgamma( rs + sa ) ) - n * sum( lgamma( a1 ) ) + sum( Rfast::Lgamma( y ) )
  f <- n * digamma(sa) - sum( Rfast::Digamma(rs + sa) ) - n * digamma(a1) + Rfast::rowsums( Rfast::Digamma(y) )
  f2 <- matrix(n * trigamma(sa) - sum( Rfast::Trigamma(rs + sa) ), p, p)
  diag(f2) <- diag(f2) - n * trigamma(a1) + Rfast::rowsums( Rfast::Trigamma(y) ) 
  a2 <- a1 - solve(f2, f)
  sa <- sum(a2)
  y <- x + a2
  lik2 <- n * lgamma( sa ) - sum( lgamma( rs + sa ) ) - n * sum( lgamma( a2 ) ) + sum( Rfast::Lgamma( y ) )

  i <- 2
  while ( lik2 - lik1 > tol ) {
    i <- i + 1
    lik1 <- lik2
    a1 <- a2
    f <- n * digamma(sa) - sum( Rfast::Digamma(rs + sa) ) - n * digamma(a1) + Rfast::rowsums( Rfast::Digamma(y) )
    f2 <- matrix(n * trigamma(sa) - sum( Rfast::Trigamma(rs + sa) ), p, p)	
    diag(f2) <-  diag(f2) - n * trigamma(a1) + Rfast::rowsums( Rfast::Trigamma(y) ) 
    a2 <- a1 - solve(f2, f)
    sa <- sum(a2)
    y <- x + a2
    lik2 <- n * lgamma( sa ) - sum( Rfast::Lgamma( rs + sa ) ) - n * sum( lgamma( a2 ) ) + sum( Rfast::Lgamma( y ) ) 
  }
  
  list(iters = i, loglik = lik2, param = a2)  
}

