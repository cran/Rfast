bc <- function(x, low = -1, up = 1) {
  lx <- log(x)
  slx <- sum( lx )
  n2 <-  - 0.5 * length(x)
  vlx <- Rfast::Var(lx)
  ell <- function(lambda, x, vlx, slx, n2) {
    if ( abs(lambda) < 1e-12 ) {
      s <- vlx
    } else {
      y <- x^lambda
      s <- Rfast::Var(y) / lambda^2
    }
    n2 * log(s) + lambda * slx
  }
  optimise(ell, c(low, up), x = x, vlx = vlx, slx = slx, n2 = n2, 
           maximum = TRUE, tol = 1e-06)$maximum
}  



#bc2 <- function(x, low = -1, up = 1, tol = 1e-06) {
#  lx <- log(x)
#  slx <-  - sum( lx )
#  n2 <-  0.5 * length(x)
#  vlx <- Rfast::Var(lx)
#  gr <- 2/(sqrt(5) + 1)
#  a <- low    ;  b <- up
#  c <- a      ;  d <- b 
#  ell <- function(lambda, x, vlx, slx, n2) {
#    if ( abs(lambda) < 1e-12 ) {
#      s <- vlx
#    } else {
#      y <- x^lambda
#      s <- Rfast::Var(y) / lambda^2
#    }
#    n2 * log(s) + lambda * slx
#  }
#  while ( abs(c - d) > tol ){
#    if ( ell(c, x, vlx, slx, n2) < ell(d, x, vlx, slx, n2) ) {
#      b <- d
#    } else a <- c
#    c <- b - (b - a) * gr
#    d <- a + (b - a) * gr
#  }
#  (b + a) / 2
#}

