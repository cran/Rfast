wrapcauchy.mle <- function(x, tol = 1e-09) {
  n <- length(x)
  cx <- cos(x)   
  sx <- sin(x)
  cs <- cbind(cx, sx)
  sa <- Rfast::colMedians(cs)
  C <- sa[1]
  S <- sa[2]
  rho <- sqrt(C^2 + S^2)
  a <- ( atan(S/C) + pi * I(C < 0) ) %% (2 * pi) 
  mc <- 2 * rho * cos(a) / (1 + rho^2) 
  ms <- 2 * rho * sin(a) / (1 + rho^2) 
  m1 <- c(mc, ms)
  wi <- 1 / (1 - mc * cx - ms * sx)
  m2 <- Rfast::colsums(wi * cs) / sum(wi)
  i <- 2
  while ( sum( abs(m1 - m2) ) > tol ) {
    i <- i + 1
    m1 <- m2
    wi <- 1 / (1 - m1[1] * cx - m1[2] * sx)
    m2 <- Rfast::colsums(wi * cs) / sum(wi)
  }

  a <- ( atan(m2[2] / m2[1]) + pi * I(m2[1] < 0) ) %% (2 * pi) 
  k <- m2[1] / cos(a)
  rho <- ( 1 - sqrt(1 - k^2) )/abs(k)
  loglik <-  - n * log(2 * pi) + n * log(1 - rho^2) - sum( log1p(rho^2 - 2 * rho * cos(x - a)) )
  param <- c(a, rho)
  names(param) <- c("mean direction", "rho" )
  list(iters = i, loglik = loglik, param = param)
}
  

   




  
