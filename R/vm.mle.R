################################
#### References: S Rao Jammalamadaka and A SenGupta (2001)
#### Topics in circular statistics
####
#### References: Mardia Kanti V. and Jupp Peter E. (2000)
#### Directional statistics
################################
vm.mle <- function(x, tol = 1e-09) {
 
  n <- length(x)  ## sample size
  C <- sum( cos(x) ) / n 
  S <- sum( sin(x) ) / n
  if (C > 0) {
    mu <- atan(S/C)
  } else  mu <- atan(S/C) + pi
  con <- sum( cos(x - mu) ) 
  R <- sqrt( C^2 + S^2 ) 
  k1 <- (1.28 - 0.53 * R^2) * tan(0.5 * pi * R)
  if ( k1 < 710 ) { 
    der <- con - n *  besselI(k1, 1, expon.scaled = TRUE) / besselI(k1, 0, expon.scaled = TRUE)
    a <- besselI(k1, 0)^2 / 2  + besselI(k1, 2) * besselI(k1, 0) / 2 - besselI(k1, 1)^2
    der2 <-  n * a / besselI(k1, 0)^2
    k2 <- k1 + der / der2
    while ( abs(k1 - k2) > tol) {
      k1 <- k2 
      der <- con - n *  besselI(k1, 1, expon.scaled = TRUE) / besselI(k1, 0, expon.scaled = TRUE)
      a <- besselI(k1, 0)^2 / 2  + besselI(k1, 2) * besselI(k1, 0) / 2 - besselI(k1, 1)^2
      der2 <-  n * a / besselI(k1, 0)^2
      k2 <- k1 + der/ der2
    }
      
  } else k1 <- k1
  
  param <- c(mu, k2) 
  names(param) <- c("mean", "concentration")
  loglik <- k2 * con - n * log(2 * pi) - n * ( log( besselI(k2, 0, expon.scaled = TRUE) ) + k2 )
  list(loglik = loglik, param = param)
}
  