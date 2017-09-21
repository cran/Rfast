lomax.mle <- function(x, tol = 1e-09) {
  n <- length(x)
  x2 <- x^2
  m <- sum(x) / n
  s2 <- sum(x2) / n - m^2
  expa <- abs( 2 * s2 / (s2 - m^2) )
  explam <- abs( expa - 1 ) * m
  dera2 <-  - expa * sum( log1p( x / explam) )
  dera <-  n + dera2
  com <- sum( x / (explam + x) )
  derlama <- expa * com
  derlam <-  - n + derlama + com
  derlam2 <-  - ( expa + 1)* explam * sum( x / (explam + x)^2 )
  aold <- log( c(expa, explam) )
  anew <- aold - c( derlam2 * dera - derlama * derlam, - derlama * dera + dera2 * derlam ) / ( dera2 * derlam2 - derlama^2 )
  
  i <- 2
  while ( sum( abs(anew - aold) ) > tol ) {
    i <- i + 1
    aold <- anew
    expa <- exp(anew[1])     ;      explam <- exp(anew[2]) 
    dera2 <-  - expa * sum( log1p( x / explam) )
    dera <-  n + dera2
    com <- sum( x / (explam + x) )
    derlama <- expa * com
    derlam <-  - n + derlama + com
    derlam2 <-  - ( expa + 1) * explam * sum( x / (explam + x)^2 )
    anew <- aold - c( derlam2 * dera - derlama * derlam, - derlama * dera + dera2 * derlam ) / ( dera2 * derlam2 - derlama^2 )
  } 

  a <- exp(anew[1])    ;     lam <- exp(anew[2])
  loglik <- n * log(a / lam) - (a + 1) * sum( log1p(x / lam) )  
  names(anew) <- c("shape", "scale")
  list( iters = i, loglik = loglik, param = c(a, lam) ) 
}



   









