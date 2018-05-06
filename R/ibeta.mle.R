ibeta.mle <- function(x, tol = 1e-09) {
 
  if ( all(x > 0  &  x < 1) ) {   
    res <- Rfast::beta.mle(x)
    mes <- "Regular beta distribution was fitted."

  } else {
    n <- length(x)
    z <- x[ x > 0 & x < 1 ]
    T2 <- sum( log(z) )
    T3 <- sum( log(1 - z) ) 
    if ( min(x) == 0 ) {
      T1 <- sum( x == 0 )
      mes <- "Zero inflated beta was fitted."
    } else {
      T1 <- sum( x == 1 )
      mes <- "One inflated beta was fitted."
    }
    a <- T1 / n
    t23 <- T2 - T3
    f <- n - T1
    ini <- Rfast::beta.mle(z)
    phi1 <- sum(ini$param)
    m1 <- ini$param[1]/phi1
    m1phi <- m1 * phi1
    m2phi <- (1 - m1) * phi1
    derm <- phi1 * f * ( digamma(m2phi) - digamma( m1phi ) ) + phi1 * t23 
    derphi <- f * ( digamma(phi1) - m1 * digamma(m1phi) - (1 - m1) * digamma(m2phi) ) + m1 * t23  + T3
    derm2 <-  - f * phi1^2 * ( trigamma(m1phi) + trigamma(m2phi) )
    derphi2 <- f * ( trigamma(phi1) - trigamma(m1phi) * m1^2 - trigamma(m2phi) * (1 - m1)^2 )
    dermphi <- f * ( - trigamma(m1phi) * m1phi - digamma(m1phi) + trigamma(m2phi) * m2phi + digamma(m2phi) ) + t23
    aold <- c(m1, phi1)
    anew <- aold - c( derphi2 * derm - dermphi * derphi, - dermphi * derm + derm2 * derphi ) / ( derm2 * derphi2 - dermphi^2 )
    i <- 2
    while ( sum( abs(aold - anew) ) > tol ) {
      i <- i + 1
      m1 <- anew[1]    ;   phi1 <- anew[2]
      aold <- anew
      m1phi <- m1 * phi1
      m2phi <- (1 - m1) * phi1
      derm <- phi1 * f * ( digamma(m2phi) - digamma( m1phi ) ) + phi1 * t23 
      derphi <- f * ( digamma(phi1) - m1 * digamma(m1phi) - (1 - m1) * digamma(m2phi) ) + m1 * t23  + T3
      derm2 <-  - f * phi1^2 * ( trigamma(m1phi) + trigamma(m2phi) )
      derphi2 <- f * ( trigamma(phi1) - trigamma(m1phi) * m1^2 - trigamma(m2phi) * (1 - m1)^2 )
      dermphi <- f * ( - trigamma(m1phi) * m1phi - digamma(m1phi) + trigamma(m2phi) * m2phi + digamma(m2phi) ) + t23
      anew <- aold - c( derphi2 * derm - dermphi * derphi, - dermphi * derm + derm2 * derphi ) / ( derm2 * derphi2 - dermphi^2 )
    }
    m <- anew[1]   ;   phi <- anew[2]
    param <- c(a, m, phi1)
    names(param) <- c("Propoprtion", "mean", "precision")
    loglik <- T1 * log(a) + f * log(1 - a) + f * ( lgamma(phi) - lgamma(m * phi) - lgamma( (1 - m) * phi ) ) + T2 * (m * phi - 1) + T3 * ( (1 - m) * phi - 1 )
    res <- list(iters = i, loglik = loglik, param = param)
  } 
  
  list(mes <- mes, res <- res)
}
