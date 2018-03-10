tobit.mle <- function(y, tol = 1e-09) {
  y1 <- y[y >0]  ;  n1 <- length(y1)
  n <- length(y)
  n0 <- n - n1
  sy12 <- sum(y1^2)
  m <- mean(y)   ;  s <- sqrt( sy12/n - m^2 )
  sy1 <- n * m
  com <- dnorm(m, 0, s) / pnorm(-m/s)
  derm <- (sy1 - n1 * m)/s^2 - n0 * com
  derm2 <-  -n1/s^2 - n0 * ( -m /s^2 * com + com^2 ) 
  ders <-  - n1 + (sy12 - 2 * m * sy1 + n1 * m^2)/s^2 + n0 * m * com
  ders2 <-  - 2 * (sy12 - 2 * m * sy1 + n1 * m^2)/s^2 + n0 * m * ( - com + m^2/s^2 * com - com^2 * m ) 
  derms <-  - 2 * (sy1 - n1 * m)/s^2 - n0 * ( - com + m^2/s^2 * com - com^2 * m ) 
  aold <- c(m, log(s))
  anew <- aold - c( ders2 * derm - derms * ders, - derms * derm + derm2 * ders ) / ( derm2 * ders2 - derms^2 )
  i <- 2
  while ( sum( abs(aold - anew) ) > tol ) {
    i <- i + 1
    aold <- anew   
    m <- anew[1]     ;    s <- exp( anew[2] )
    com <- dnorm(m, 0, s) / pnorm(-m/s, 0, 1)
    derm <- (sy1 - n1 * m)/s^2 - n0 * com
    derm2 <-  - n1/s^2 - n0 * ( -m /s^2 * com + com^2 ) 
    ders <-  - n1 + (sy12 - 2 * m * sy1 + n1 * m^2)/s^2 + n0 * m * com
    ders2 <-  - 2 * (sy12 - 2 * m * sy1 + n1 * m^2)/s^2 + n0 * m * ( - com + m^2/s^2 * com - com^2 * m ) 
    derms <-  - 2 * (sy1 - n1 * m)/s^2 + n0 * ( com - m^2/s^2 * com + com^2 * m ) 
    anew <- aold - c( ders2 * derm - derms * ders, - derms * derm + derm2 * ders ) / ( derm2 * ders2 - derms^2 )
  }
  s <- exp(anew[2])
  loglik <-  - 0.5 * n1 * log(2 * pi * s^2) - 0.5 * ( sy12 - 2 * m * sy1 + n1 * m^2 ) / s^2 + n0 * log( pnorm(-m/s) )
  param <- c(anew[1], s)
  names(param) <- c("location", "scale")
  list(iters = i, loglik = loglik, param = param)
} 