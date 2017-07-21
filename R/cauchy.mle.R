cauchy.mle <- function(x, tol = 1e-09) {
  n <- length(x)
  m <- med(x) 
  s <- 0.5 * ( nth(x, 3 * n/4) - nth(x, n/4) )
  y <- x - m
  y2 <- y^2
  down <- s^2 + y2
  down2 <- down^2
  deri <- c( 2 * sum( y / down ), n/s - 2 * s * sum(1 / down) )
  derm2 <- 2 * sum( (y2 - s^2) / down2 )
  ders2 <-  - n / s^2 - derm2
  derms <-  - 4 * s * sum( y / down2)
  aold <- c(m, s)
  anew <- aold - c( ders2 * deri[1] - derms * deri[2], - derms * deri[1] + derm2 * deri[2]) / (derm2 * ders2 - derms^2)
  i <- 2
  while ( sum( abs(aold - anew) ) > tol ) {
    i <- i + 1
    aold <- anew   
    m <- anew[1]     ;    s <- anew[2]
    y <- x - m
    y2 <- y^2
    down <- s^2 + y2
    down2 <- down^2
    deri <- c( 2 * sum( y / down ), n/s - 2 * s * sum(1 / down) )
    derm2 <- 2 * sum( (y2 - s^2) / down2 )
    ders2 <-  - n / s^2 - derm2
    derms <-  - 4 * s * sum( y / down2) 
    anew <- aold - c( ders2 * deri[1] - derms * deri[2], - derms * deri[1] + derm2 * deri[2]) / (derm2 * ders2 - derms^2)  
  }
  
  names(anew) <- c("location", "scale")
  lik <-  n * log(anew[2] / pi) - sum( log( anew[2]^2 + y2 ) ) 
  list(iters = i, loglik = lik, param = anew)
}  

