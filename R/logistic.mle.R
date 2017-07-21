logistic.mle <- function(x, tol = 1e-07) {
 
  n <- length(x)
  m <- sum(x) / n    
  exps <- sqrt(3) * sd(x) / pi
  y <- ( x - m ) / exps
  y2 <- 0.5 * y
  tanhy2 <- tanh(y2)
  sechy22 <- 1 / cosh(y2)^2
  derm <- sum( tanhy2 ) / exps
  derm2 <-  - 0.5 * sum( sechy22 ) / exps^2 
  ders <-  - n + sum( y * tanhy2 )
  ders2 <-  - ders - n - sum( y2^2 * sechy22 )
  derms <-  - derm - sum( sechy22 * y2) / exps
  aold <- c(m, log(exps) )
  anew <- aold - c( ders2 * derm - derms * ders, - derms * derm + derm2 * ders) / (derm2 * ders2 - derms^2)
  
  i <- 2
  while ( sum( abs(aold - anew) ) > tol ) {
    i <- i + 1
    aold <- anew  
    m <- anew[1]    
    exps <- exp( anew[2] ) 
    y <- (x - m ) / exps
    y2 <- 0.5 * y
    tanhy2 <- tanh(y2)
    sechy22 <- 1 / cosh(y2)^2
    derm <- sum( tanhy2 ) / exps
    derm2 <-  - 0.5 * sum( sechy22 ) / exps^2 
    ders <-  - n + sum( y * tanhy2 )
    ders2 <-  - ders - n - sum( y2^2 * sechy22 )
    derms <-  - derm - sum( sechy22 * y2) / exps
    anew <- aold - c( ders2 * derm - derms * ders, - derms * derm + derm2 * ders) / (derm2 * ders2 - derms^2)
  }
  
  anew[2] <- exp(anew[2])
  names(anew) <- c("location", "scale")
  lik <-  - n * log(4 * exps) + sum( log(sechy22) )
  list(iters = i, param = anew, loglik = lik)
}  