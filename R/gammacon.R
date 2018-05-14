gammacon <- function(y, tol = 1e-08) {
  n <- length(y)
  sx <- n
  ly <- log(y)
  b1 <- sum(ly) / n
  m <- exp( - b1 )
  com <- y * m 
  der <-  - sum(com) + sx
  der2 <- sum(com)
  b2 <- b1 - der/der2
  while ( sum( abs(b2 - b1) ) > tol ) {
    b1 <- b2
    m <- exp(- b1)
    com <- y * m 
    der <-  - sum(com) + sx
    der2 <- sum(com)
    b2 <- b1 - der/der2
  }
  com <- y * m
  phi <- sum( (com - 1)^2 ) / (n - 1)
  devi <-  - 2 * sum( log(com) - com ) - 2 * n
  list(be = b2, deviance = devi, phi = phi)
}


     