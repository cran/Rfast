foldnorm.mle <- function(x, tol = 1e-09) {
  n <- length(x)
  m <- sum(x) / n
  x2 <- x^2
  sx2 <- sum(x2)
  a <- sx2 / n - m^2  
  y <- m * x / a
  tanhy <- tanh(y)
  coshy2 <- 1 / cosh(y)^2
  derm <-  - n * m / a + sum(x * tanhy ) / a
  ders <-  - n / 2 / a + (sx2 + n * m^2) / 2 / a^2 - sum( y * tanhy ) / a
  derm2 <-  - n/ a + sum(x2 * coshy2 ) / a^2
  ders2 <- n / 2 / a^2 - (sx2 + n * m^2) / a^3  + 2 * sum( y * tanhy ) / a^2 + sum(y^2 * coshy2 ) / a^2
  derms <- n * m / a^2 - sum(x * tanhy ) / a^2 - sum(y * x * coshy2 ) / a^2
  aold <- c(m, a)
  anew <- aold - c( ders2 * derm - derms * ders, - derms * derm + derm2 * ders) / (derm2 * ders2 - derms^2)
  
  i <- 2
  while ( sum( abs(anew - aold) ) > tol ) {
    i <- i + 1
    m <- anew[1]    ;   a <- anew[2]
    aold <- anew
    y <- m * x / a
    tanhy <- tanh(y)
    coshy2 <- 1 / cosh(y)^2
    derm <-  - n * m / a + sum(x * tanhy ) / a
    ders <-  - n / 2 / a + (sx2 + n * m^2) / 2 / a^2 - sum( y * tanhy ) / a
    derm2 <-  - n/ a + sum(x2 * coshy2 ) / a^2
    ders2 <- n / 2 / a^2 - (sx2 + n * m^2) / a^3  + 2 * sum( y * tanhy ) / a^2 + sum(y^2 * coshy2 ) / a^2
    derms <- n * m / a^2 - sum(x * tanhy ) / a^2 - sum(y * x * coshy2 ) / a^2
    anew <- aold - c( ders2 * derm - derms * ders, - derms * derm + derm2 * ders) / (derm2 * ders2 - derms^2)
  }
  
  m <- anew[1]   ;  a <- anew[2]
  names(anew) <- c("mean", "sigma squared")
  loglik <- n/2 * log( 2 / pi / a) - 0.5 * n * m^2 / a - 0.5 * sum(x2) / a + sum( log( cosh( y ) ) )
  list(iters = i, loglik = loglik, param = anew)
}