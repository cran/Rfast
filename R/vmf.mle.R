vmf.mle <- function(x, tol = 1e-07) {
  dm <- dim(x)
  p <- dm[2]  ## dimensionality of the data
  n <- dm[1]  ## sample size of the data
  Apk <- function(p, k)  besselI(k, p/2, expon.scaled = TRUE) / besselI(k, p/2 - 1, expon.scaled = TRUE)
  m1 <- Rfast::colsums(x)
  R <- sqrt( sum(m1^2) ) / n  ## mean resultant length
  m <- m1 / n / R
  k1 <- R * (p - R^2)/(1 - R^2)

  if (k1 < 100000) {
    apk <- Apk(p, k1)
    k2 <- k1 - (apk - R)/( 1 - apk^2 - (p - 1)/k1 * apk )
    while ( abs(k2 - k1) > tol ) {
      k1 <- k2
      apk <- Apk(p, k1)
      k2 <- k1 - (apk - R)/( 1 - apk^2 - (p - 1)/k1 * apk )
    }
    k <- k2
  } else  k <- k1

  loglik <- n * (p/2 - 1) * log(k) - 0.5 * n * p * log(2 * pi) -
    n * ( log( besselI(k, p/2 - 1, expon.scaled = TRUE) ) + k ) + k * n * R
  list(loglik = loglik, mu = m, kappa = k)
}
