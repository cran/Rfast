#[export]
vmf.mle <- function (x, tol = 1e-07) {
    dm <- dim(x)
    p <- dm[2]
    n <- dm[1]
    Apk <- function(p, k) besselI(k, p/2, expon.scaled = TRUE)/besselI(k, 
        p/2 - 1, expon.scaled = TRUE)
    m1 <- Rfast::colsums(x)
    R <- sqrt(sum(m1^2))/n
    m <- m1/n/R
    k <- R * (p - R^2)/(1 - R^2)
    if (k < 1e+05) {
      lik1 <- (p/2 - 1) * log(k) - log(besselI(k, p/2 - 1, expon.scaled = TRUE)) - k + k * R
      apk <- Apk(p, k)
      k <- k - (apk - R)/(1 - apk^2 - (p - 1)/k * apk)
      lik2 <- (p/2 - 1) * log(k) - log(besselI(k, p/2 - 1, expon.scaled = TRUE)) - k + k * R
      while ( lik2 - lik1 > tol ) {
        lik1 <- lik2
        apk <- Apk(p, k)
        k <- k - (apk - R)/(1 - apk^2 - (p - 1)/k * apk)
        lik2 <- (p/2 - 1) * log(k) - log(besselI(k, p/2 - 1, expon.scaled = TRUE)) - k + k * R
      }
    }
    else k <- k
    loglik <- n* lik2 - 0.5 * n * p * log(2 * pi) 
    list(loglik = loglik, mu = m, kappa = k)
}
