multivmf.mle <- function(x, ina, tol = 1e-07, ell = FALSE) {
    ni <- tabulate(ina)
    dm <- dim(x)
    p <- dm[2]
    n <- dm[1]
    Apk <- function(p, k) besselI(k, p/2, expon.scaled = TRUE)/besselI(k, 
        p/2 - 1, expon.scaled = TRUE)
    m1 <- rowsum(x, ina)
    Ri <- sqrt( Rfast::rowsums(m1^2) ) / ni
    m <- m1/ni/Ri
    ki <- Ri * (p - Ri^2)/(1 - Ri^2)
    g <- max(ina) 
    for (i in 1:g) {
      k1 <- ki[i]  ;  R <- Ri[i]
      if (k1 < 1e+05) {
        apk <- Apk(p, k1)
        k2 <- k1 - (apk - R)/(1 - apk^2 - (p - 1)/k1 * apk)
        while (abs(k2 - k1) > tol) {
            k1 <- k2
            apk <- Apk(p, k1)
            k2 <- k1 - (apk - R)/(1 - apk^2 - (p - 1)/k1 * apk)
        }
        ki[i] <- k2
      }  else ki[i] <- k1
    }
    loglik <- NULL
    if (ell) {
      loglik <- ni * (p/2 - 1) * log(ki) - 0.5 * ni * p * log(2 * 
        pi) - ni * (log(besselI(ki, p/2 - 1, expon.scaled = TRUE)) + 
        ki) + ki * ni * Ri
    }
    list(loglik = loglik, mi = m, ki = ki) 
}



















