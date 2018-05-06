colvm.mle <- function (x, tol = 1e-07) {
    n <- dim(x)[1]
    C <- Rfast::colmeans( cos(x) )
    S <- Rfast::colmeans( sin(x) )
    ep <- (C < 0)
    mu <- atan(S/C)
    mu[ep] <- mu[ep] + pi
    con <- Rfast::rowsums(cos(t(x) - mu))
    R <- C^2 + S^2
    k1 <- (1.28 - 0.53 * R) * tan(0.5 * pi * sqrt(R))
    der <- con - n * besselI(k1, 1)/besselI(k1, 0)
    a <- besselI(k1, 0)^2/2 + besselI(k1, 2) * besselI(k1, 0)/2 - 
        besselI(k1, 1)^2
    der2 <- n * a/besselI(k1, 0)^2
    k2 <- k1 + der/der2
    while (max(abs(k1 - k2)) > tol) {
        k1 <- k2
        der <- con - n * besselI(k1, 1)/besselI(k1, 0)
        a <- besselI(k1, 0)^2/2 + besselI(k1, 2) * besselI(k1, 
            0)/2 - besselI(k1, 1)^2
        der2 <- n * a/besselI(k1, 0)^2
        k2 <- k1 + der/der2
    }
    k2[which(is.na(k2))] <- 709
    loglik <- k2 * con - n * log(2 * pi) - n * (log(besselI(k2, 
        0, expon.scaled = TRUE)) + k2)
    res <- cbind(mu, k2, loglik)
    colnames(res) <- c("mean", "concentration", "log-likelihood")
    res
}
