lognorm.mle <- function (x) {
    n <- length(x)
    x <- log(x)
    sx <- sum(x)
    m <- sx/n
    s <- sum(x^2)/n - m^2
    loglik <-  - 0.5 * n * ( log(2 * pi * s) + 1 ) - sx
    param <- c(m, s)
    names(param) <- c("mean", "variance")
    list(loglik = loglik, param = param)
}
