normal.mle <- function(x) {
    n <- length(x)
    m <- sum(x)/n
    s <- (sum(x^2) - n * m^2)/(n - 1)
    loglik <-  - 0.5 * n * ( log(2 * pi) + log(s) ) - 0.5 * (n - 1)
    param <- c(m, s)
    names(param) <- c("mean", "unbiased variance")
    list(loglik = loglik, param = param)
}
