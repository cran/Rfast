colnormlog.mle <- function(x) {
    dm <- dim(x)
    n <- dm[1]
    mx <- Rfast::colmeans(x)
    m <- log(mx)
    sigma <- Rfast::colmeans(x^2) - mx^2
    loglik <-  -0.5 * n * log(2 * pi * sigma) - 0.5 * n
    res <- cbind(mx, m, sigma, sigma * n/(n - 1), loglik)
    colnames(res) <- c("exp_mu", "mu", "biased variance", "unbiased variance", "log-lik")
    res
}
