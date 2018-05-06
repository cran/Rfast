collindley.mle <- function(x) {
    n <- dim(x)[1]
    sx <- Rfast::colsums(x)
    a <- sx/n
    b <- a - 1
    delta <- b^2 + 8 * a
    theta <- 0.5 * (-b + sqrt(delta))/a
    loglik <- 2 * n * log(theta) - n * log1p(theta) + colsums (log1p(x) ) - theta * sx
    res <- cbind(theta, loglik)
    colnames(res) <- c("theta", "log-likelihood")
    res
}
