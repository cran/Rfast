colmaxboltz.mle <- function(x) {
    n <- dim(x)[1]
    a <- sqrt( colsums(x^2) / (3 * n) )
    loglik <- n/2 * log(2/pi) + 2 * colsums( Log(x) ) - 1.5 * n - 3 * n * log(a)
    res <- cbind(a, loglik)
    colnames(res) <- c("alpha", "loglikelihood")
    res
}
