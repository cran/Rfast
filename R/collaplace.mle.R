collaplace.mle <- function(x) {
    n <- dim(x)[1]
    m <- colMedians(x)
    b <- rowmeans( abs(t(x) - m) )
    loglik <- -n * log(2 * b) - n
    res <- cbind(m, b, loglik)
    colnames(res) <- c("location", "scale", "log-likelihood")
    res
}
