colpareto.mle <- function(x) {
    n <- dim(x)[1]
    xm <- colMins(x, value = TRUE)
    com <- n * log(xm)
    slx <- colsums( Log(x) )
    a <- n/(slx - com)
    loglik <- n * log(a) + a * com - (a + 1) * slx
    res <- cbind(xm, a, loglik)
    colnames(res) <- c("scale", "shape", "log-likelihood")
    res
}