colrayleigh.mle <- function(x) {
    n <- dim(x)[1]
    sigma <- 0.5 * Rfast::colmeans(x^2)
    loglik <- Rfast::colsums( Rfast::Log(x) ) - n * log(sigma) - n
    res <- cbind(sigma, loglik)
    colnames(res) <- c("sigma", "log-likelihood")
    res
}