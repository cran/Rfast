collaplace.mle <- function(x) {
    n <- dim(x)[1]
    m <- Rfast::colMedians(x)
    b <- Rfast::colmeans( abs( Rfast::eachrow(x, m, oper = "-") ) )
    loglik <- -n * log(2 * b) - n
    res <- cbind(m, b, loglik)
    colnames(res) <- c("location", "scale", "log-likelihood")
    res
}
