colpoisson.mle <- function(x) {
    n <- dim(x)[1]
    sx <- Rfast::colsums(x)
    loglik <-  -sx + sx * log(sx/n) - Rfast::colsums( Rfast::Lgamma(x + 1) )
    res <- cbind(sx/n, loglik)
    colnames(res) <- c("lambda", "log-likelihood")
    res
}
