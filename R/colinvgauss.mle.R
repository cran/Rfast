colinvgauss.mle <- function(x) {
    n <- dim(x)[1]
    sx <- Rfast::colsums(x)
    sx2 <- Rfast::colsums(1/x)
    m <- sx/n
    lambda <- 1/(sx2/n - 1/m)
    loglik <- n * 0.5 * log( 0.5 * lambda/pi) - 1.5 * colsums( Log(x) ) - 0.5 * lambda/m^2 * (-sx + m^2 * sx2)
    res <- cbind(m, lambda, loglik)
    colnames(res) <- c("mu", "lambda", "log-likelihood")
    res
}
