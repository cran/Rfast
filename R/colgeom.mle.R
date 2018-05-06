colgeom.mle <- function (x, type = 1) {
    if (type == 1) {
        sx <- Rfast::colsums(x)
        n <- dim(x)   
        prob <- 1/(1 + sx/n )
        loglik <- n * log(prob) + sx * log(1 - prob)
    }
    else {
        n <- dim(x)[1]
        prob <- n/Rfast::colsums(x)
        loglik <- n * log(prob) + (n/prob - n) * log(1 - prob)
    }
    res <- cbind(prob, loglik)
	colnames(res) <- c("prob of success", "log-likelihood")
	res
}
