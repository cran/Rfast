geom.mle <- function (x, type = 1) {
    if (type == 1) {
        sx <- sum(x) 
        n <- length(x)
        prob <- 1/(1 + sx/n)
        loglik <- n * log(prob) + sx * log(1 - prob)
    }
    else {
        n <- length(x)
        prob <- n/sum(x)
        loglik <- n * log(prob) + (n/prob - n) * log(1 - prob)
    }
    list(loglik = loglik, prob = prob)
}
