poisson.mle <- function(x) {
   n <- length(x)
   sx <- sum(x)
   lambda <- sx / n
   loglik <-  - sx + sx * log(lambda) - sum( Rfast::Lgamma(x + 1) )
   list(loglik = loglik, lambda = lambda)
 }
