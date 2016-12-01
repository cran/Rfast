rmvnorm <- function(n, mu, sigma) {
   p <- length(mu)
   x <- matrix(RcppZiggurat::zrnorm(n * p), ncol = p)
   x %*% chol(sigma) + rep(mu, rep(n, p) )

}



