rmvnorm <- function(n, mu, sigma) {
   p <- length(mu)
   x <- Rfast::matrnorm(n, p)
   x %*% chol(sigma) + rep(mu, rep(n, p) )
}