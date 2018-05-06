mvnorm.mle <- function(x) {
   m <- Rfast::colmeans(x)
   dm <- dim(x)
   n <- dim(x)[1]
   d <- dm[2]
   s <- crossprod(x)/n - tcrossprod(m)
   a <-  n * d * log(2 * pi) + n * log( det(s) ) + n * d
   list(loglik = - 0.5 * a, mu = m, sigma = s)
}

