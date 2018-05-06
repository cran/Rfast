loglogistic.mle <- function(x, tol = 1e-09) {
  y <- log(x)
  n <- length(x)
  param <- Rfast::logistic.mle(y, tol = tol)$param
  a <- exp(param[1])   ;  b <- 1/param[2]
  loglik <- n * log(b/a) + (b - 1) * sum(y) - n * (b - 1) * log(a) - 2 * sum( log1p( (x/a)^b ) )
  list(loglik = loglik, param = c(a, b) )
}





# loglogistic <- function(x) {
#   slx <- sum( log(x) )
#   n <- length(x)
#   fun <- function(pa) {
#     a <- exp(pa[1])  ;  b <- exp(pa[2])
#     -n * log(b/a) - (b-1) * sum(slx) + n * (b-1)*log(a) + 2 * sum( log1p( (x/a)^b ) )
#   } 
#   mod <- nlm( fun, c(1, 2))
#   param <- mod$estimate
#   loglik <-  - mod$minimum
#   a <- exp(param[1])  ;  b <- exp(param[2])
#   c(loglik, a, b)
# } 
