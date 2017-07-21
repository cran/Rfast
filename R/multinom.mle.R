multinom.mle <- function(x) {
  N <- sum( x[1, ] )
  p <- colmeans(x) / N
  n <- dim(x)[1]  
  loglik <- n * lgamma(N + 1) + sum(t(x) * log(p) ) - sum( Lgamma(x + 1) )
  list(loglik = loglik, prob = p)
}


