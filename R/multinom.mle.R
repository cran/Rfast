#[export]
multinom.mle <- function(x) {
  N <- sum( x[1, ] )
  p <- Rfast::colmeans(x) / N
  n <- dim(x)[1]  
  loglik <- n * lgamma(N + 1) +  sum( p * log(p) )* N * n - sum( Rfast::Lgamma(x + 1) )
  list(loglik = loglik, prob = p)
}
