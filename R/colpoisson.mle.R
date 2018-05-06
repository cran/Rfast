colpoisson.mle <- function(x) { 
  n <- dim(x)[1]
  sx <- Rfast::colsums(x)
  loglik <- sum( - sx + sx * log(sx/n) - Rfast::colsums( Rfast::Lgamma(x + 1) ) )
  list(loglik = loglik, param = sx / n) 
}