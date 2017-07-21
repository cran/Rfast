colpoisson.mle <- function(x) {
  
  n <- dim(x)[1]
  sx <- colsums(x)
  loglik <- sum( - sx + sx * log(sx/n) - colsums( Lgamma(x + 1) ) )

  list(loglik = loglik, param = sx / n) 

}