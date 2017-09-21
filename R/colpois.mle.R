colpois.mle <- function(x) {
  n <- dim(x)[1]
  sx <- colsums(x)
  loglik <- sx * log(sx/n) - sx - colsums( Lgamma(x + 1) )
  res <- cbind(sx / n, loglik)
  colnames(res) <- c("lambda", "log-likelihood")
  res  
}