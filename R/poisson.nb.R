poisson.nb <- function(xnew, x, ina) {
  nu <- tabulate(ina)
  m <- rowsum(x, ina) / nu 
  score <-  - Rfast::rowsums(m) + tcrossprod( xnew, log(m) ) 
  ## - Rfast::rowsums( lgamma(xnew + 1) ) ## not necessary, does not change the score 
  rowMaxs(score)
}