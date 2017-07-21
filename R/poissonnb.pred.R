poissonnb.pred <- function(xnew, m) {
  score <-  - Rfast::rowsums(m) + tcrossprod( xnew, log(m) ) 
  rowMaxs(score)
}