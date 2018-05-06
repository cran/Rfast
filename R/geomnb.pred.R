geomnb.pred <- function(xnew, prob) {
  score <- Rfast::rowsums(log(prob)) + tcrossprod(log(1 - prob), xnew)
  Rfast::rowMaxs(score)
}  
