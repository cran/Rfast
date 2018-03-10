geomnb.pred <- function(xnew, prob) {
  score <- rowsums(log(prob)) + tcrossprod(log(1 - prob), xnew)
  rowMaxs(score)
}  
