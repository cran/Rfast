multinomnb.pred <- function(xnew, m) {
  score <- tcrossprod( xnew, log(m) )
  rowMaxs(score)
}