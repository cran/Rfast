gammanb.pred <- function(xnew, a, b) {
  score <-  - tcrossprod(b, xnew) + tcrossprod(a - 1, Log(xnew)) - 
            rowsums( Lgamma(a) - a * Log(b) )
  colMaxs(score)
}