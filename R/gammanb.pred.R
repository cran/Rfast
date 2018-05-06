gammanb.pred <- function(xnew, a, b) {
  score <-  - tcrossprod(b, xnew) + tcrossprod(a - 1, Rfast::Log(xnew)) - 
            Rfast::rowsums( Rfast::Lgamma(a) - a * Rfast::Log(b) )
  Rfast::colMaxs(score)
}