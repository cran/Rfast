multinom.nb <- function(xnew, x, ina) {
  nu <- tabulate(ina)
  x <- x / Rfast::rowsums(x)  ## normalizes the data, so that each observation sums to 1
  m <- rowsum(x, ina) / nu
  score <- tcrossprod( xnew, log(m) )
  rowMaxs(score)
}