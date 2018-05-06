multinom.nb <- function(xnew, x, ina) {
  ni <- tabulate(ina)
  ni <- ni[ni > 0]
  x <- x / Rfast::rowsums(x)  ## normalizes the data, so that each observation sums to 1
  m <- rowsum(x, ina) / ni
  score <- tcrossprod( xnew, log(m) )
  Rfast::rowMaxs(score)
}