gaussiannb.pred <- function(xnew, m, s, ni) {
  con <- 2 * log( ni )
  dets <- Rfast::rowsums( log(s) )
  xnew <- t(xnew)
  k <- dim(m)[1]
  mat <- matrix(nrow = dim(xnew)[2], ncol = k)
  for (j in 1:k)  mat[, j] <-  - Rfast::colsums( (xnew - m[j, ])^2 / s[j, ] ) - dets[j] + con[j]
  Rfast::rowMaxs(mat)
}
  