ginis <- function(x) {
  n <- dim(x)[1]
  x <- sort_mat(x)
  g <- colsums(x * 1:n)
  2 * g/colsums(x) / n - (n + 1)/n
}
