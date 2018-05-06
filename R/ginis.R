ginis <- function(x) {
  n <- dim(x)[1]
  x <- Rfast::sort_mat(x)
  g <- Rfast::colsums(x * 1:n)
  2 * g / n / Rfast::colsums(x) - (n + 1)/n
}
