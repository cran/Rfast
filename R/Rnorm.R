Rnorm <- function(n, m = 0, s = 1) {
  if (m == 0 & s == 1) {
    x <- RcppZiggurat::zrnorm(n)
  } else if (m == 0 & s != 1) {
    x <- RcppZiggurat::zrnorm(n) * s
  } else if (m != 0  & s == 1) {
    x <- RcppZiggurat::zrnorm(n) + m
  } else x <- RcppZiggurat::zrnorm(n) * s + m
  x
}
