Rnorm <- function(n, m = 0, s = 1) {
  if (m == 0 & s == 1) {
    x <- zrnorm(n)
  } else if (m == 0 & s != 1) {
    x <- zrnorm(n) * s
  } else if (m != 0  & s == 1) {
    x <- zrnorm(n) + m
  } else x <- zrnorm(n) * s + m
  x
}
