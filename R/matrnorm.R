matrnorm <- function(n, p) {
  matrix( zrnorm(n * p), ncol = p)
}