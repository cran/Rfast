
squareform <- function(x) {
  a <- .Call('Rfast_squareform_c',x)
  a + t(a)
}