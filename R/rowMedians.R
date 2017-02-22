
rowMedians <- function(x) {
  .Call('Rfast_colmeds', PACKAGE = 'Rfast',t(x))
}