
colMedians <- function(x) {
  .Call('Rfast_colmeds', PACKAGE = 'Rfast',x)
}