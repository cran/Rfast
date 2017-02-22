
Lgamma <- function(x) {
  .Call('Rfast_Lgamma', PACKAGE = 'Rfast',x)
}