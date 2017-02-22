
logistic_only <- function(x,y) {
  .Call('Rfast_logistic_only', PACKAGE = 'Rfast',x,y)
}