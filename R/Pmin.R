
Pmin <- function(x,y) {
  .Call('Rfast_pmin',PACKAGE = "Rfast",x,y)
}