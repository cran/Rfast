
Pmax <- function(x,y) {
  .Call('Rfast_pmax',PACKAGE = "Rfast",x,y)
}