
Pmax <- function(x,y,na.rm = FALSE) {
  .Call('Rfast_pmax',PACKAGE = "Rfast",x,y,na.rm)
}