
Pmin <- function(x,y,na.rm = FALSE) {
  .Call('Rfast_pmin',PACKAGE = "Rfast",x,y,na.rm)
}