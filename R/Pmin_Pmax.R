
Pmin_Pmax <- function(x,y,na.rm = FALSE) {
  .Call('Rfast_pmin_pmax',PACKAGE = "Rfast",x,y,na.rm)
}