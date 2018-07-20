
Pmin <- function(x,y,na.rm = FALSE) {
  .Call(Rfast_pmin,x,y,na.rm)
}