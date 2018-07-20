
Pmax <- function(x,y,na.rm = FALSE) {
  .Call(Rfast_pmax,x,y,na.rm)
}