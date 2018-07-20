
med <- function(x,na.rm=FALSE) {
  .Call(Rfast_med,x,na.rm)
}