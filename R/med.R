
med <- function(x,na.rm=FALSE) {
  .Call('Rfast_med', PACKAGE = 'Rfast',x,na.rm)
}