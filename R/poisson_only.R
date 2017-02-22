
poisson_only <- function(x,y) {
  .Call('Rfast_poisson_only', PACKAGE = 'Rfast',x,y,sum(y*log(y),na.rm=TRUE))
}