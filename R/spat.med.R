
spat.med <- function(x,tol = 1e-09) {
  as.vector(.Call('Rfast_spat_med', PACKAGE = 'Rfast',x,tol))
}