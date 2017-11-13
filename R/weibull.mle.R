weibull.mle <- function(x, tol = 1e-09, maxiters = 100) {
  .Call('Rfast_weibull_mle', PACKAGE = 'Rfast', x, tol,maxiters)
}
