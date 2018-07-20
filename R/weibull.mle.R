weibull.mle <- function(x, tol = 1e-09, maxiters = 100) {
  .Call(Rfast_weibull_mle, x, tol,maxiters)
}
