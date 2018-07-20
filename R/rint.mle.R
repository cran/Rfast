rint.mle <- function(x, ina, ranef = FALSE, tol = 1e-09, maxiters = 100) {
  res <- .Call(Rfast_rint_mle,x,ina,ranef,tol,maxiters)
  names(res$info) <- c("sigma_tau", "sigma_errors", "log-lik")
  res      
}  