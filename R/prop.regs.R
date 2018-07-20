prop.regs <- function(y, x, varb = "quasi", tol = 1e-09, logged = FALSE, maxiters = 100) {
  stat <- .Call(Rfast_prop_regs,x,y, tol, varb,maxiters)
  pval <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
  cbind(stat, pval)
}
