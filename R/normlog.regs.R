normlog.regs <- function(y, x, tol = 1e-08, logged = FALSE, parallel = FALSE, maxiters = 100) {
  dm <- dim(x)
  n <- dm[1]
  ly <- log(y + 0.1)
  con <-  - n * log(2 * pi) - n  - 2 * normlog.mle(y)$loglik + n * log(n)
  be <- allbetas( ly, as.matrix(x) )
  mod <- .Call("Rfast_normlog_regs",PACKAGE = "Rfast",y,x,be,con,tol,logged,parallel,maxiters)
  colnames(mod) <- c("stat", "pval")
  mod
}