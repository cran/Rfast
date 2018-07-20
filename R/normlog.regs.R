normlog.regs <- function (y, x, tol = 1e-08, logged = FALSE, parallel = FALSE, 
    maxiters = 100)  {
    
   dm <- dim(x)
   n <- dm[1]
   ly <- log(y + 0.1)
   con <- Rfast::Var(y) * (n - 1)
   be <- allbetas( ly, x )

   mod <- .Call(Rfast_normlog_regs, y, x, be, con, tol, logged, parallel, maxiters)

    colnames(mod) <- c("stat", "pvalue")
    mod
}