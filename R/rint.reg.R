rint.reg <- function (y, x, id, tol = 1e-08, ranef = FALSE, maxiters = 100) {
    x <- cbind(1, x)
    mod <- .Call("Rfast_rint_reg", PACKAGE = "Rfast", x, y, id, tol, ranef, maxiters)
    names(mod$info) <- c("iters", "sigma_tau", "sigma_errors", "log-lik", 
        "deviance", "BIC")
    mod
}
