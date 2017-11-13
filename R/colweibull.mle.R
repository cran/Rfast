colweibull.mle <- function (x, tol = 1e-09, maxiters = 100, parallel = FALSE) {
    res <- .Call("Rfast_colweibull_mle", PACKAGE = "Rfast", x, 
        tol, maxiters, parallel)
    colnames(res) <- c("shape", "scale", "log-lik")
    res
}
