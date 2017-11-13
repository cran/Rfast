weib.reg <- function (y, x, full = FALSE, tol = 1e-07, maxiters = 100) {
    X <- model.matrix(y ~ ., data.frame(x))
    mod <- .Call("Rfast_weib_reg", PACKAGE = "Rfast", y, X, tol, 
        maxiters)
    rownames(mod$be) <- colnames(X)
    res <- list(iters = mod$iters, loglik = mod$loglik, shape = mod$shape, 
        be = mod$be)
    if (full) {
        be <- mod$be
        se <- spdinv(mod$der2)
        se <- sqrt(diag(se))
        wald <- be/se
        pval <- 2 * pnorm(abs(wald), lower.tail = FALSE)
        info <- cbind(be, se, wald, pval)
        colnames(info) <- c("estimate", "std error", "Wald stat", 
            "p-value")
        rownames(info) <- colnames(X)
        mod$be <- info
        res <- list(iters = mod$iters, loglik = mod$loglik, shape = mod$shape, 
            be = mod$be)
    }
    res
}
