weib.reg <- function (y, x, tol = 1e-07, maxiters = 100) {
    X <- model.matrix(y ~ ., data.frame(x))
    mod <- .Call(Rfast_weib_reg, y, X, tol, 
        maxiters)
    rownames(mod$be) <- colnames(X)
    list(iters = mod$iters, loglik = mod$loglik, shape = mod$shape, be = mod$be)
}
