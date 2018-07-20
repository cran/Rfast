glm_poisson <- function (x, y, full = FALSE,tol = 1e-09) {
    x <- model.matrix(y ~ ., data.frame(x))
    mod <- .Call(Rfast_glm_poisson, x, y, sum(y * log(y), na.rm = TRUE),tol)
    res <- list(be = mod$be, devi = mod$deviance)
    if (full) {
        be <- mod$be
        se <- chol2inv(chol(mod$L2))
        se <- sqrt(diag(se))
        wald <- be/se
        pval <- 2 * pnorm(abs(wald), lower.tail = FALSE)
        info <- cbind(be, se, wald, pval)
        colnames(info) <- c("estimate", "std error", "Wald stat", 
            "p-value")
        rownames(info) <- colnames(x)
        res <- list(info = info, devi = mod$deviance)
    }
    res
}
