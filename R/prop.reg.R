prop.reg <- function (y, x, varb = "quasi", tol = 1e-09) {
    X <- model.matrix(~., data.frame(x))
    L <- .Call("Rfast_prop_reg", X, y)
    der2 <- L$der2
    u <- y - as.vector(L$p)
    bnew <- as.vector(L$be)
    Ainv <- solve(der2)
    if (varb == "quasi") {
        B <- crossprod(u * X)
        vb <- Ainv %*% B %*% Ainv
    }
    else {
      dm <- dim(X)
      dof <- dm[1] - dm[2] 
      vb <- sum((y - L$p)^2/L$p/(1 - L$p))/dof * Ainv
    }
    info <- cbind(bnew, sqrt(diag(vb)), bnew^2/diag(vb))
    info <- cbind(info, pchisq(info[, 3], 1, lower.tail = FALSE))
    rownames(info) <- colnames(X)
    colnames(info) <- c("Estimate", "Std. error", "Wald", "p-value")
    list(iters = L$i, varb = vb, info = info)
}