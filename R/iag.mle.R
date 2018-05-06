iag.mle <- function (x, tol = 1e-07) {
    n <- dim(x)[1]
    mod <- Rfast::vmf.mle(x)
    ka <- mod$kappa
    x23 <- crossprod(x) - n * diag(3)
    m1 <- mod$mu * sqrt(ka)
    a <- as.vector(x %*% m1)
    a2 <- a^2
    pa <- pnorm(a)
    da <- dnorm(a)
    gm <- pa + a2 * pa + a * da
    der <- 2 * (a * pa + da) * x
    sqa <- sqrt(pa) * x/sqrt(gm)
    sqa2 <- der/gm
    fm1 <- Rfast::colsums(a * x) - n * m1 + Rfast::colsums(sqa2)
    fm2 <- x23 + 2 * crossprod(sqa) - crossprod(sqa2)
    m2 <- m1 - solve(fm2, fm1)
    i <- 2
    while (sum(abs(m2 - m1)) > tol) {
        m1 <- m2
        a <- as.vector(x %*% m1)
        a2 <- a^2
        pa <- pnorm(a)
        da <- dnorm(a)
        gm <- pa + a2 * pa + a * da
        der <- 2 * (a * pa + da) * x
        sqa2 <- der/gm
        fm1 <- Rfast::colsums(a * x) - n * m1 + Rfast::colsums(sqa2)
        sqa <- sqrt(pa) * x/sqrt(gm)
        fm2 <- x23 + 2 * crossprod(sqa) - crossprod(sqa2)
        m2 <- m1 - solve(fm2, fm1)
        i <- i + 1
    }
    rl <- sum(m2^2)
    mesi <- rbind(m2, m2/sqrt(rl))
    rownames(mesi) <- c("Mean vector", "Mean direction")
    if ( is.null(colnames(x)) ) {
        colnames(mesi) <- c("X", "Y", "Z")
    } else  colnames(mesi) <- colnames(x)
    loglik <- -n * log(2 * pi) + 0.5 * sum(a2) - n/2 * rl + sum(log(gm))
    l0 <- -n * log(pi/0.25)
    param <- c(rl, loglik, l0)
    names(param) <- c("Norm of mean", "Log likelihood", "Uniform log-likelihood")
    list(iters = i, mesi = mesi, param = param)
}
