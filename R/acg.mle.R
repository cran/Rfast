acg.mle <- function (x, tol = 1e-07) {
    p <- dim(x)[2]
    n <- dim(x)[1]
    mu <- numeric(p)
    lam1 <- Rfast::cova(x)
    maha <- 1/Rfast::mahala(x, mu, lam1)
    down <- sum(maha)
    up <- crossprod(x * maha, x)
    lam2 <- up/down
    i <- 2
    while (sum(abs(lam2 - lam1)) > tol) {
        i <- i + 1
        lam1 <- lam2
        maha <- 1/Rfast::mahala(x, mu, lam1)
        down <- sum(maha)
        up <- crossprod(x * maha, x)
        lam2 <- up/down
    }
    A <- p * lam2
    if ( is.null(colnames(x)) )  colnames(A) <- rownames(A) <- paste("X", 1:p, sep = "")
    else colnames(A) <- rownames(A) <- colnames(x)
    list(iter = i, cova = A)
}
