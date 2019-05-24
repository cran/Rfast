#[export]
score.weibregs <- function (y, x, logged = FALSE) {
    mod <- Rfast::weibull.mle(y)
    k <- mod$param[1]
    lam <- mod$param[2]
    yk <- y^k
    u <- Rfast::eachcol.apply(x, yk)/lam^k - Rfast::colsums(x)
    vu <- Rfast::colsums(x^2)
    stat <- u^2/vu
    pvalue <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
    cbind(stat, pvalue)
}
