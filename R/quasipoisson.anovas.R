quasipoisson.anovas <- function (y, ina, logged = FALSE) {
    ni <- tabulate(ina)
	ni <- ni[ni > 0]
    k <- length(ni)
    n <- sum(ni)
    si <- rowsum(y, ina)
    sy <- colsums(si)
    mi <- si/ni
    d1 <- colsums(si * log(mi))
    d0 <- sy * log(sy/n)
    up <- ( 2 * d1 - 2 * d0 ) / (k - 1)
    yi2 <- rowsum(y^2, ina)/mi
    phi <- ( colsums(yi2) - sy ) / (n - k)
    stat <- up / phi
    pvalue <- pf(stat, k - 1, n - k, lower.tail = FALSE, log.p = logged)
    cbind(stat, pvalue, phi)
}
