anovas <- function (x, ina, logged = FALSE) {
    ina <- as.numeric(ina)
    k <- max(ina)
    ni <- tabulate(ina)
    n <- dim(x)[1]
    sx2 <- colsums(x^2)
    m <- rowsum(x, ina)
    a <- colsums(m^2/ni)
    b <- colsums(m)^2/n
    mst <- (a - b) / (k - 1)
    mse <- (sx2 - a) / (n - k)
    fa <- mst / mse
    pvalue <- pf(fa, k - 1, n - k, lower.tail = FALSE, log.p = logged)
    tab <- cbind(fa, pvalue)
    colnames(tab) <- c("F value", "p-value")
    if (!is.null(colnames(x))) 
        rownames(tab) <- colnames(x)
    tab
}