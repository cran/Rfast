poisson.nb <- function (xnew, x, ina) {
    nu <- tabulate(ina)
    m <- rowsum(x, ina)/nu
    score <-  -Rfast::rowsums(m) + tcrossprod(log(m), xnew)
    colMaxs(score)
}
