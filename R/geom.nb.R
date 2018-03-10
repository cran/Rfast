geom.nb <- function (xnew, x, ina, type = 1) {
    ni <- tabulate(ina)
	ni <- ni[ni > 0]
    if (type == 1) {
        si <- rowsum(x, ina)
        prob <- 1/(1 + si/ni)
        score <- rowsums( log(prob) ) + tcrossprod(log(1 - prob), xnew)
    }   else {
        prob <- ni/rowsum(x, ina)
        score <- rowsums( log(prob) ) + tcrossprod(log(1 - prob), xnew)
    }
    colMaxs(score)
}
