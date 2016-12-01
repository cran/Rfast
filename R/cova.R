cova <- function(x) {
    mat <- t(x) - colmeans(x)
    tcrossprod(mat) / ( dim(x)[1] - 1 )
}
