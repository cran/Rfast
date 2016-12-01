cora <- function(x) {
    mat <- t(x) - colmeans(x)
    mat <- mat / sqrt( rowsums(mat^2) )
    tcrossprod(mat)
}