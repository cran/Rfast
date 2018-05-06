trim.colmeans <- function(x, trim = 0.05) {
  x <- Rfast::sort_mat(x)
  dm <- dim(x)
  n <- dm[1]
  cu <- floor(n * trim)
  Rfast::colmeans( Rfast::submatrix(x, cu + 1, n - cu , 1, dm[2]) )
}

