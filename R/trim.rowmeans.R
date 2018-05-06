trim.rowmeans <- function(x, trim = 0.05) {
  x <- Rfast::sort_mat(x, by.row = TRUE)
  dm <- dim(x)
  n <- dm[2]
  cu <- floor(n * trim)
  Rfast::rowmeans( Rfast::submatrix(x, 1, dm[1], cu + 1, n - cu) )
}

