standardise <- function (x, center = TRUE, scale = TRUE) {
  if (center & scale) {
    y <- t(x) - colmeans(x)
    y <- y / sqrt( rowsums(y^2) ) * sqrt( (dim(x)[1] - 1) )
	y <- t(y)
  } else if (center & !scale) {
    m <- colmeans(x)
    y <- eachrow(x, m, oper ="-" )
  } else if (!center & scale) {
    s <- colVars(x, std = TRUE)
    y <- eachrow(x, s, oper = "/")
  }
  y
}
