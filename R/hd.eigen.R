hd.eigen <- function(x, center = TRUE, scale = FALSE) {
  n <- dim(x)[1]
  if (center & scale) {
        y <- t(x) - colmeans(x)
        y <- y/sqrt(rowsums(y^2)) * sqrt( n - 1 )
    }
    else if (center & !scale) {
        m <- colmeans(x)
        y <- t(x) - m
    }
    else if (!center & scale) {
        s <- colVars(x, std = TRUE)
        y <- t(x)/s
    }

  xx <- crossprod(y)/(n - 1)
  eigen(xx)$values
}   