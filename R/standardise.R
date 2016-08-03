standardise <- function(x, center = TRUE, scale = TRUE) {

  if ( center == TRUE & scale == TRUE ) {
    m <- as.vector(colmeans(x))
    s <- as.vector(colVars(x, std = TRUE))
    y <- ( t(x) - m ) / s

  } else if ( center == TRUE & scale == FALSE )  {

    m <- as.vector(colmeans(x))
    s <- colVars(x, std = TRUE)
    y <- ( t(x) - m )

  } else if ( center == FALSE & scale == TRUE )  {

    s <- colVars(x, std = TRUE)
    y <- t(x) / s
  }

  t(y)

}