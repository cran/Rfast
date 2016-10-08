standardise <- function(x, center = TRUE, scale = TRUE) {

  if ( center & scale ) {
    m <- colmeans(x)
    s <- colVars(x, std = TRUE)
    y <- ( t(x) - m ) / s

  } else if ( center & !scale )  {

    m <- colmeans(x)
    y <- ( t(x) - m )

  } else if ( !center & scale )  {

    s <- colVars(x, std = TRUE)
    y <- t(x) / s
  }

  t(y)

}