dista <- function(xnew, x, type = "euclidean") {

  ## x must be matrix
  ## type can be either "euclidean" or "manhattan"

  n <- nrow(x)
  xnew <- as.matrix(xnew)

  if ( ncol(xnew) == 1 )   xnew <- t(xnew)

  nu <- nrow(xnew)

  disa <- matrix( 0, nu, n )

  if (type == "euclidean") {
    y <- t(x)

    for (i in 1:nu) {
      z <- y - xnew[i, ]
      disa[i, ] <- sqrt( colSums( z^2 ) )
    }

  } else if (type == "manhattan") {
    y <- t(x)

    for (i in 1:nu) {
      a <- y - xnew[i, ]
      disa[i, ] <- colSums( abs(a) )
    }
  }

  disa

}
