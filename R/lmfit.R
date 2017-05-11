lmfit <- function(x, y, w = NULL) {
  if ( is.null(w) ) {
    be <- solve( crossprod(x), crossprod(x, y) )
  } else  be <- solve( crossprod(x, w * x), crossprod(x, w * y) )
  e <- y - x %*% be 
  list(be = be, residuals = e)
}
