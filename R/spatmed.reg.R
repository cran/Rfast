################################
#### Spatial median regression
#### Tsagris Michail 10/2014
#### Biman Chakraborty (2003) On multivariate quantile regression
#### Journal of Statistical Planning and Inference
#### http://www.stat.nus.edu.sg/export/sites/dsap/research/documents/tr01_2000.pdf
#### mtsagris@yahoo.gr
################################
spatmed.reg <- function(y, x, tol = 1e-07) {

  x <- model.matrix(y ~ ., data.frame(x) )
  p <- dim(x)[2]
  d <- dim(y)[2]

  B1 <- solve(crossprod(x), crossprod(x, y) )
  est <- y - x %*% B1
  ww <- sqrt( .Call(Rfast_row_sums, est^2, indices = NULL))
  ## ww <- sqrt( Rfast::rowsums( est^2 ) )
  z <- x / ww
  B2 <- solve( crossprod(z, x), crossprod(z, y) )
  i <- 2
  while ( sum( abs(B2 - B1) ) > tol ) {
    i <- i + 1
    B1 <- B2
    est <- y - x %*% B1
    ww <- sqrt( .Call(Rfast_row_sums, est^2, indices = NULL))
	## ww <- sqrt( Rfast::rowsums( est^2 ) )
    ela <- which( ww == 0 )
    z <- x / ww
    if ( length(ela) > 0 )  z[ela, ] <- 0
    B2 <- solve( crossprod(z, x), crossprod(z, y) )
  }
  be <- B2
  
  if ( is.null(colnames(y)) ) {
    colnames(be) <- paste("Y", 1:d, sep = "")
  } else  colnames(be) <- colnames(y)
  rownames(be)  <- colnames(x)

  list(iters = i, be = be)
}
