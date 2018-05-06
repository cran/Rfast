######### Simulation using any symmetric A matrix
rbingham <- function(n, A) {
  p <- dim(A)[2]  ## dimensionality of A
  eig <- eigen(A)
  lam <- eig$values  ## eigenvalues
  V <- eig$vectors  ## eigenvectors
  lam <- lam - lam[p]
  lam <- lam[-p]
  x <- Rfast::rbing(n, lam)
  ## the x contains the simulated values
  tcrossprod(x, V) ## simulated data
}
