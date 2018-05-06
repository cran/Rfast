groupcorrels <- function(y, x, type ="pearson", ina) {
  p <- dim(x)[2]
  a <- Rfast::sort_unique(ina)
  d <- length(a) 
  res <- matrix(nrow = d, ncol = p)
  for (i in 1:d) res[i, ] <- cor(y[ina == i], x[ina == i, ], method = type)  
  rownames(res) <- a
  res
}
