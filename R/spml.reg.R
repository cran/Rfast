spml.reg <- function(y, x, tol = 1e-07, seb = FALSE, maxiters = 100) {
  x <- model.matrix(~., data.frame(x) )
  y <- as.matrix(y)
  l <- .Call(Rfast_spml_reg, y, x, tol, seb, maxiters)
 
  if ( seb ) {
    colnames(l$seb) <- c("Cosinus of y", "Sinus of y")
    rownames(l$seb) <- colnames(x)    
  } else   l$seb <- NULL
  
  l
}