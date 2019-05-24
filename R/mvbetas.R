#[export]
mvbetas <- function (y, x, pvalue = FALSE) {

    n <- dim(y)[1]
    my <- Rfast::colmeans(y)
    mx <- sum(x)/n
    sx <- ( sum(x^2) - sum(x)^2 / n ) / (n - 1)
    denom <- n - 1    
    r <- ( Rfast::eachcol.apply(y, x) - n * mx * my ) /denom
    be <- r/sx
    a <- my - be * mx
    
    if ( !pvalue ) {
      result <- cbind(a, be)
      if ( is.null( colnames(y) ) ) {
         rownames(result) <- paste("Y", 1:ncol(y), sep = "")
      }  else  rownames(result) <- colnames(x)

    } else {
      sy <- Rfast::colVars(y, suma = n * my, std = TRUE)
      rho <- r/(sqrt(sx) * sy)
      sqdof <- sqrt(n - 2)
      ta <- rho * sqdof/sqrt(1 - rho^2)
      pvalue <- 2 * pt(abs(ta), n - 2, lower.tail = FALSE)
      result <- cbind(a, be, rho, pvalue)
      if ( is.null( colnames(y) ) ) {
        rownames(result) <- paste("Y", 1:ncol(y), sep = "")
      } else  rownames(result) <- colnames(x)
    }
    result
}
