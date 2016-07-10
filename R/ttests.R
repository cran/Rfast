ttests <- function(x, ina, paired = FALSE) {

  ina <- as.numeric(ina)
  x1 <- x[ ina == 1, ]
  x2 <- x[ ina == 2, ]

  if ( paired == FALSE ) {

    m1 <- colMeans(x1)
    m2 <- colMeans(x2)
    s1 <- colVars(x1)
    s2 <- colVars(x2)

    n1 <- sum( ina == 1 )
    n2 <- length(ina) - n1
    f1 <- s1 / n1
    f2 <- s2 / n2
    fac <-f1 + f2
    va <- sqrt(fac)
    dof <- fac^2 / ( f1^2 / (n1 - 1) + f2^2/(n2 - 1) )

    stat <- ( m1 - m2 )/ sqrt(fac)
    pvalue <- 2 * pt( abs(stat), dof, lower.tail = FALSE )
    result <- cbind(stat, pvalue, dof)

  } else {

    n1 <- sum( ina == 1 )
    n2 <- sum( ina == 2 )

    if ( n1 != n2 ) {
      result = paste("The sample sizes are not equal, no paired samples t-test was performed")

    } else {

      n <- n1
      z <- x1 - x2

      m <- colMeans(z)
      s <- colVars(z)
      stat <- m / sqrt(s/n)
      pvalue <- 2 * pt( abs(stat), n - 1, lower.tail = FALSE )
      result <- cbind(stat, pvalue)
    }

  }

  result
}
