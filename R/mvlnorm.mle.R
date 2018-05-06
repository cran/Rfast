mvlnorm.mle <- function(x) {
  dm <- dim(x)
  d <- dm[2]
  n <- dm[1]
  y <- Rfast::Log(x)  ## transform the data to the whole of R^d
  m1 <- Rfast::colmeans(y)  ## mean vector of y
  sigma <- crossprod(y)/n - tcrossprod(m1)
  a <- n * d * log(2 * pi) + n * log(det(s)) + n * d - sum(y)
  
  s1 <- diag(sigma)
  m <- exp( m1 + 0.5 * s1 )  ## mean vector of x
  
  m2 <- outer(m1, m1, "+")
  s2 <- outer(s1, s1, "+")
  s <- exp( m2 + 0.5 * s2 ) * ( exp(sigma) - 1 ) 

  list(loglik = -0.5 * a, mu = m1, sigma = sigma, m = m, s = s)
}