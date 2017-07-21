ordinal.mle <- function(y, link = "logit") {
  ina <- tabulate(y)
  k <- length(ina)
  ni <- cumsum(ina)/length(y)
  if ( link == "logit" ) {
    a <- log( ni/(1 - ni) )[-k]
  } else  a <- qnorm(ni)[-k]
  loglik <- sum( ina * log( c(ni[1], diff(ni) ) ) )
  list(loglik = loglik, param = a)
}

 
