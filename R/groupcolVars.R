groupcolVars <- function(x, ina, std = FALSE){
  m <- rowsum(x, ina)
  m2 <- rowsum(x^2, ina)
  ni <- tabulate(ina)
  ni <- ni[ni > 0]
  s <- (m2 - m^2/ni) / (ni - 1)
  if ( std ) s <- sqrt(s)
  s
}
 