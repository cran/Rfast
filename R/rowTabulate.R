

rowTabulate <- function(x, max_number = max(x)) {
  .Call('Rfast_rowtabulate',x,max_number)
}