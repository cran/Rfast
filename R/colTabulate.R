

colTabulate <- function(x, max_number = max(x)) {
  .Call('Rfast_coltabulate',x,max_number)
}