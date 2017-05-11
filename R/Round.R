
Round <- function(x, digit=0) {
  .Call('Rfast_Round',x,digit)
}