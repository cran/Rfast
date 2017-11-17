
Round <- function(x, digit=0, na.rm = FALSE) {
  .Call('Rfast_Round',PACKAGE = "Rfast",x,digit,na.rm)
}