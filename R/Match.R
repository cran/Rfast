
Match <- function(x,key) {
  if(is.null(key))
  	stop("Key cant be Null.")
  .Call('Rfast_Match', PACKAGE = 'Rfast',x,key)
}