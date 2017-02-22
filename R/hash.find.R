
hash.find <- function(x,key) {
  .Call('Rfast_hash_find', PACKAGE = 'Rfast',x,key)
}