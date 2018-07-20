
hash.find <- function(x,key) {
  .Call(Rfast_hash_find,x,key)
}