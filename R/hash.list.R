
hash.list <- function(key,x) {
  .Call(Rfast_Hash_list,key,x)
}