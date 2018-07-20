
sort_unique.length <- function(x) {
  .Call(Rfast_len_sort_unique_int,x)
}