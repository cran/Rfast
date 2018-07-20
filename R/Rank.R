

Rank <- function(x,method = "average",descending = FALSE) {
  .Call(Rfast_rank,x,method,descending)
}