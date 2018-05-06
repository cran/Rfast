

Rank <- function(x,method = "average",descending = FALSE) {
  .Call("Rfast_rank",PACKAGE = "Rfast",x,method,descending)
}