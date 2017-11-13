

Rank <- function(x,method = "average",descending = FALSE,stable = FALSE) {
  .Call("Rfast_rank",PACKAGE = "Rfast",x,method,descending,stable)
}