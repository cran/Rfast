knn <- function(xnew, y, x, k, dist.type = "euclidean", type = "C", method = "average", freq.option = 0) {
  .Call("Rfast_k_nn",PACKAGE = "Rfast",xnew, y, x, k, dist.type, type, method, freq.option)
}
