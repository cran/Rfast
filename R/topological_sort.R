topological_sort <- function (dag) {
  a <- .Call("Rfast_topological_sort", PACKAGE = "Rfast", dag) + 1
  if ( sort_unique.length(a) != dim(dag)[2] ) a <- NA
  a
}