dirknn <- function(xnew, x, y, k, type = "C", parallel = FALSE) {
  .Call('Rfast_dir_knn', PACKAGE = 'Rfast',t(xnew),t(x),y,k,type,parallel)
}
