
pc.skel <- function(dataset, method = "pearson", alpha = 0.05, R = 1) {
  .Call("Rfast_pc_skel",PACKAGE = "Rfast",dataset, method, alpha, R)
}