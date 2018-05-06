eigs.sym <- function(A, k) {
  .Call("Rfast_eigs_sym_c",PACKAGE = "Rfast",A, k)
}