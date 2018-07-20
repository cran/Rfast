eigs.sym <- function(A, k) {
  .Call(Rfast_eigs_sym_c,A, k)
}