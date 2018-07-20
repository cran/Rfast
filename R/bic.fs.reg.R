bic.fs.reg <- function(y, x, tol = 2, type = "logistic") {
  ret <- .Call(Rfast_bic_fs_reg,y, x, tol, type)
  colnames(ret) <- c("vars", "bic")
  ret
}
