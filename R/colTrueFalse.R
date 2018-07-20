
colTrueFalse <- function(x) {
  x <- .Call(Rfast_col_true_false,x)
  rownames(x) <- c("FALSE","TRUE")
  x
}