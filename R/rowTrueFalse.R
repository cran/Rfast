
rowTrueFalse <- function(x) {
  x <- .Call(Rfast_row_true_false,x)
  rownames(x) <- c("FALSE","TRUE")
  x
}