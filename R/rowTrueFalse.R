
rowTrueFalse <- function(x) {
  x <- .Call('Rfast_row_true_false', PACKAGE = 'Rfast',x)
  rownames(x) <- c("FALSE","TRUE")
  x
}