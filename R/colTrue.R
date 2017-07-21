
colTrue <- function(x) {
  .Call('Rfast_col_true', PACKAGE = 'Rfast',x)
}