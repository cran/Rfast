
design_matrix <- function(x,ones=TRUE) {
  .Call('Rfast_design_matrix', PACKAGE = 'Rfast',x,ones)
}