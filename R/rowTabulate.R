

rowTabulate <- function(x, max_number = max(x)) {
  .Call(Rfast_row_tabulate,x,max_number)
}