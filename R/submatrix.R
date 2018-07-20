
submatrix <- function(x,rowStart=1,rowEnd=1,colStart=1,colEnd=1) {
  .Call(Rfast_submatrix,x,rowStart,rowEnd,colStart,colEnd)
}