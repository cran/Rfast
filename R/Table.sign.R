
Table.sign <- function(x,names = TRUE,useNA = FALSE) {
  .Call(Rfast_table_sign,x,useNA,names)
}