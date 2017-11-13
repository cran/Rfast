
bs.reg <- function(y, x, alpha = 0.05, type = "logistic") {
  .Call("Rfast_bs_reg",PACKAGE = "Rfast",y, x, alpha, type)
}
