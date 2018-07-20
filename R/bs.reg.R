
bs.reg <- function(y, x, alpha = 0.05, type = "logistic") {
  .Call(Rfast_bs_reg,y, x, alpha, type)
}
