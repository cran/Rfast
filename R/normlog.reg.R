normlog.reg <- function(y, x, tol = 1e-07, maxiters = 100) {
  x <- model.matrix(y ~ ., data.frame(x))
  mod <- .Call(Rfast_normlog_reg,y, x, tol, maxiters)
  mod
}
