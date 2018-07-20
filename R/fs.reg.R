
fs.reg <- function(y,ds,sig = 0.05,tol = 2,type = "logistic") {
	x <- .Call(Rfast_fs_reg,y,ds,sig,tol,type)
	colnames(x) <- c("vars","log.pval","stat","bic")
	x
}