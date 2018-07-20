qpois.regs <- function (x, y, tol = 1e-09, logged = FALSE) {
  ylogy <- sum(y * log(y), na.rm = T)
  stat <- .Call(Rfast_qpois_regs,x, y, tol, ylogy, mean(y))
  pval <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
  ret <- cbind(stat, pval)
  colnames(ret) <- c("stat", "pval")
  ret
}
