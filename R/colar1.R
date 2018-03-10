colar1 <- function (y, method = "cmle") {
    N <- dim(y)[1]
    if (method == "cmle") {
        dera2 <- N - 1
        derab <- colsums(y[-N, ])
        derb2 <- colsums(y[-N, ]^2)
        dera <- derab
        derb <- colsums(y[-N, ] * y[-1, ])
        cphi <- cbind(derb2 * dera - derab * derb, -derab * dera + 
            dera2 * derb)/(dera2 * derb2 - derab^2)
        s <- colsums((y[-1, ] - cphi[1] - cphi[2] * y[-N, ])^2)/dera2
        param <- cbind(cphi, s)
        colnames(param) <- c("constant", "phi", "sigma")
    }
    else if (method == "yw") {
        m <- colmeans(y)
        z <- eachrow(y, m, oper = "-")
        phi <- colsums(z[-1, ] * z[-N, ])/colsums(z^2)
        sigma <- (1 - phi^2) * colsums(z^2)/(N - 2)
        param <- cbind(m, phi, sigma)
        colnames(param) <- c("mean", "phi", "sigma")
    }
    param
}
