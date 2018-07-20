
g2Test_univariate_perm <- function(data,dc,nperm) {
    .Call(Rfast_g2Test_univariate_perm,data,dc,nperm)
}