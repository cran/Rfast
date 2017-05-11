
g2Test_univariate <- function(data,dc) {
    .Call('Rfast_g2Test_univariate', PACKAGE = 'Rfast',data,dc)
}