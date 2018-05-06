
chi2Test_univariate <- function(data,dc) {
    .Call('Rfast_chi2Test_univariate', PACKAGE = 'Rfast',data,dc)
}