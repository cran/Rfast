
chi2tests <- function(data,x,y,dc) {
    .Call('Rfast_chi2tests', PACKAGE = 'Rfast',data,x,y,dc)
}