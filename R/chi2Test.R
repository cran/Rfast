
chi2Test <- function(data,x,y,cs,dc) {
    .Call('Rfast_chi2Test', PACKAGE = 'Rfast',data,x,y,cs,dc)
}