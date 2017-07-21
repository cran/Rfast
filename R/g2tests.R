
g2tests <- function(data,x,y,dc) {
    .Call('Rfast_g2tests', PACKAGE = 'Rfast',data,x,y,dc)
}