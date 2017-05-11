
g2Test <- function(data,x,y,cs,dc) {
    .Call('Rfast_g2Test', PACKAGE = 'Rfast',data,x,y,cs,dc)
}