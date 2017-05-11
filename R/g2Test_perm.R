
g2Test_perm <- function(data,x,y,cs,dc,nperm) {
    .Call('Rfast_g2Test_perm', PACKAGE = 'Rfast',data,x,y,cs,dc,nperm)
}