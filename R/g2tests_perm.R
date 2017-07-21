
g2tests_perm <- function(data,x,y,dc,nperm) {
    .Call('Rfast_g2tests_perm', PACKAGE = 'Rfast',data,x,y,dc,nperm)
}