
g2tests_perm <- function(data,x,y,dc,nperm) {
    .Call(Rfast_g2tests_perm,data,x,y,dc,nperm)
}