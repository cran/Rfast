
sort_cor_vectors <- function(x,y) {
  .Call('Rfast_sort_cor_vecs', PACKAGE = 'Rfast',x,y)
}