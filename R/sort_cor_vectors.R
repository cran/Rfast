
sort_cor_vectors <- function(x, base, stable = FALSE, descending = FALSE) {
  x[Rfast::Order(base,stable,descending)]
}