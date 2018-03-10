
is_integer <- function(x) {
  .Call('Rfast_is_integer', PACKAGE = 'Rfast',x)
}