
//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;

double sum_mat(SEXP X){
  NumericVector x(X);
  arma::colvec f(x.begin(),x.size(),false);
  return arma::accu(f);
}

RcppExport SEXP Rfast_matrix_sum(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = wrap(sum_mat(x));
    return __result;
END_RCPP
}
