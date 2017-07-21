
//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
double sum_mat(SEXP X){
  NumericVector x(X);
  colvec f(x.begin(),x.size(),false);
  return arma::sum(f);
}

RcppExport SEXP Rfast_matrix_sum(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = wrap(sum_mat(x));
    return __result;
END_RCPP
}
