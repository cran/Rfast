//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace std;
using namespace arma;

// [[Rcpp::export]]
colvec row_sums(NumericMatrix x){
  mat X = mat(x.begin(), x.nrow(), x.ncol(), false); 
  return sum(X, 1); 
}

RcppExport SEXP Rfast_row_sums(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(row_sums(x));
    return __result;
END_RCPP
}
