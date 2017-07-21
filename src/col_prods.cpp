//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace std;
using namespace arma;

// [[Rcpp::export]]
rowvec col_prods(NumericMatrix x){
  mat X = mat(x.begin(), x.nrow(), x.ncol(), false); 
  return prod(X, 0); 
}

RcppExport SEXP Rfast_col_prods(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(col_prods(x));
    return __result;
END_RCPP
}
