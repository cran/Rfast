//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;
using namespace arma;

// [[Rcpp::export]]
colvec row_means(NumericMatrix x){
  mat X = mat(x.begin(), x.nrow(), x.ncol(), false); 
  return mean(X, 1); 
}

RcppExport SEXP Rfast_row_means(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(row_means(x));
    return __result;
END_RCPP
}
