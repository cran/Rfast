//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
IntegerVector Match(NumericVector x,NumericVector key){
  return match(x,key);
}

RcppExport SEXP Rfast_Match(SEXP xSEXP,SEXP keySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type key(keySEXP);
    __result = wrap(Match(x,key));
    return __result;
END_RCPP
}
