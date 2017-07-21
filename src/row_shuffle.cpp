//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
mat row_shuffle(mat x){
  return shuffle(x,1);
}

RcppExport SEXP Rfast_row_shuffle(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< mat >::type x(xSEXP);
    __result = row_shuffle(x);
    return __result;
END_RCPP
}
