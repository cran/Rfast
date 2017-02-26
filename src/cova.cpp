//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;

//[[Rcpp::export]]
mat cova(NumericMatrix X){
  mat x = mat(X.begin(), X.nrow(), X.ncol());
  const int nrow1=x.n_rows-1;
  rowvec m=mean(x,0);
  x=x.each_row()-m;
  x=x.t()*x;
  return x/nrow1;
}

RcppExport SEXP Rfast_cova(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(cova(x));
    return __result;
END_RCPP
}
