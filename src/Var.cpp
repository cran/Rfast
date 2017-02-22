
//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;

//[[Rcpp::export]]
double var_c(NumericVector x){
  const int n = x.size();
  double *xx=&x[0],*end=xx+n,sum1=0,sum2=0,v;
  for(;end-xx;++xx){
    v=*xx;
    sum1+=v*v;
    sum2+=v;
  }
  return (sum1-sum2*sum2/n)/(n-1);
}

RcppExport SEXP Rfast_var_c(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    __result = var_c(x);
    return __result;
END_RCPP
}
