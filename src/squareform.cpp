//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;

NumericMatrix squareform_c(NumericVector x) {
  const int d = my_round(0.5 + sqrt( 1 + 8 * x.size() ) / 2.0);
  int i,j,s=0;
  NumericMatrix f(d,d);
  for(i=0;i<d;++i){
      for(j=i+1;j<d;++j,++s){
        f(j,i)=x(s);
      }
  }
  return f;
}


RcppExport SEXP Rfast_squareform_c(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    __result = wrap(squareform_c(x));
    return __result;
END_RCPP
}
