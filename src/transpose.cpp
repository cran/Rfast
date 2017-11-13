//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
NumericMatrix transpose_sq(NumericMatrix x){
  int i,ncl=x.ncol(),u;
  for(i=1;i<ncl;++i)
    for(u=0;u<i;++u)
    	swap(x(u,i),x(i,u));
  return x;
}

RcppExport SEXP Rfast_transpose_sq(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(transpose_sq(x));
    return __result;
END_RCPP
}
