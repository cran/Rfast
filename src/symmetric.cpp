//Author: Manos Papadakis


#include <RcppArmadillo.h>

using namespace Rcpp;

bool symmetric(NumericMatrix x){
  int ncl=x.ncol(),i,j;
  for(i=1;i<ncl;++i)
    for(j=0;j<i;++j)
      if(x(j,i)!=x(i,j))
        return false;
  return true;
}

RcppExport SEXP Rfast_symmetric(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(symmetric(x));
    return __result;
END_RCPP
}
