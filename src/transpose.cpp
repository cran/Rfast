//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;

NumericMatrix transpose_sq(NumericMatrix x){
  int i,ncl=x.ncol(),u;
  NumericMatrix f=clone(x);
  for(i=1;i<ncl;++i)
    for(u=0;u<i;++u)
    	swap(f(u,i),f(i,u));
  return f;
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

NumericMatrix Rfast_transpose_g_p(NumericMatrix x){
    int i,p=x.ncol(),n=x.nrow();
    NumericMatrix f(p,n);
    mat ff(f.begin(),p,n,false),xx(x.begin(),n,p,false);
#pragma omp parallel for
    for(i=0;i<p;++i){
      ff.row(i)=xx.col(i).t();
  }
    return f;
}

RcppExport SEXP Rfast_transpose_g_p(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(Rfast_transpose_g_p(x));
    return __result;
END_RCPP
}
