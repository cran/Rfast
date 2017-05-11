
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>

using namespace Rcpp;

//[[Rcpp::export]]
SEXP min_max_perc(SEXP x){
  const int n=LENGTH(x);
  SEXP f=Rf_allocVector(REALSXP,4);
  double *start=REAL(x),*end=start+n,mx,mn,pos=0,xx,*FF=REAL(f);
  mn=mx=*start;
  for(;start!=end;++start){
    xx=*start;
    if(xx>0) pos++;
    if(mn>xx)
      mn=xx;
    else if(mx<xx)
      mx=xx;
  }
  *FF=mn;
  FF[1]=mx;
  FF[3]=(pos/n)*100.0;
  FF[2]=100.0-FF[3];
  return f;
}

RcppExport SEXP Rfast_min_max_perc(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = min_max_perc(x);
    return __result;
END_RCPP
}
