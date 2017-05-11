//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
IntegerVector Match(NumericVector x,IntegerVector key){
  const int n=key.size()+1;
  IntegerVector f(n-1);
  IntegerVector::iterator F=f.begin();
  IntegerVector::iterator a=key.begin();
  NumericVector::iterator bg=x.begin();
  int t;
  sort(x.begin(),x.end());
  for(;a!=key.end();++a,++F){
    t=lower_bound(bg,x.end(),*a)-bg+1;
    *F = (t!=n && *a>=*bg) ? t : NA_INTEGER;
  }
  return f;
}

RcppExport SEXP Rfast_Match(SEXP xSEXP,SEXP keySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type key(keySEXP);
    __result = wrap(Match(x,key));
    return __result;
END_RCPP
}
