
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
NumericVector group_max(NumericVector x,IntegerVector group,const int n){
  IntegerVector::iterator kk=group.begin();
  const double int_min=INT_MIN;
  NumericVector f(n,int_min);
  NumericVector::iterator xx=x.begin(),ff=f.begin(),rr;
  for(;xx!=x.end();++xx,++kk){
    f[*kk-1]=std::max(f[*kk-1],*xx);
  }
  int count_not_zero=0;
  for(;ff!=f.end();++ff){
    if(*ff!=int_min)
      ++count_not_zero;
  }
  NumericVector res(count_not_zero);
  for(rr=res.begin(),ff=f.begin();ff!=f.end();++ff){
    if(*ff!=int_min)
      *rr++=*ff;
  }
  return res;
}

RcppExport SEXP Rfast_group_max(SEXP xSEXP,SEXP groupSEXP,SEXP max_nSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    traits::input_parameter< const int >::type max_n(max_nSEXP);
    __result = wrap(group_max(x,group,max_n));
    return __result;
END_RCPP
}
