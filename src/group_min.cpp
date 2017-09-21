
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
NumericVector group_min(NumericVector x,IntegerVector group,const int n){
  IntegerVector::iterator kk=group.begin();
  const double int_max=INT_MAX;
  NumericVector f(n,int_max);
  NumericVector::iterator xx=x.begin(),ff=f.begin(),rr;
  for(;xx!=x.end();++xx,++kk){
    f[*kk-1]=std::min(f[*kk-1],*xx);
  }
  int count_not_zero=0;
  for(;ff!=f.end();++ff){
    if(*ff!=int_max)
      ++count_not_zero;
  }
  NumericVector res(count_not_zero);
  for(rr=res.begin(),ff=f.begin();ff!=f.end();++ff){
    if(*ff!=int_max)
      *rr++=*ff;
  }
  return res;
}

RcppExport SEXP Rfast_group_min(SEXP xSEXP,SEXP groupSEXP,SEXP max_nSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    traits::input_parameter< const int >::type max_n(max_nSEXP);
    __result = wrap(group_min(x,group,max_n));
    return __result;
END_RCPP
}
