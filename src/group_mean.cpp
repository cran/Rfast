
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
NumericVector group_mean(NumericVector x,IntegerVector group,const int n){
  IntegerVector::iterator kk=group.begin();
  pr<double,int> *f=new pr<double,int>[n];
  NumericVector::iterator xx=x.begin(),rr;
  int i;
  for(;xx!=x.end();++xx,++kk){
    f[*kk-1].first+=*xx;
    f[*kk-1].second++;
  }
  int count_not_zero=0;
  for(i=0;i<n;++i){
    if(f[i].second!=0)
      ++count_not_zero;
  }
  NumericVector res(count_not_zero);
  for(i=0,rr=res.begin();i<n;++i){
    if(f[i].second!=0)
      *rr++=f[i].first/f[i].second;
  }
  delete[] f;
  return res;
}

RcppExport SEXP Rfast_group_mean(SEXP xSEXP,SEXP groupSEXP,SEXP max_nSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    traits::input_parameter< const int >::type max_n(max_nSEXP);
    __result = wrap(group_mean(x,group,max_n));
    return __result;
END_RCPP
}
