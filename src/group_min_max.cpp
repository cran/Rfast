
//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;

//[[Rcpp::export]]
NumericMatrix group_min_max(NumericVector x,IntegerVector group,const int n){
  IntegerVector::iterator kk=group.begin();
  const double int_max=INT_MAX;
  NumericVector mn(n,int_max),mx(n,(double)(INT_MIN));
  NumericVector::iterator xx=x.begin(),minv=mn.begin(),maxv=mx.begin();
  int k;
  for(;xx!=x.end();++xx,++kk){
    k=*kk-1;
    mx[k]=std::max(mx[k],*xx);
    mn[k]=std::min(mn[k],*xx);
  }
  int count_not_zero=0;
  for(;minv!=mn.end();++minv){
    if(*minv!=int_max)
      ++count_not_zero;
  }
  NumericMatrix res(2,count_not_zero);
  int i=0;
  for(minv=mn.begin(),maxv=mx.begin();minv!=mn.end();++minv,++maxv){
    if(*minv!=int_max){
      res(0,i)=*minv;
      res(1,i)=*maxv;
      ++i;
    }
  }
  return res;
}

RcppExport SEXP Rfast_group_min_max(SEXP xSEXP,SEXP groupSEXP,SEXP max_nSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    traits::input_parameter< const int  >::type max_n(max_nSEXP);
    __result = wrap(group_min_max(x,group,max_n));
    return __result;
END_RCPP
}
