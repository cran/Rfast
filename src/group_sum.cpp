
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
SEXP min_max_h(SEXP x){
  SEXP F;
  int *xx=INTEGER(x),*end=xx+LENGTH(x);
  int xxx;
  F= PROTECT(Rf_allocVector(INTSXP, 2));
  int *f=INTEGER(F),min=*xx,max=min;
  for(xx++;xx!=end;++xx){
    xxx=*xx;
    if(xxx>max)
      max=xxx;
    else if(xxx<min)
      min=xxx;
  }
  *f=min;
  f[1]=max;
  UNPROTECT(1);
  return F;
}

//[[Rcpp::export]]
NumericVector group_sum(NumericVector x,IntegerVector key,SEXP minn,SEXP maxx){
  int mn,mx;
  const bool is_mn=Rf_isNull(minn),is_mx=Rf_isNull(maxx);
  if(is_mx & is_mn){
  	SEXP mnmx=min_max_h(key);
  	mn=INTEGER(mnmx)[0];
  	mx=INTEGER(mnmx)[1];
  }else if(is_mx){
  	mn=Rf_asInteger(minn);
  	mx=*std::max_element(key.begin(),key.end());
  }else if(is_mn){
  	mx=Rf_asInteger(maxx);
  	mn=*std::min_element(key.begin(),key.end());
  }else{
  	mx=Rf_asInteger(maxx);
  	mn=Rf_asInteger(minn);
  }
  IntegerVector::iterator kk=key.begin();
  NumericVector f(mx-mn+1);
  NumericVector::iterator xx=x.begin(),ff=f.begin(),rr;
  for(;xx!=x.end();++xx,++kk){
      f[*kk-mn]+=*xx;
  }
  int count_not_zero=0;
  for(;ff!=f.end();++ff){
    if(*ff!=0)
      ++count_not_zero;
  }
  NumericVector res(count_not_zero);
  for(rr=res.begin(),ff=f.begin();ff!=f.end();++ff){
    if(*ff!=0)
      *rr++=*ff;
  }
  return res;
}

RcppExport SEXP Rfast_group_sum(SEXP xSEXP,SEXP groupSEXP,SEXP minn,SEXP maxx) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    __result = wrap(group_sum(x,group,minn,maxx));
    return __result;
END_RCPP
}
