// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "mn.h"

using namespace std;
using namespace Rcpp;


//[[Rcpp::export]]
LogicalVector group_all(LogicalVector x,IntegerVector group,const int n){
  IntegerVector::iterator kk=group.begin();
  pr<int,int> *y=new pr<int,int>[n];
  int i,c=0,k;
  LogicalVector::iterator xx=x.begin();
  for(;xx!=x.end();++xx,++kk){
    k=*kk-1;
    y[k].first+=*xx;
    y[k].second++;
    y[k].is_good=true;
  }
  for(i=0;i<n;++i){
    if(y[i].is_good){
    	++c;
    }
  }
  LogicalVector F(c);
  for(i=0,k=0;i<n;++i){
    if(y[i].is_good){
      F[k++]= y[i].first==y[i].second;
    }
  }
  delete[] y;
  return F;
}


RcppExport SEXP Rfast_group_all(SEXP xSEXP,SEXP groupSEXP,SEXP nSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< LogicalVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    traits::input_parameter< const int >::type n(nSEXP);
    __result = wrap(group_all(x,group,n));
    return __result;
END_RCPP
}
