// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <algorithm>
#include "mn.h"

using namespace std;
using namespace Rcpp;


//[[Rcpp::export]]
NumericVector group_med(NumericVector x,IntegerVector group){
  const int n=x.size(),n_1=n+1;
  IntegerVector::iterator kk=group.begin();
  NumericVector f(n);
  pr<double,int> *y=new pr<double,int>[n_1];
  pr<int,int> *ind=new pr<int,int>[n];
  int i,j=0,c=0;
  NumericVector::iterator xx=x.begin();
  for(i=0;xx!=x.end();++xx,++kk,++i){
    y[i].first=*xx;
    y[i].second=*kk-1;
  }
  y[n]=pr<double,int>();
  sort(y,y+n,my_compare_order_second);
  for(i=1;i<n_1;++i){
    if(y[j].second!=y[i].second){
      ind[y[j].second].first=j;
      ind[y[j].second].second=i;
      ind[y[j].second].is_good=true;
      ++c;
      j=i;
    }
    f[i-1]=y[i-1].first;
  }
  NumericVector F(c);
  for(int i=0,k=0;i<n;++i){
    if(ind[i].is_good){
      F[k++]=med_helper(f.begin()+ind[i].first,f.begin()+ind[i].second);
    }
  }
  delete[] y;
  delete[] ind;
  return F;
}


RcppExport SEXP Rfast_group_med(SEXP xSEXP,SEXP groupSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    __result = wrap(group_med(x,group));
    return __result;
END_RCPP
}
