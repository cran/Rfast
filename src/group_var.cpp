// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "mn.h"

using namespace std;
using namespace Rcpp;

struct var_h{
  double x2;
  double x;
  int n;
  bool is_good;
  var_h(){n=x=x2=0;is_good=false;}
};

//[[Rcpp::export]]
NumericVector group_var(NumericVector x,IntegerVector group,const int n){
  IntegerVector::iterator kk=group.begin();
  var_h *y=new var_h[n];
  int i,c=0,k;
  double xxx;
  NumericVector::iterator xx=x.begin();
  for(i=0;xx!=x.end();++xx,++kk,++i){
    k=*kk-1;
    xxx=*xx;
    y[k].x2+=xxx * xxx;
    y[k].x+=xxx;
    y[k].n++;
    y[k].is_good=true;
  }
  for(i=0;i<n;++i){
    if(y[i].is_good){
      y[i].x*=y[i].x;
      ++c;
    }
  }
  NumericVector F(c);
  double x2,x1;
  int ni;
  for(i=0,k=0;i<n;++i){
    if(y[i].is_good){
      x2=y[i].x2;
      x1=y[i].x;
      ni=y[i].n;
      F[k++]=(x2-x1/ni)/(ni-1);
    }
  }
  delete[] y;
  return F;
}


RcppExport SEXP Rfast_group_var(SEXP xSEXP,SEXP groupSEXP,SEXP nSEXP) {
  BEGIN_RCPP
  RObject __result;
  RNGScope __rngScope;
  traits::input_parameter< NumericVector >::type x(xSEXP);
  traits::input_parameter< IntegerVector >::type group(groupSEXP);
  traits::input_parameter< const int  >::type n(nSEXP);
  __result = wrap(group_var(x,group,n));
  return __result;
  END_RCPP
}
