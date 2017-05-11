
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
NumericMatrix minkowski_dist(NumericMatrix x,const double p){
  const int ncl=x.ncol(),nrw=x.nrow();
  const double p_1=1.0/p;
  mat xx(x.begin(),nrw,ncl,false);
  NumericMatrix f(ncl,ncl);
  colvec xv(nrw);
  register double a;
  int i,j;
  for(i=0;i<ncl-1;++i){
    xv=xx.col(i);
    for(j=i+1;j<ncl;++j){      
      a=pow(sum_pow(abs(xv-xx.col(j)),p),p_1);
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}

RcppExport SEXP Rfast_minkowski_dist(SEXP xSEXP,SEXP pSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const double >::type p(pSEXP);
    __result = wrap(minkowski_dist(x,p));
    return __result;
END_RCPP
}
