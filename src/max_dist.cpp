
//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
NumericMatrix max_dist(NumericMatrix x){
  const int ncl=x.ncol(),nrw=x.nrow();
  mat xx(x.begin(),nrw,ncl,false);
  NumericMatrix f(ncl,ncl);
  colvec xv(nrw),tmp(nrw);
  double a;
  int i,j;
  for(i=0;i<ncl-1;++i){
    xv=xx.col(i);
    for(j=i+1;j<ncl;++j){
      tmp=abs(xv-xx.col(j));
      a=tmp.at(tmp.index_max());
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}

RcppExport SEXP Rfast_max_dist(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(max_dist(x));
    return __result;
END_RCPP
}
