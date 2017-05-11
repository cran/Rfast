
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
NumericMatrix kullback_leibler_dist(NumericMatrix x){
  const int ncl=x.ncol(),nrw=x.nrow();
  NumericMatrix f(ncl,ncl),log_x(nrw,ncl);
  mat xx(x.begin(),nrw,ncl,false),log_xx(log_x.begin(),nrw,ncl,false);
  colvec xv(nrw),log_xv(nrw);
  register double a;
  int i,j;
  fill_with_log(x.begin(),x.end(),log_xx.begin());
  for(i=0;i<ncl-1;++i){
    xv=xx.col(i);
    log_xv=log_xx.col(i);
    for(j=i+1;j<ncl;++j){
      a=sum((xv-xx.col(j))%(log_xv-log_xx.col(j)));
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}
RcppExport SEXP Rfast_kullback_leibler_dist(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(kullback_leibler_dist(x));
    return __result;
END_RCPP
}
