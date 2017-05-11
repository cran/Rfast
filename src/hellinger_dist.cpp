
//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
NumericMatrix hellinger_dist(NumericMatrix x,const bool sqr){
  const int ncl=x.ncol(),nrw=x.nrow();
  const double p=1.0/std::sqrt(2.0);
  mat xx(x.begin(),nrw,ncl,false);
  NumericMatrix f(ncl,ncl);
  colvec xv(nrw);
  register double a;
  int i,j;
  if(sqr)
    for(i=0;i<ncl-1;++i){
      xv=xx.col(i);
      for(j=i+1;j<ncl;++j){
        a=sum(square(xv-xx.col(j)))*0.5;
        f(i,j)=a;
        f(j,i)=a;
      }
    }
  else
  	for(i=0;i<ncl-1;++i){
      xv=xx.col(i);
      for(j=i+1;j<ncl;++j){
        a=p*std::sqrt(sum(square(xv-xx.col(j))));
        f(i,j)=a;
        f(j,i)=a;
      }
    }
  return f;
}

RcppExport SEXP Rfast_hellinger_dist(SEXP xSEXP,SEXP sqrSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type sqr(sqrSEXP);
    __result = wrap(hellinger_dist(x,sqr));
    return __result;
END_RCPP
}
