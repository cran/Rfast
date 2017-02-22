
//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
NumericMatrix euclidean_dist(NumericMatrix x,bool sqr){
  const int ncl=x.ncol(),nrw=x.nrow();
  mat xx(x.begin(),nrw,ncl,false);
  NumericMatrix f(ncl,ncl);
  colvec xv(nrw);
  double a;
  int i,j;
  if(sqr)
    for(i=0;i<ncl-1;++i){
      xv=xx.col(i);
      for(j=i+1;j<ncl;++j){
        a=sum(square(xx.col(j)-xv));
        f(i,j)=a;
        f(j,i)=a;
      }
    }
  else
    for(i=0;i<ncl-1;++i){
      xv=xx.col(i);
      for(j=i+1;j<ncl;++j){
        a=std::sqrt(sum(square(xv-xx.col(j))));
        f(i,j)=a;
        f(j,i)=a;
      }
    }
  return f;
}

RcppExport SEXP Rfast_euclidean_dist(SEXP xSEXP,SEXP sqrSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< bool >::type sqr(sqrSEXP);
    __result = wrap(euclidean_dist(x,sqr));
    return __result;
END_RCPP
}
