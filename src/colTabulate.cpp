//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
NumericMatrix coltabulate(NumericMatrix x,int nroww){
  const int ncl=x.ncol();
  NumericMatrix f(nroww,ncl);
  for(int i=0;i<ncl;++i)
    f.column(i)=Tabulate(x.column(i),nroww);
  return f;
}

RcppExport SEXP Rfast_coltabulate(SEXP xSEXP,SEXP nrowwSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< int >::type nroww(nrowwSEXP);
    __result = wrap(coltabulate(x,nroww));
    return __result;
END_RCPP
}
