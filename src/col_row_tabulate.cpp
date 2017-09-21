//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
NumericMatrix col_tabulate(NumericMatrix x,int nroww){
  const int ncl=x.ncol();
  NumericMatrix f(nroww,ncl);
  for(int i=0;i<ncl;++i)
    f.column(i)=Tabulate(x.column(i),nroww);
  return f;
}

//[[Rcpp::export]]
NumericMatrix row_tabulate(NumericMatrix x,int ncoll){
  const int nrw=x.nrow();
  NumericMatrix f(nrw,ncoll);
  for(int i=0;i<nrw;++i)
    f.row(i)=Tabulate(x.row(i),ncoll);
  return f;
}

RcppExport SEXP Rfast_row_tabulate(SEXP xSEXP,SEXP ncollSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< int >::type ncoll(ncollSEXP);
    __result = wrap(row_tabulate(x,ncoll));
    return __result;
END_RCPP
}

RcppExport SEXP Rfast_col_tabulate(SEXP xSEXP,SEXP nrowwSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< int >::type nroww(nrowwSEXP);
    __result = wrap(col_tabulate(x,nroww));
    return __result;
END_RCPP
}
