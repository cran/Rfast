//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
IntegerMatrix col_order(NumericMatrix x,const bool stable,const bool descending){
	const int ncl=x.ncol();
	IntegerMatrix f(x.nrow(),ncl);
  	for(int i=0;i<ncl;++i)
  		f.column(i)=Order(x.column(i),stable,descending);
  	return f;
}

RcppExport SEXP Rfast_col_order(SEXP xSEXP,SEXP stableSEXP,SEXP descendingSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    traits::input_parameter< const bool >::type descending(descendingSEXP);
    __result = wrap(col_order(x,stable,descending));
    return __result;
END_RCPP
}

//[[Rcpp::export]]
IntegerMatrix row_order(NumericMatrix x,const bool stable,const bool descending){
  const int nrw=x.nrow();
  IntegerMatrix f(nrw,x.ncol());
    for(int i=0;i<nrw;++i)
      f.row(i)=Order(x.row(i),stable,descending);
  return f;
}

RcppExport SEXP Rfast_row_order(SEXP xSEXP,SEXP stableSEXP,SEXP descendingSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    traits::input_parameter< const bool >::type descending(descendingSEXP);
    __result = wrap(row_order(x,stable,descending));
    return __result;
END_RCPP
}
