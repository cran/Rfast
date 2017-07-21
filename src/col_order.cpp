//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
IntegerMatrix col_order(NumericMatrix x,const bool stable){
	const int ncl=x.ncol();
	IntegerMatrix f(x.nrow(),ncl);
  	for(int i=0;i<ncl;++i)
  		f.column(i)=Order(x.column(i),stable);
  return f;
}

RcppExport SEXP Rfast_col_order(SEXP xSEXP,SEXP stableSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    __result = wrap(col_order(x,stable));
    return __result;
END_RCPP
}
