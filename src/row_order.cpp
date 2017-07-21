//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
IntegerMatrix row_order(NumericMatrix x,const bool stable){
	const int nrw=x.nrow();
	IntegerMatrix f(nrw,x.ncol());
  	for(int i=0;i<nrw;++i)
  		f.row(i)=Order(x.row(i),stable);
  return f;
}

RcppExport SEXP Rfast_row_order(SEXP xSEXP,SEXP stableSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    __result = wrap(row_order(x,stable));
    return __result;
END_RCPP
}
