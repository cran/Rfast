//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
SEXP col_true_false(SEXP x){
	const int n=Rf_nrows(x);
	SEXP f=Rf_allocMatrix(INTSXP,2,Rf_ncols(x));
	int *ff=INTEGER(f),*xx=LOGICAL(x),*endx=xx+LENGTH(x),t;
	for(;xx!=endx;xx+=n,ff+=2){
		t=True(xx,xx+n);
		*ff=n-t;
		ff[1]=t;
	}
  	return f;
}

RcppExport SEXP Rfast_col_true_false(SEXP x){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = col_true_false(x);
    return __result;
END_RCPP
}
