//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>
#include <R.h>
#include <Rinternals.h>
#include "mn.h"

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
NumericVector col_max_indices(NumericMatrix x){
  unsigned int i=0,p=x.ncol();
  mat X = mat(x.begin(), x.nrow(), p, false);
  NumericVector F(p);
  NumericVector::iterator FF=F.begin();
    for(;i<p;++i,++FF)
      *FF=(X.col(i)).index_max()+1;
  return F;
}

//[[Rcpp::export]]
SEXP col_max(SEXP x){
  int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP F;
  switch(TYPEOF(x)){
  	case REALSXP:{
  		F=PROTECT(Rf_allocVector(REALSXP,ncol));
  		double *xx=REAL(x),*end=xx+ncol*nrow,*f=REAL(F);
		for(;xx!=end;xx+=nrow,++f)
		    max_d(xx,xx+nrow,*f);
	  	break;
  	}
  	default:{
  		F=PROTECT(Rf_allocVector(INTSXP,ncol));
		int *xx=INTEGER(x),*end=xx+ncol*nrow,*f=INTEGER(F);
		for(;xx!=end;xx+=nrow,++f)
		    max_i(xx,xx+nrow,*f);
	  	break;
  	}
  }
  UNPROTECT(1);
  return F;
}

// find the maximum index of its collumn
RcppExport SEXP Rfast_col_max_indices(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(col_max_indices(x));
    return __result;
END_RCPP
}

// find the maximum value of its collumn
RcppExport SEXP Rfast_col_max(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = col_max(x);
    return __result;
END_RCPP
}
