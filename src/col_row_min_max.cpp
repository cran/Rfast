//Author: Manos Papadakis
#include <RcppArmadillo.h>
#include <Rinternals.h>
#include <R.h>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
SEXP col_min_max(SEXP x){
  const int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP F;
  switch(TYPEOF(x)){
  	case REALSXP:{
  		F=PROTECT(Rf_allocMatrix(REALSXP,2,ncol));
  		double *xx=REAL(x),*end=xx+LENGTH(x),*f=REAL(F),min,max;
  		for(;xx!=end;xx+=nrow,f+=2){
  			min_max_d(xx,xx+nrow,min,max);
			*f=min;
			f[1]=max;
	  	}
	  	break;
  	}
  	default:{
  		F=PROTECT(Rf_allocMatrix(INTSXP,2,ncol));
  		int *xx=INTEGER(x),*end=xx+LENGTH(x),*f=INTEGER(F),min,max;
		for(;xx!=end;xx+=nrow,f+=2){
		    min_max_i(xx,xx+nrow,min,max);
		    *f=min;
		    f[1]=max;
		}
  	}
  }
  UNPROTECT(1);
  return F;
}


RcppExport SEXP Rfast_col_min_max(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = col_min_max(x);
    return __result;
END_RCPP
}



//[[Rcpp::export]]
SEXP row_min_max(SEXP x){
  int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP F;
  F=PROTECT(Rf_allocMatrix(REALSXP,2,nrow));
  double *xx=REAL(x),*end=xx+ncol*nrow,*f=REAL(F),*x3,*ff;
  const double *endf=f+(nrow<<1);
  for(ff=f;ff!=endf;ff+=2,++xx)
    *ff=ff[1]=*xx;
  for(;xx!=end;)
    for(ff=f,x3=xx,xx+=nrow;x3!=xx;ff+=2,++x3){
      if(*ff>*x3)
        *ff=*x3;
      else if(ff[1]<*x3)
        ff[1]=*x3;
    }
    UNPROTECT(1);
  return F;
}


RcppExport SEXP Rfast_row_min_max(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = row_min_max(x);
    return __result;
END_RCPP
}
