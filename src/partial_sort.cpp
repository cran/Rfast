//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>
#include <algorithm>
#include "mn.h"

using namespace Rcpp;
using namespace std;

SEXP partial_sort(SEXP x,const int n,const bool descend){
  SEXP f=PROTECT(Rf_duplicate(x));
  int len=LENGTH(x);
  switch(TYPEOF(x)){
	  case INTSXP:{
	    int *F=INTEGER(f);
	    if(descend)
	    	nth_element(F,F+n-1,F+len,descending_int);
	    else 
	    	nth_element(F,F+n-1,F+len);
	    break;
	  }
	  default:{
	    double *F=REAL(f);
	    if(descend)
	    	nth_element(F,F+n-1,F+len,descending_double);
	    else 
	    	nth_element(F,F+n-1,F+len);
	    break;
	  }
  }
  UNPROTECT(1);
  return f;
}

RcppExport SEXP Rfast_partial_sort(SEXP x,SEXP nSEXP,SEXP descendSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const int >::type n(nSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    __result = partial_sort(x,n,descend);
    return __result;
END_RCPP
}
