//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
SEXP Round(SEXP x,const int dg){
  const int n=LENGTH(x);
  SEXP f=PROTECT(Rf_duplicate(x));
  double *start=REAL(x),*end=start+n,*ff=REAL(f);
  for(;start!=end;++start,++ff)
    *ff=my_round_gen(*start,dg);
  UNPROTECT(1);
  return f;
}

RcppExport SEXP Rfast_Round(SEXP x,SEXP dgSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const int >::type dg(dgSEXP);   
    __result = Round(x,dg);
    return __result;
END_RCPP
}
