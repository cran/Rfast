//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>
#include "mn.h"

using namespace Rcpp;

SEXP Round_simple(SEXP x,const int dg){
  const int n=LENGTH(x);
  SEXP f=PROTECT(Rf_duplicate(x));
  double *start=REAL(x),*end=start+n,*ff=REAL(f);
  for(;start!=end;++start,++ff)
    *ff=my_round_gen_simple(*start,dg);
  UNPROTECT(1);
  return f;
}

SEXP Round_na_rm(SEXP x,const int dg){
  const int n=LENGTH(x);
  SEXP f=PROTECT(Rf_duplicate(x));
  double *start=REAL(x),*end=start+n,*ff=REAL(f);
  for(;start!=end;++start,++ff)
    *ff=my_round_gen_na_rm(*start,dg);
  UNPROTECT(1);
  return f;
}


//[[Rcpp::export]]
SEXP Round(SEXP x,const int dg,const bool na_rm){
  return na_rm ? Round_simple(x,dg) : Round_na_rm(x,dg);
}

RcppExport SEXP Rfast_Round(SEXP x,SEXP dgSEXP,SEXP na_rmSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const int >::type dg(dgSEXP);
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);   
    __result = Round(x,dg,na_rm);
    return __result;
END_RCPP
}
