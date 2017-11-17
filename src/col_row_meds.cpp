//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>
#include <algorithm>
#include "templates.h"

using namespace Rcpp;
using namespace std;
using namespace arma;

SEXP col_meds_simple(NumericMatrix& x){
  const int p=x.ncol(),step=x.nrow(),middle=step/2-1;
  int i;
  NumericVector tmp(step);
  SEXP F=Rf_allocVector(REALSXP,p);
  double *FF=REAL(F);
  if(step%2==0)
    for(i=0;i<p;++i,++FF){
      tmp=x.column(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      *FF=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0;
    }
    else 
      for(i=0;i<p;++i,++FF){
        tmp=x.column(i);
        nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
        *FF=tmp[middle+1];
      }
      return F;
}

SEXP col_meds_na_rm(NumericMatrix& x){
  const int p=x.ncol();
  int i;
  NumericVector tmp;
  SEXP F=Rf_allocVector(REALSXP,p);
  double *FF=REAL(F);
  for(i=0;i<p;++i,++FF){
    tmp=x.column(i);
    *FF=med_helper<NumericVector>(tmp.begin(),tmp.begin()+(int)(std::remove_if(tmp.begin(),tmp.end(),R_IsNA)-tmp.begin()));
  }
  return F;
}

SEXP col_meds(NumericMatrix x,const bool na_rm){
  return na_rm ? col_meds_na_rm(x) : col_meds_simple(x);
}

// colMedians
RcppExport SEXP Rfast_col_meds(SEXP xSEXP,SEXP na_rmSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    __result = col_meds(x,na_rm);
    return __result;
END_RCPP
}


//[[Rcpp::export]]
SEXP row_meds(NumericMatrix x){
  const int sz=x.ncol(),p=x.nrow(),middle=sz/2-1;
  int i;
  NumericVector rowi(sz);
  SEXP F=Rf_allocVector(REALSXP,p);
  double *FF=REAL(F);
  if(sz%2==0)
    for(i=0;i<p;++i,++FF){
      rowi=x.row(i);
      nth_element(rowi.begin(),rowi.begin()+middle,rowi.end());
      *FF=(rowi[middle]+*(min_element(rowi.begin()+middle+1,rowi.end())))/2.0;
    }
  else
    for(i=0;i<p;++i,++FF){
      rowi=x.row(i);
      nth_element(rowi.begin(),rowi.begin()+middle,rowi.end());
      *FF=rowi[middle+1]/2.0;
    }
  return F;
}

// rowMedians
RcppExport SEXP Rfast_row_meds(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = row_meds(x);
    return __result;
END_RCPP
}
