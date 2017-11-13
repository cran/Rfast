//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>
#include <algorithm>
#include "templates.h"

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
SEXP col_meds_simple_p(NumericMatrix x){
  const int p=x.ncol(),step=x.nrow(),middle=step/2-1;
  mat xx(x.begin(),step,p,false);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,p));
  double *FF=REAL(F);
  if(step%2==0){
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<p;++i){
      colvec tmp=xx.col(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      FF[i]=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0;
    }
  }else{
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<p;++i){
      colvec tmp=xx.col(i);
      nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
      FF[i]=tmp[middle+1];
    }
  }
  UNPROTECT(1);
  return F;
}

//[[Rcpp::export]]
SEXP col_meds_na_rm_p(NumericMatrix& x){
  const int p=x.ncol();
  int i;
  mat xx(x.begin(),x.nrow(),p,false);
  SEXP F=Rf_allocVector(REALSXP,p);
  double *FF=REAL(F);
  #ifdef _OPENMP
  #pragma omp parallel for
  #endif
  for(i=0;i<p;++i){
    colvec tmp=xx.col(i);
    FF[i]=med_helper<colvec>(tmp.begin(),tmp.begin()+(int)(std::remove_if(tmp.begin(),tmp.end(),R_IsNA)-tmp.begin()));
  }
  return F;
}

SEXP col_meds_p(NumericMatrix x,const bool na_rm){
  return na_rm ? col_meds_na_rm_p(x) : col_meds_simple_p(x);
}

// colMedians
RcppExport SEXP Rfast_col_meds_p(SEXP xSEXP,SEXP na_rmSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    __result = col_meds_p(x,na_rm);
    return __result;
END_RCPP
}

//[[Rcpp::export]]
SEXP row_meds_p(NumericMatrix x){
  const int sz=x.ncol(),p=x.nrow(),middle=sz/2-1;
  mat xx(x.begin(),sz,p,false);
  SEXP F=Rf_allocVector(REALSXP,p);
  double *FF=REAL(F);
  if(sz%2==0){
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<p;++i){
      rowvec rowi=xx.row(i);
      nth_element(rowi.begin(),rowi.begin()+middle,rowi.end());
      FF[i]=(rowi[middle]+*(min_element(rowi.begin()+middle+1,rowi.end())))/2.0;
    }
  }else{
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<p;++i){
      rowvec rowi=xx.row(i);
      nth_element(rowi.begin(),rowi.begin()+middle,rowi.end());
      FF[i]=rowi[middle+1]/2.0;
    }
  }
  return F;
}

// rowMedians
RcppExport SEXP Rfast_row_meds_p(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = row_meds_p(x);
    return __result;
END_RCPP
}
