
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

SEXP col_meds_helper_1_p(SEXP X){
  NumericMatrix x(X);
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

SEXP col_meds_helper_2_p(SEXP X){
  NumericMatrix x(X);
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
      FF[i]=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0 * 1.482602;
    }
  }else{
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i=0;i<p;++i){
      colvec tmp=xx.col(i);
      nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
      FF[i]=tmp[middle+1] * 1.482602;
    }
  }
  UNPROTECT(1);
  return F;
}

SEXP row_meds_helper_1_p(SEXP X){
  NumericMatrix x(X);
  const int p=x.nrow(),step=x.ncol(),middle=step/2-1;
  mat xx(x.begin(),step,p,false);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,p));
  double *FF=REAL(F);
  if(step%2==0){
    #ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i=0;i<p;++i){
      rowvec tmp=xx.row(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      FF[i]=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0;
    }
  }else{
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i=0;i<p;++i){
      rowvec tmp=xx.row(i);
      nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
      FF[i]=tmp[middle+1];
    }
  }
  UNPROTECT(1);
  return F;
}

SEXP row_meds_helper_2_p(SEXP X){
  NumericMatrix x(X);
  const int p=x.nrow(),step=x.ncol(),middle=step/2-1;
  mat xx(x.begin(),step,p,false);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,p));
  double *FF=REAL(F);
  if(step%2==0){
    #ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i=0;i<p;++i){
      rowvec tmp=xx.row(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      FF[i]=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0 * 1.482602;
    }
  }else{
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i=0;i<p;++i){
      rowvec tmp=xx.row(i);
      nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
      FF[i]=tmp[middle+1] * 1.482602;
    }
  }
  UNPROTECT(1);
  return F;
}

//[[Rcpp::export]]
SEXP col_mads_p(SEXP x){
  SEXP y=PROTECT(col_meds_helper_1_p(x));
  y=eachrow_min_abs(x,y);
  y=col_meds_helper_2_p(y);
  UNPROTECT(1);
  return y;
}

//[[Rcpp::export]]
SEXP row_mads_p(SEXP x){
  SEXP y=PROTECT(row_meds_helper_1_p(x));
  y=eachcol_min_abs(x,y);
  y=row_meds_helper_2_p(y);
  UNPROTECT(1);
  return y;
}

RcppExport SEXP Rfast_row_mads_p(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = row_mads_p(x);
    return __result;
END_RCPP
}

RcppExport SEXP Rfast_col_mads_p(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = col_mads_p(x);
    return __result;
END_RCPP
}
