
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;


SEXP col_meds_helper_1(SEXP X){
  NumericMatrix x(X);
  const int p=x.ncol(),step=x.nrow(),middle=step/2-1;
  NumericVector tmp(step);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,p));
  double *FF=REAL(F);
  if(step%2==0){
    for(int i=0;i<p;++i,++FF){
      tmp=x.column(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      *FF=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0;
    }
  }else{
    for(int i=0;i<p;++i,++FF){
      tmp=x.column(i);
      nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
      *FF=tmp[middle+1];
    }
  }
  UNPROTECT(1);
  return F;
}

SEXP col_meds_helper_2(SEXP X){
  NumericMatrix x(X);
  const int p=x.ncol(),step=x.nrow(),middle=step/2-1;
  NumericVector tmp(step);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,p));
  double *FF=REAL(F);
  if(step%2==0){
    for(int i=0;i<p;++i,++FF){
      tmp=x.column(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      *FF=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0 * 1.482602;
    }
  }else{
    for(int i=0;i<p;++i,++FF){
      tmp=x.column(i);
      nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
      *FF=tmp[middle+1] * 1.482602;
    }
  }
  UNPROTECT(1);
  return F;
}

SEXP row_meds_helper_1(SEXP X){
  NumericMatrix x(X);
  const int p=x.nrow(),step=x.ncol(),middle=step/2-1;
  NumericVector tmp(step);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,p));
  double *FF=REAL(F);
  if(step%2==0){
    for(int i=0;i<p;++i,++FF){
      tmp=x.row(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      *FF=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0;
    }
  }else{
    for(int i=0;i<p;++i,++FF){
      tmp=x.row(i);
      nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
      *FF=tmp[middle+1];
    }
  }
  UNPROTECT(1);
  return F;
}

SEXP row_meds_helper_2(SEXP X){
  NumericMatrix x(X);
  const int p=x.nrow(),step=x.ncol(),middle=step/2-1;
  NumericVector tmp(step);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,p));
  double *FF=REAL(F);
  if(step%2==0){
    for(int i=0;i<p;++i,++FF){
      tmp=x.row(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      *FF=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0 * 1.482602;
    }
  }else{
    for(int i=0;i<p;++i,++FF){
      tmp=x.row(i);
      nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
      *FF=tmp[middle+1] * 1.482602;
    }
  }
  UNPROTECT(1);
  return F;
}

//[[Rcpp::export]]
SEXP col_mads(SEXP x){
  SEXP y=PROTECT(col_meds_helper_1(x));
  y=eachrow_min_abs(x,y);
  y=col_meds_helper_2(y);
  UNPROTECT(1);
  return y;
}


//[[Rcpp::export]]
SEXP row_mads(SEXP x){
  SEXP y=PROTECT(row_meds_helper_1(x));
  y=eachcol_min_abs(x,y);
  y=row_meds_helper_2(y);
  UNPROTECT(1);
  return y;
}

RcppExport SEXP Rfast_row_mads(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = wrap(row_mads(x));
    return __result;
END_RCPP
}

RcppExport SEXP Rfast_col_mads(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = col_mads(x);
    return __result;
END_RCPP
}
