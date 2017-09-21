//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"
#include <vector>
#include <R.h>
#include <Rinternals.h>

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
vector<double> col_min_indices(NumericMatrix x){
  unsigned int i=0,p=x.ncol();
  mat X = mat(x.begin(), x.nrow(), p, false);
  vector<double> F(p);
  vector<double>::iterator FF=F.begin();
    for(;i<p;++i,++FF)
      *FF=(X.col(i)).index_min()+1;
  return F;
}

//[[Rcpp::export]]
SEXP col_min(SEXP x){
  int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP F;
  switch(TYPEOF(x)){
    case REALSXP:{
      F=PROTECT(Rf_allocVector(REALSXP,ncol));
      double *xx=REAL(x),*end=xx+ncol*nrow,*f=REAL(F);
    for(;xx!=end;xx+=nrow,++f)
        min_d(xx,xx+nrow,*f);
      break;
    }
    default:{
      F=PROTECT(Rf_allocVector(INTSXP,ncol));
    int *xx=INTEGER(x),*end=xx+ncol*nrow,*f=INTEGER(F);
    for(;xx!=end;xx+=nrow,++f)
        min_i(xx,xx+nrow,*f);
      break;
    }
  }
  UNPROTECT(1);
  return F;
}

// find the minimum index of its collumn
RcppExport SEXP Rfast_col_min_indices(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(col_min_indices(x));
    return __result;
END_RCPP
}

// find the minimum value of its collumn
RcppExport SEXP Rfast_col_min(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = col_min(x);
    return __result;
END_RCPP
}



// [[Rcpp::export]]
NumericVector row_min_indices(NumericMatrix x){
  const unsigned int p=x.nrow();
  mat X = mat(x.begin(), p, x.ncol(), false); 
  NumericVector F(p);
  NumericVector::iterator FF=F.begin();
  for(unsigned int i=0;i<p;++i,++FF)
      *FF=(X.row(i)).index_min()+1;
  return F;
}

//[[Rcpp::export]]
SEXP row_min(SEXP x){
  int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP F;
  F=PROTECT(Rf_allocVector(REALSXP,nrow));
  double *xx=REAL(x),*end=xx+ncol*nrow,*f=REAL(F),*x3,*ff;
  const double *endf=f+LENGTH(F);
  for(ff=f;ff!=endf;++ff,++xx)
    *ff=*xx;
  for(;xx!=end;)
    for(ff=f,x3=xx,xx+=nrow;x3!=xx;++ff,++x3){
      *ff=std::min(*ff,*x3);
    }
    UNPROTECT(1);
  return F;
}

RcppExport SEXP Rfast_row_min_indices(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(row_min_indices(x));
    return __result;
END_RCPP
}

RcppExport SEXP Rfast_row_min(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = row_min(x);
    return __result;
END_RCPP
}
