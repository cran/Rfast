//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>
#include "mn.h"

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

//[[Rcpp::export]]
SEXP cholesky_par(SEXP AA) {
  int i,j,k,ni,nj,n=Rf_ncols(AA);
  SEXP LL=PROTECT(Rf_allocMatrix(REALSXP,n,n));
  double s,*A=REAL(AA),*L=REAL(LL);
  for(j=0;j<n*n;++j)
    L[j]=0;
  for (j = 0; j <n; ++j) {            
    s = 0;
    nj=n*j;
    for (k = 0; k < j; ++k) {
      s += L[nj + k] * L[nj + k];
    }
    L[nj + j] = sqrt(A[nj + j] - s);
    #ifdef _OPENMP
    #pragma omp parallel for
  	#endif
    for (i = j+1; i <n; ++i) {
      s=0;
      ni=i*n;
      for (k = 0; k < j; ++k) {
        s += L[ni + k] * L[nj + k];
      }
      L[ni + j] = (1.0 / L[nj + j] * (A[ni + j] - s));
    }
  }
  UNPROTECT(1);
  return LL;
}

//[[Rcpp::export]]
SEXP cholesky(SEXP AA) {
  int i,j,k,ni,nj,n=Rf_ncols(AA);
  SEXP LL=PROTECT(Rf_allocMatrix(REALSXP,n,n));
  double s,*A=REAL(AA),*L=REAL(LL);
  for(j=0;j<n*n;++j)
    L[j]=0;
  for (j = 0; j <n; ++j) {            
    s = 0;
    nj=n*j;
    for (k = 0; k < j; ++k) {
      s += L[nj + k] * L[nj + k];
    }
    L[nj + j] = sqrt(A[nj + j] - s);
    for (i = j+1; i <n; ++i) {
      s=0;
      ni=i*n;
      for (k = 0; k < j; ++k) {
        s += L[ni + k] * L[nj + k];
      }
      L[ni + j] = (1.0 / L[nj + j] * (A[ni + j] - s));
    }
  }
  UNPROTECT(1);
  return LL;
}



RcppExport SEXP Rfast_cholesky(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = cholesky(x);
    return __result;
END_RCPP
}

RcppExport SEXP Rfast_cholesky_par(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = cholesky_par(x);
    return __result;
END_RCPP
}
