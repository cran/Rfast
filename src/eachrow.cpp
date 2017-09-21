
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>

using namespace Rcpp;
using namespace std;


SEXP eachrow_mul(SEXP x,SEXP y){
  int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP mat=PROTECT(Rf_duplicate(x));
  double *xx=REAL(mat),*end=xx+ncol*nrow,*yy=REAL(y),y3,*x3;
  for(;xx!=end;++yy){
    y3=*yy;
    for(x3=xx,xx+=nrow;x3!=xx;++x3){
      *x3*=y3;
    }
  }
  UNPROTECT(1);
  return mat;
}

SEXP eachrow_plus(SEXP x,SEXP y){
  int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP mat=PROTECT(Rf_duplicate(x));
  double *xx=REAL(mat),*end=xx+ncol*nrow,*yy=REAL(y),y3,*x3;
  for(;xx!=end;++yy){
    y3=*yy;
    for(x3=xx,xx+=nrow;x3!=xx;++x3){
      *x3+=y3;
    }
  }
  UNPROTECT(1);
  return mat;
}

SEXP eachrow_div(SEXP x,SEXP y){
  int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP mat=PROTECT(Rf_duplicate(x));
  double *xx=REAL(mat),*end=xx+ncol*nrow,*yy=REAL(y),y3,*x3;
  for(;xx!=end;++yy){
    y3=*yy;
    for(x3=xx,xx+=nrow;x3!=xx;++x3){
      *x3/=y3;
    }
  }
  UNPROTECT(1);
  return mat;
}

SEXP eachrow_min(SEXP x,SEXP y){
  int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP mat=PROTECT(Rf_duplicate(x));
  double *xx=REAL(mat),*end=xx+ncol*nrow,*yy=REAL(y),y3,*x3;
  for(;xx!=end;++yy){
    y3=*yy;
    for(x3=xx,xx+=nrow;x3!=xx;++x3){
      *x3-=y3;
    }
  }
  UNPROTECT(1);
  return mat;
}

//[[Rcpp::export]]
SEXP eachrow(SEXP x,SEXP y,const char oper){
  switch(oper){
    case '*': return eachrow_mul(x,y);
    case '+': return eachrow_plus(x,y);
    case '/': return eachrow_div(x,y);
    case '-': return eachrow_min(x,y);
    default: stop("The operation doesn't supported.");
  }
  return R_NilValue;
}

RcppExport SEXP Rfast_eachrow(SEXP x,SEXP y,SEXP operSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const char  >::type oper(operSEXP);
    __result = eachrow(x,y,oper);
    return __result;
END_RCPP
}
