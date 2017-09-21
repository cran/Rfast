
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>

using namespace Rcpp;
using namespace std;


static double sum_eachrow_mul(SEXP x,SEXP y){
  int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP mat=Rf_duplicate(x);
  double *xx=REAL(mat),*end=xx+ncol*nrow,*yy=REAL(y),y3,*x3,s=0;
  for(;xx!=end;++yy){
    y3=*yy;
    for(x3=xx,xx+=nrow;x3!=xx;++x3){
      s+=*x3*y3;
    }
  }
  return s;
}

static double sum_eachrow_plus(SEXP x,SEXP y){
  int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP mat=Rf_duplicate(x);
  double *xx=REAL(mat),*end=xx+ncol*nrow,*yy=REAL(y),y3,*x3,s=0;
  for(;xx!=end;++yy){
    y3=*yy;
    for(x3=xx,xx+=nrow;x3!=xx;++x3){
      s+=*x3+y3;
    }
  }
  return s;
}

static double sum_eachrow_div(SEXP x,SEXP y){
  int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP mat=Rf_duplicate(x);
  double *xx=REAL(mat),*end=xx+ncol*nrow,*yy=REAL(y),y3,*x3,s=0;
  for(;xx!=end;++yy){
    y3=*yy;
    for(x3=xx,xx+=nrow;x3!=xx;++x3){
      s+=*x3/y3;
    }
  }
  return s;
}

static double sum_eachrow_min(SEXP x,SEXP y){
  int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP mat=Rf_duplicate(x);
  double *xx=REAL(mat),*end=xx+ncol*nrow,*yy=REAL(y),y3,*x3,s=0;
  for(;xx!=end;++yy){
    y3=*yy;
    for(x3=xx,xx+=nrow;x3!=xx;++x3){
      s+=*x3-y3;
    }
  }
  return s;
}

//[[Rcpp::export]]
double sum_eachrow(SEXP x,SEXP y,const char oper){
  switch(oper){
    case '*': return sum_eachrow_mul(x,y);
    case '+': return sum_eachrow_plus(x,y);
    case '/': return sum_eachrow_div(x,y);
    case '-': return sum_eachrow_min(x,y);
    default: stop("The operation doesn't supported.");
  }
  return 0.0;
}

RcppExport SEXP Rfast_sum_eachrow(SEXP x,SEXP y,SEXP operSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const char  >::type oper(operSEXP);
    __result = sum_eachrow(x,y,oper);
    return __result;
END_RCPP
}
