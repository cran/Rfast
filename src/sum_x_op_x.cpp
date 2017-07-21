// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rinternals.h>
#include <R.h>

using namespace Rcpp;

static double sum_x_mul_x(SEXP x){
  const int n=LENGTH(x);
  double s=0,*startx=REAL(x),*endx=startx+n;
  for(;startx!=endx;++startx)
    s+=*startx * *startx;
  return s;
}

static double sum_x_plus_x(SEXP x){
  const int n=LENGTH(x);
  double s=0,*startx=REAL(x),*endx=startx+n;
  for(;startx!=endx;++startx)
    s+=*startx + *startx;
  return s;
}

static double sum_x_div_x(SEXP x){
  const int n=LENGTH(x);
  double s=0,*startx=REAL(x),*endx=startx+n;
  for(;startx!=endx;++startx)
    s+=*startx / *startx;
  return s;
}

static double sum_x_min_x(SEXP x){
  const int n=LENGTH(x);
  double s=0,*startx=REAL(x),*endx=startx+n;
  for(;startx!=endx;++startx)
    s+=*startx - *startx;
  return s;
}

//[[Rcpp::export]]
double sum_XopX(SEXP x,const char oper){
  switch(oper){
    case '+': return sum_x_plus_x(x);
    case '-': return sum_x_min_x(x);
    case '*': return sum_x_mul_x(x);
    case '/': return sum_x_div_x(x);
    default: stop("The operation doesn't supported.");
  }
  return 0.0;
}

RcppExport SEXP Rfast_sum_XopX(SEXP x,SEXP operSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const char  >::type oper(operSEXP);
    __result = wrap(sum_XopX(x,oper));
    return __result;
END_RCPP
}
