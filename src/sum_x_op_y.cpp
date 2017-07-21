// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rinternals.h>
#include <R.h>

using namespace Rcpp;

static double sum_x_mul_y(SEXP x,SEXP y){
  const int n=LENGTH(x);
  double s=0,*startx=REAL(x),*endx=startx+n,*starty=REAL(y);
  for(;startx!=endx;++startx,++starty)
    s+=*startx * *starty;
  return s;
}

static double sum_x_plus_y(SEXP x,SEXP y){
  const int n=LENGTH(x);
  double s=0,*startx=REAL(x),*endx=startx+n,*starty=REAL(y);
  for(;startx!=endx;++startx,++starty)
    s+=*startx + *starty;
  return s;
}

static double sum_x_div_y(SEXP x,SEXP y){
  const int n=LENGTH(x);
  double s=0,*startx=REAL(x),*endx=startx+n,*starty=REAL(y);
  for(;startx!=endx;++startx,++starty)
    s+=*startx / *starty;
  return s;
}

static double sum_x_min_y(SEXP x,SEXP y){
  const int n=LENGTH(x);
  double s=0,*startx=REAL(x),*endx=startx+n,*starty=REAL(y);
  for(;startx!=endx;++startx,++starty)
    s+=*startx - *starty;
  return s;
}

//[[Rcpp::export]]
double sum_XopY(SEXP x,SEXP y,const char oper){
  switch(oper){
    case '+': return sum_x_plus_y(x,y);
    case '-': return sum_x_min_y(x,y);
    case '*': return sum_x_mul_y(x,y);
    case '/': return sum_x_div_y(x,y);
    default: stop("The operation doesn't supported.");
  }
  return 0.0;
}

RcppExport SEXP Rfast_sum_XopY(SEXP x,SEXP y,SEXP operSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const char  >::type oper(operSEXP);
    __result = wrap(sum_XopY(x,y,oper));
    return __result;
END_RCPP
}
