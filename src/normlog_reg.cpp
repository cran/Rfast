//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include "math.h"
#include "reg_lib.h"
#include <iostream>

using namespace Rcpp;
using namespace arma;
using namespace std;

List normlog_reg(NumericVector Y,NumericMatrix X, const double tol, const int maxiters){
  List l;
  int n = Y.size();
  double con =  - n * 0.5 * 1.83787706640935 - 0.5 * n;
  int pcols = X.ncol();
  mat x(X.begin(),n,pcols,false),xt = x.t();
  colvec y(Y.begin(),n,false);
  colvec b1 = solve(xt * x, xt * log(y + 0.1),solve_opts::fast);
  colvec yhat = (exp(x * b1));

  mat com = x.each_col()%(yhat % yhat);

  mat com2 = x.each_col()%(yhat % y);
  colvec der = conv_to<colvec>::from(sum(com-com2));

  mat der2 = 2 * com.t() * x - com2.t() * x;
  colvec b2 = b1 - solve(der2, der,solve_opts::fast);
  int i = 2;

  while(i++<maxiters && sum(abs(b1-b2)) > tol){
    b1 = b2;
    yhat = (exp(x * b1));
    com = x.each_col()%(yhat % yhat);
    com2 = x.each_col()%(yhat % y);
    der = conv_to<colvec>::from(sum(com-com2));
    der2 = 2 * com.t() * x - com2.t() * x;
    b2 = b1 - solve(der2, der,solve_opts::fast);
  }

  colvec ymyhat = y - yhat;
  double deviance = sum((ymyhat % ymyhat));
  double loglik = con - n * 0.5 * log(deviance/n);
  l["iters"] = i;
  l["loglik"] = loglik;
  l["deviance"] = deviance;
  l["be"] = b2;
  return l;
}

RcppExport SEXP Rfast_normlog_reg(SEXP YSEXP,SEXP XSEXP,SEXP tolSEXP,SEXP maxitersSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type Y(YSEXP);
    traits::input_parameter< NumericMatrix >::type X(XSEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const int >::type maxiters(maxitersSEXP);
    __result = wrap(normlog_reg(Y,X,tol,maxiters));
    return __result;
END_RCPP
}
