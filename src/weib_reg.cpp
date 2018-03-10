//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include <cmath>
#include "reg_lib.h"
#include "templates.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
List weib_reg(NumericVector Y, NumericMatrix X, const double tol, const int maxiters){
  List l;
  int n = Y.size(), d = X.ncol();
  mat x(X.begin(),n,d,false);
  vec y(Y.begin(),n,false), b1(d,fill::zeros),com(n),ini,lam;

  ini = weibull_mle2(y, n, tol, maxiters);

  double sly = sum(log(y)), ek = ini[0], logek = log(ek), yhat0 = n/sum(y);
  b1[0] = log(ini[1]);
  com = my_pow2(y*yhat0,com,ek,n);
  rowvec sx = sum(x);

  vec logcom = log(com);
  vec comlogcom = com%logcom;

  double derk = n + ek * (sly + n * yhat0) - sum( comlogcom);
  double derk2 = derk - n - sum(comlogcom%logcom);

  mat xcom = x.each_col()%com;
  vec derb =  ek * conv_to<vec>::from(sum(xcom)- sx);

  mat derb2 = (- ek*ek) * cross_x_y<mat,mat,vec>(xcom, x);
  double k2 = logek - derk/derk2;

  vec b2 = b1 - solve(derb2, derb);
  int i = 2;

  vec yhat;

  while (i++<maxiters && (sum(abs(b2 - b1))+abs(logek-k2)) > tol ) {
    logek = k2;
    b1 = b2;
    ek = exp(logek);
    lam = -(x*b1);
    yhat = exp(lam);
    com = my_pow2(y % yhat,com,ek,n);
    logcom = log(com);
    comlogcom = com%logcom;
    derk = n + ek * (sly + sum(lam)) - sum(comlogcom);
    derk2 = derk - n - sum(comlogcom%logcom);

    xcom = x.each_col()%com;
    derb =  ek * conv_to<vec>::from(sum(xcom)- sx);
    derb2 = (- ek*ek) * cross_x_y<mat,mat,vec>(xcom, x);
    k2 = logek - derk/derk2;
    b2 = b1 - solve(derb2, derb);
  }

  l["iters"] = i-1;
  l["loglik"] = n * k2 + (ek - 1) * sly + ek * sum(lam) - sum(com);
  l["shape"] = ek;
  l["be"] = b2;
  l["der2"] = derb2;

  return l;
}

RcppExport SEXP Rfast_weib_reg(SEXP YSEXP,SEXP XSEXP,SEXP tolSEXP,SEXP maxitersSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type Y(YSEXP);
    traits::input_parameter< NumericMatrix >::type X(XSEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const int >::type maxiters(maxitersSEXP);
    __result = wrap(weib_reg(Y,X,tol,maxiters));
    return __result;
END_RCPP
}
