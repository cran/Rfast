//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include <cmath>
#include "reg_lib.h"
#include "mn.h"

using namespace arma;
using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
List rint_mle(NumericVector X, NumericVector id, const bool ranef, const double tol, const int maxiters){
  int n = X.size(),idmx = max(id);
  vec x(X.begin(),n,false);
  double sxy = sum(x);
  vec sx = group_sum2(x, id,idmx);
  NumericVector tb = Tabulate(id,idmx);

  List res;

  vec ni(tb.begin(), tb.size(),false);

  if (var(ni) == 0) {
    List tmp;

    tmp = varcomps_mle(X,as<IntegerVector>(wrap(id)),idmx,tol);
    NumericVector mat = tmp["mat"];
    NumericVector info(3);
    info(0) = mat(0);
    info(1) = mat(1);
    info(2) = mat(2);
    double tmpd = mat(3);
    res["info"] = info;
    NumericVector syina = tmp["syina"];

    if(ranef)
      res["ranef"] = mat(0)/(mat(0) + mat(1)/tmpd) * syina/tmpd;
  }
  else {
    vec mx = sx/ni;
    vec com = ni % sx;
    double b1 = sxy/n;
    vec xminb1 = x - b1;
    double S = sum(xminb1%xminb1);
    vec mxminb1 = mx-b1;
    vec hi2 = mxminb1%mxminb1;

    vec d(2);
    vec ni2 = ni%ni;
    d = gold_rat3(n, ni, ni2, S, hi2,idmx, tol);
    vec oneplnid = 1 + ni * d[0];
    double down = n - d[0] * sum(ni2/(oneplnid));
    double b2 = (sxy - d[0] * sum(com/(oneplnid)))/down;
    int i = 2;
    while (i++ < maxiters && abs(b2 - b1) > tol) {
      b1 = b2;
      xminb1 = x - b1;
      S = sum(xminb1%xminb1);
      mxminb1 = mx-b1;
      hi2 = mxminb1%mxminb1;
      d = gold_rat3(n, ni, ni2, S, hi2,idmx, tol);
      oneplnid = 1 + ni * d[0];
      down = n - d[0] * sum(ni2/(oneplnid));
      b2 = (sxy - d[0] * sum(com/(oneplnid)))/down;
    }
    NumericVector info(3);

    double sigma = S/n;
    info[1] = sigma/(1 + d[0]);
    info[0] = sigma - info[1];
    info[2] = -0.5 * (d[1] + n * (1.83787706640935-log(n) + 1));
    res["info"] = info;
    res["my"] = b2;
    if (ranef) {
      res["ranef"] = conv_to<rowvec>::from((mx - b2) % (d[0] * ni/(oneplnid)));
    }
  }
  return res;
}

RcppExport SEXP Rfast_rint_mle(SEXP XSEXP,SEXP idSEXP,SEXP ranefSEXP,SEXP tolSEXP,SEXP maxitersSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type X(XSEXP);
    traits::input_parameter< NumericVector >::type id(idSEXP);
    traits::input_parameter< const bool >::type ranef(ranefSEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const int >::type maxiters(maxitersSEXP);
    __result = wrap(rint_mle(X,id,ranef,tol,maxiters));
    return __result;
END_RCPP
}

