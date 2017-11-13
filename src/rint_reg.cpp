//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include "mn.h"
#include <cmath>
#include "reg_lib.h"

using namespace arma;
using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
List rint_reg(NumericMatrix X, NumericVector Y, NumericVector id, const double tol, const bool ranef, const int maxiters){
  int n = X.nrow(), p = X.ncol(), idmx = max(id);
  mat x(X.begin(), n,p,false),xx(p,p),sx(idmx,p),sxy(p,1),mx(idmx,p);
  vec y(Y.begin(),n,false),my(idmx);

  double logpitimes2 = 1.83787706640935,logn = log(n);
  NumericVector tb = Tabulate(id,idmx);

  vec ni(tb.begin(), tb.size(),false);

  xx = cross_x_2(x);
  for(int i=0;i<p;i++)
    sx.col(i) = group_sum2(x.col(i), id,idmx);
  sxy = cross_x_y_2(x,y);
  colvec sy = group_sum2(y, id,idmx);
  mx = sx.each_col()/ni;
  my = sy/ni;

  mat b1 = solve(xx,sxy,solve_opts::fast);
  vec tmp = y - x*b1;
  double S = sum(tmp%tmp);
  vec tmp2 = my-mx*b1;
  vec hi2 = tmp2%tmp2;
  vec ni2 = ni%ni;

  vec d(2);
  d = gold_rat3(n, ni, ni2, S, hi2,idmx, tol);
  vec oneplnid = 1+ni*d(0);
  vec b2 = solve(xx - d(0)* cross_x_y_2(sx.each_col()/oneplnid, sx), sxy -
    d(0) * cross_x_y_2(sx, sy/oneplnid),solve_opts::fast);
  int i = 2;

  while(i++<maxiters && sum(abs(b2-b1.col(0))) > tol) {
    b1.col(0) = b2;

    tmp = y - x*b1;
    S = accu(tmp%tmp);
    tmp2 = my-mx*b1;
    hi2 = tmp2%tmp2;

    d = gold_rat3(n, ni, ni2, S, hi2,idmx, tol);
    oneplnid = 1+ni*d(0);
    b2 = solve(xx - d(0) * cross_x_y_2(sx.each_col()/oneplnid, sx), sxy -
      d(0) * cross_x_y_2(sx, sy/oneplnid),solve_opts::fast);
  }

  mat eye;
  List l;
  NumericVector info(6);
  info(0) = i-1;
  info(2) = (S-d(0)*sum(ni2%hi2/oneplnid))/n;
  info(1) = d(0)*info(2);
  info(3) = -0.5 * d(1)-0.5*n*(logpitimes2-logn+1);
  info(4) = -2 * info(3);
  info(5) = info(4) + (p + 2) * logn;
  l["info"] = info;
  l["be"] = as<NumericMatrix>(wrap(b2));
  l["seb"] = as<NumericVector>(wrap(sqrt(((mat)solve(xx-d(0)*cross_x_y_2(sx.each_col()/oneplnid,sx),eye.eye(p,p),solve_opts::fast)).diag()*info(2))));

  if(ranef){
    mat er = y - x * (conv_to<colvec>::from(b2));
    l["ranef"] =  as<NumericVector>(wrap(d[0] * ni/(oneplnid) % group_sum2(er.col(0), id,idmx)/ni));
  }
  return l;
}

RcppExport SEXP Rfast_rint_reg(SEXP XSEXP,SEXP YSEXP,SEXP idSEXP,SEXP tolSEXP,SEXP ranefSEXP,SEXP maxitersSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type X(XSEXP);
    traits::input_parameter< NumericVector >::type Y(YSEXP);
    traits::input_parameter< NumericVector >::type id(idSEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const bool >::type ranef(ranefSEXP);
    traits::input_parameter< const int >::type maxiters(maxitersSEXP);
    __result = wrap(rint_reg(X,Y,id,tol,ranef,maxiters));
    return __result;
END_RCPP
}
