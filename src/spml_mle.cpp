//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include <cmath>
#include "mn.h"
#include "reg_lib.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
List spml_mle(NumericVector X, const double tol, const int maxiters){
  int n = X.size();
  vec x(X.begin(),n,false);
  List l;
  mat u(n,2);
  u.col(0) = cos(x);
  u.col(1) = sin(x);

  vec ci2 = u.col(0)%u.col(0),cisi = u.col(0)%u.col(1), si2 = u.col(1)%u.col(1);

  vec su(2);
  su(0) = sum(u.col(0)), su(1) = sum(u.col(1));

  double nR = sqrt(sum(su%su)), kappa = vmf_mle2(nR, n, tol, maxiters);

  vec mu = su*(1/nR);

  vec mu1 = mu*kappa;
  double f = -0.5, con = 2.506628274631;
  vec tau = u*mu1, ptau = pnormc(tau);

  vec rat = ptau/(exp(f * tau%tau)/con + tau % ptau);

  vec psit = tau + rat;
  vec psit2 = 2 - rat%(tau + rat);

  vec der(2);
  der[0] = sum(u.col(0)%psit) - n * mu1[0];
  der[1] = sum(u.col(1)%psit) - n * mu1[1];

  double dera = der[0],derb = der[1],dera2 = sum(psit2%ci2)-n,derab = sum(psit2%cisi),derb2 = sum(psit2%si2)-n;

  double down = dera2 * derb2 - derab*derab;
  vec mu2(2);
  mu2[0] = mu1[0] - (derb2 * dera - derab * derb)/down;
  mu2[1] = mu1[1] - (-derab * dera + dera2 * derb)/down;

  int i = 2;
  while (i++<maxiters && sum(abs(mu2 - mu1)) > tol) {
    mu1 = mu2;
    tau = u*mu1;
    ptau = pnormc(tau);
    rat = ptau/(exp(f * (tau%tau))/con + tau % ptau);
    psit = tau + rat;
    psit2 = 2 - rat%(tau + rat);

    der[0] = sum(u.col(0)%psit) - n * mu1[0];
    der[1] = sum(u.col(1)%psit) - n * mu1[1];
    dera = der[0],derb = der[1],dera2 = sum(psit2%ci2)-n,derab = sum(psit2%cisi),derb2 = sum(psit2%si2)-n;
    down = dera2 * derb2 - derab*derab;
    mu2[0] = mu1[0] - (derb2 * dera - derab * derb)/down;
    mu2[1] = mu1[1] - (-derab * dera + dera2 * derb)/down;

  }

  l["iters"] = i-1;

  double gam = sum(mu2[0]*mu2[0]+mu2[1]*mu2[1]);
  l["loglik"] = -0.5 * n * gam + log1pColvecSum(((tau % ptau) * con)/exp(f*tau%tau),n) - n * 1.83787706640935;
  l["gamma"] = gam;
  l["mu"] = conv_to<rowvec>::from(mu2);

  return l;
}

RcppExport SEXP Rfast_spml_mle(SEXP XSEXP,SEXP tolSEXP,SEXP maxitersSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type X(XSEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const int >::type maxiters(maxitersSEXP);
    __result = wrap(spml_mle(X,tol,maxiters));
    return __result;
END_RCPP
}
