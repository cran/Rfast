//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include <cmath>
#include "reg_lib.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
NumericMatrix geom_regs(NumericVector Y,NumericMatrix X, const double tol, const bool logged, const bool type, const bool parallel, const int maxiters){
  int n = X.nrow(), D = X.ncol();
  mat x(X.begin(),n,D,false);
  vec y(Y.begin(),n,false);
  double a0 = mean(y);
  double p0, ini;
  vec y1;
  if(type == 1){
    y1 = y + 1;
    p0 = 1/(1+a0);
    ini = 2 * n * log(p0) + 2 * a0 * n * log(1 - p0);
  }else{
    y1 = y;
    p0 = 1/(a0);
    ini = 2 * n * log(p0) + 2 * (n * a0 - n) * log(1 - p0);
  }
  vec yp0 = y1*p0;

  double dera0 = n - sum(yp0);
  double dera20 = - sum(yp0 * (1 - p0) );

  vec lik(D);
  if(parallel){
      #ifdef _OPENMP
      #pragma omp parallel
        {
      #endif
        vec yp, ypxp, tmpX, aold(2), tmpCB(2), anew(2), oneminusp, p, yptmpX;
        double tmpsumX, dera, dera2, derb, derb2, derab, divider;
        int ij;

        #ifdef _OPENMP
            #pragma omp for
        #endif
        for(int i = 0; i < D; i++){
          tmpX = x.col(i);
          tmpsumX = sum(tmpX);
          aold[0] = -log(a0);
          aold[1] = 0;
          yp = yp0;
          dera = dera0;
          dera2 = dera20;
          yptmpX = yp%tmpX;
          derb = tmpsumX - sum(yptmpX);
          derb2 = -sum((yptmpX % tmpX) * (1 - p0));

          derab =  -sum(yptmpX * (1 - p0));
          tmpCB[0] = derb2 * dera - derab * derb;
          tmpCB[1] = -derab * dera + dera2 * derb;

          divider = (dera2 * derb2 - derab * derab);
          anew[0] = aold[0] - tmpCB[0]/divider;
          anew[1] = aold[1] - tmpCB[1]/divider;
          ij=2;

          while(ij++<maxiters && sum( abs(anew - aold) ) > tol ) {
            aold = anew;
            p = 1/( 1 + (exp(- aold[0] - aold[1] * tmpX)));
            oneminusp = 1-p;
            yp = y1 % p;
            dera = n - sum(yp);
            dera2 =  - sum(yp % oneminusp);
            yptmpX = yp%tmpX;
            derb = tmpsumX - sum(yptmpX);
            ypxp = (yptmpX) % oneminusp;
            derb2 =  -sum(ypxp % tmpX);
            derab = -sum(ypxp);
            tmpCB[0] = derb2 * dera - derab * derb;
            tmpCB[1] = -derab * dera + dera2 * derb;
            divider = (dera2 * derb2 - derab * derab);
            anew[0] = aold[0] - tmpCB[0]/divider;
            anew[1] = aold[1] - tmpCB[1]/divider;
          }
          lik[i] = n * anew[0] + anew[1] * tmpsumX - sum(y1 % log1pColvec(exp( anew[0] + anew[1] * tmpX),n));
        }
      #ifdef _OPENMP
        }
      #endif
  }
  else{
    vec yp, ypxp, tmpX, aold(2), tmpCB(2), anew(2), oneminusp, p, yptmpX;
    double tmpsumX, dera, dera2, derb, derb2, derab, divider;
    int ij;

    for(int i = 0; i < D; i++){
      tmpX = x.col(i);
      tmpsumX = sum(tmpX);
      aold[0] = -log(a0);
      aold[1] = 0;
      yp = yp0;
      dera = dera0;
      dera2 = dera20;
      yptmpX = yp%tmpX;
      derb = tmpsumX - sum(yptmpX);
      derb2 = -sum((yptmpX % tmpX) * (1 - p0));

      derab =  -sum(yptmpX * (1 - p0));
      tmpCB[0] = derb2 * dera - derab * derb;
      tmpCB[1] = -derab * dera + dera2 * derb;

      divider = (dera2 * derb2 - derab * derab);
      anew[0] = aold[0] - tmpCB[0]/divider;
      anew[1] = aold[1] - tmpCB[1]/divider;
      ij=2;

      while(ij++<maxiters && sum( abs(anew - aold) ) > tol ) {
        aold = anew;
        p = 1/( 1 + (exp(- aold[0] - aold[1] * tmpX)));
        oneminusp = 1-p;
        yp = y1 % p;
        dera = n - sum(yp);
        dera2 =  - sum(yp % oneminusp);
        yptmpX = yp%tmpX;
        derb = tmpsumX - sum(yptmpX);
        ypxp = (yptmpX) % oneminusp;
        derb2 =  -sum(ypxp % tmpX);
        derab = -sum(ypxp);
        tmpCB[0] = derb2 * dera - derab * derb;
        tmpCB[1] = -derab * dera + dera2 * derb;
        divider = (dera2 * derb2 - derab * derab);
        anew[0] = aold[0] - tmpCB[0]/divider;
        anew[1] = aold[1] - tmpCB[1]/divider;
      }
      lik[i] = n * anew[0] + anew[1] * tmpsumX - sum(y1 % log1pColvec(exp( anew[0] + anew[1] * tmpX),n));
    }
  }

  NumericMatrix ret(D,2);

  if(parallel){
      #ifdef _OPENMP
      #pragma omp parallel for
      #endif
      for(int i = 0; i < D; i++){
        ret(i,0) = 2 * lik(i) - ini;
        ret(i,1) = R::pchisq(ret(i,0), 1, false, logged);
      }
  }
  else{
      for(int i = 0; i < D; i++){
        ret(i,0) = 2 * lik(i) - ini;
        ret(i,1) = R::pchisq(ret(i,0), 1, false, logged);
      }
  }


  return ret;
}

RcppExport SEXP Rfast_geom_regs(SEXP YSEXP,SEXP XSEXP,SEXP tolSEXP,SEXP loggedSEXP,SEXP typeSEXP,SEXP parallelSEXP,SEXP maxitersSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type Y(YSEXP);
    traits::input_parameter< NumericMatrix >::type X(XSEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const bool >::type logged(loggedSEXP);
    traits::input_parameter< const bool >::type type(typeSEXP);
    traits::input_parameter< const bool >::type parallel(parallelSEXP);
    traits::input_parameter< const int >::type maxiters(maxitersSEXP);
    __result = wrap(geom_regs(Y,X,tol,logged,type,parallel,maxiters));
    return __result;
END_RCPP
}
