//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include <cmath>
#include "reg_lib.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
NumericMatrix geom_regs(NumericVector Y,NumericMatrix X,const double tol,const int logged,const int type,const int parallel,const int maxiters){
  int n = X.nrow(), D = X.ncol();
  mat x(X.begin(),n,D,false);
  vec y(Y.begin(),Y.size(),false);
  double a0 = mean(y);
  double p0, ini;
  vec p;
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
  vec yp, ypxp;
  double dera0 = n - sum(yp0);
  double dera20 = - sum(yp0 * (1 - p0) );

  vec lik(D);
  #ifdef _OPENMP
      #pragma omp parallel if(parallel)
  {
  #endif
    vec yp, ypxp, tmpX,log1pCV(n), aold(2), tmpCB(2), anew(2), oneminusp, p;
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
      
      derb = tmpsumX - sum(yp % tmpX);
      derb2 = -sum(yp % (tmpX % tmpX) * (1 - p0));
      
      derab =  -sum((yp % tmpX) * (1 - p0));
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
        derb = tmpsumX - sum(yp % tmpX);
        ypxp = (yp % tmpX) % oneminusp;
        derb2 =  -sum(ypxp % tmpX);
        derab = -sum(ypxp);
        tmpCB[0] = derb2 * dera - derab * derb;
        tmpCB[1] = -derab * dera + dera2 * derb;
        divider = (dera2 * derb2 - derab * derab);
        anew[0] = aold[0] - tmpCB[0]/divider;
        anew[1] = aold[1] - tmpCB[1]/divider;
      }
      log1pCV = exp( anew[0] + anew[1] * tmpX);
      lik[i] = n * anew[0] + anew[1] * tmpsumX - sum(y1 % log1pColvec(log1pCV, 0));
    }
#ifdef _OPENMP
  }
#endif
  NumericMatrix ret(D,2);
  #ifdef _OPENMP
      #pragma omp parallel for if(parallel)
  #endif
  for(int i = 0; i < D; i++){
    ret(i,0) = 2 * lik(i) - ini;
    ret(i,1) = R::pchisq(ret(i,0), 1, false, logged);
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
    traits::input_parameter< const int >::type logged(loggedSEXP);
    traits::input_parameter< const int >::type type(typeSEXP);
    traits::input_parameter< const int >::type parallel(parallelSEXP);
    traits::input_parameter< const int >::type maxiters(maxitersSEXP);
    __result = wrap(geom_regs(Y,X,tol,logged,type,parallel,maxiters));
    return __result;
END_RCPP
}













