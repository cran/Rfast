//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include <cmath>
#include "mn.h"
#include "reg_lib.h"
#include <iostream>

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
NumericMatrix spml_regs(NumericVector Y, NumericMatrix X0, const double tol, const bool logged, const int maxiters, const bool parallel){
  int n = X0.nrow(),D = X0.ncol();
  vec y(Y.begin(),n,false);
  mat x(X0.begin(),n,D,false), u(n,2);

  u.col(0) = cos(y);
  u.col(1) = sin(y);

  vec ci2 = u.col(0)%u.col(0), cisi = u.col(0)%u.col(1), si2 = u.col(1)%u.col(1);

  double f = -0.5, con = 2.506628274631;

  vec lik(D), one(n,fill::ones);

  #pragma omp parallel if(parallel)
  {
    double sumX;
    int i;
    mat xx(2,2,fill::zeros),X(n,2), B1,B2,mu,a11,a12,a22,der2(4,4),XX,der(4,1);
    vec tau,ptau,psit,rat,psit2,slv;
    mat::iterator tmpit = der2.begin(),it;
    X.col(0) = one;

    xx(0) = n;
    #pragma omp for
    for(int j = 0; j < D; j++){
      X.col(1) = x.col(j);
      sumX = sum(X.col(1));
      xx(1) = sumX;
      xx(2) = sumX;
      xx(3) = sum(X.col(1)%X.col(1));
      XX = solve(xx, X.t());
      B1 =  XX * u;
      mu = X * B1;
      tau = sum(u % mu,1);
      ptau = pnormc(tau);

      psit = tau + ptau / ( exp(f * (tau%tau))/con + tau%ptau);
      B2 = XX * (u.each_col()%psit);

      mu = mu.each_col() % psit;
      tau = sum(u % mu,1);
      ptau = pnormc(tau);
      i=2;
      while(i++<maxiters && accu(abs(B2-B1)) > tol){
        B1 = B2  ;
        mu = X * B1;
        tau = sum(u % mu,1);
        ptau = pnormc(tau);
        rat = ptau / ( exp(f * (tau%tau))/con + tau%ptau );
        psit = tau + rat;
        psit2 = 2+((-rat)%psit);
        a11 = cross_x_y_2(X, u.each_col()%psit-mu);
        der[0] = a11[0],der[1] = a11[1],der[2] = a11[2],der[3] = a11[3];

        a11 = cross_x_y_2(X, X.each_col() % (psit2%ci2 - 1));
        a12 = cross_x_y_2(X, X.each_col() % (psit2%cisi));
        a22 = cross_x_y_2(X, X.each_col() % (psit2%si2 - 1));
        it = tmpit;

        // in our case equivalent to cbind( rbind(a11, a12), rbind(a12, a22) )
          (*(it)) = a11[0], (*(++it)) = a11[2], (*(++it)) = a12[0], (*(++it)) = a12[2];
        (*(++it)) = a11[1], (*(++it)) = a11[3], (*(++it)) = a12[1], (*(++it)) = a12[3];
        (*(++it)) = a12[0], (*(++it)) = a12[2], (*(++it)) = a22[0], (*(++it)) = a22[2];
        (*(++it)) = a12[1], (*(++it)) = a12[3], (*(++it)) = a22[1], (*(++it)) = a22[3];

        slv = solve(der2,der);

        B2[0] = B1[0]-slv[0];
        B2[1] = B1[1]-slv[1];
        B2[2] = B1[2]-slv[2];
        B2[3] = B1[3]-slv[3];

      }

      lik[j] = -0.5 * accu( mu%mu ) + log1pColvecSum( (tau%ptau) * con / exp(f * (tau%tau)), n ) - n * 1.83787706640935;
    }
  }

  NumericMatrix ret(D,2);

  double ini = spml_mle2(u,ci2,cisi,si2,n,1e-09,maxiters);

  #ifdef _OPENMP
  #pragma omp parallel for if(parallel)
  #endif
  for(int i = 0; i < D; i++){
    ret(i,0) = 2 * (lik(i) - ini);
    ret(i,1) = R::pchisq(ret(i,0), 2, false, logged);
  }

  return ret;
}






RcppExport SEXP Rfast_spml_regs(SEXP YSEXP,SEXP X0SEXP,SEXP tolSEXP,SEXP loggedSEXP,SEXP maxitersSEXP,SEXP parallelSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type Y(YSEXP);
    traits::input_parameter< NumericMatrix >::type X0(X0SEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const bool >::type logged(loggedSEXP);
    traits::input_parameter< const int >::type maxiters(maxitersSEXP);
    traits::input_parameter< const int >::type parallel(parallelSEXP);
    __result = wrap(spml_regs(Y,X0,tol,logged,maxiters,parallel));
    return __result;
END_RCPP
}
