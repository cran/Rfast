//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include <cmath>
#include "mn.h"
#include "reg_lib.h"
#include "templates.h"
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

  if(parallel){
      #ifdef _OPENMP
     #pragma omp parallel
     {
     #endif
    double sumX;
    int i;
    mat xx(2,2,fill::zeros),X(n,2), B1,B2,mu,a11,a12,a22,der2(4,4),XX,der(4,1);
    vec tau,ptau,psit,rat,psit2,slv;
    mat::iterator tmpit = der2.begin(),it;
    X.col(0) = one;

    xx(0) = n;
    #ifdef _OPENMP
    #pragma omp for
    #endif
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
        a11 = cross_x_y<mat,mat,vec>(X, u.each_col()%psit-mu);
        der[0] = a11[0],der[1] = a11[1],der[2] = a11[2],der[3] = a11[3];

        a11 = cross_x_y<mat,mat,vec>(X, X.each_col() % (psit2%ci2 - 1));
        a12 = cross_x_y<mat,mat,vec>(X, X.each_col() % (psit2%cisi));
        a22 = cross_x_y<mat,mat,vec>(X, X.each_col() % (psit2%si2 - 1));
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
    #ifdef _OPENMP
    }
    #endif
  }
  else{
    double sumX;
    int i;
    mat xx(2,2,fill::zeros),X(n,2), B1,B2,mu,a11,a12,a22,der2(4,4),XX,der(4,1);
    vec tau,ptau,psit,rat,psit2,slv;
    mat::iterator tmpit = der2.begin(),it;
    X.col(0) = one;

    xx(0) = n;

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
        a11 = cross_x_y<mat,mat,vec>(X, u.each_col()%psit-mu);
        der[0] = a11[0],der[1] = a11[1],der[2] = a11[2],der[3] = a11[3];

        a11 = cross_x_y<mat,mat,vec>(X, X.each_col() % (psit2%ci2 - 1));
        a12 = cross_x_y<mat,mat,vec>(X, X.each_col() % (psit2%cisi));
        a22 = cross_x_y<mat,mat,vec>(X, X.each_col() % (psit2%si2 - 1));
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

  if(parallel){
      #ifdef _OPENMP
      #pragma omp parallel for
      #endif
      for(int i = 0; i < D; i++){
        ret(i,0) = 2 * (lik(i) - ini);
        ret(i,1) = R::pchisq(ret(i,0), 2, false, logged);
      }
  }
  else{
      for(int i = 0; i < D; i++){
        ret(i,0) = 2 * (lik(i) - ini);
        ret(i,1) = R::pchisq(ret(i,0), 2, false, logged);
      }
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


/////////////////////////////////////////////////////////////////////////////////////////////////


List spml_reg_helper(mat B1,mat B2,mat x,mat u,colvec ci,colvec si,const double con,const double tol){
	int i=2;
  	const double f=-0.5;
	mat tx=x.t(),a11,a12,a22,mu,der2,tmp;
	colvec tau,ptau,rat,psit,psit2,der,ctmp,cisq=square(ci),sisq=square(si);
	while ( sum_abs(B2,B1) > tol ) {
		++i;
		B1 = B2;
		mu = x * B1;
		tau = sum(u % mu,1);
		ptau = pnormc(tau);
		rat = ptau / ( exp(f * square(tau))/con + tau % ptau );
		psit = tau + rat;
		psit2 = 2 - tau % rat - square(rat);
		tmp=tx * (  psit % u.each_col() - mu);
		der = colvec(tmp.begin(),tmp.n_elem,false);
		a11 = tx * ( x.each_col() % (psit2 % cisq - 1) );
		a12 = tx * ( x.each_col() % (psit2 % ci % si ) );
		a22 = tx * ( x.each_col() % (psit2 % sisq - 1 ) );
		der2 = join_cols( join_rows(a11, a12), join_rows(a12, a22) );
		ctmp=solve(der2, der);
		tmp=mat(ctmp.begin(),x.n_cols,2,false);
		B2 = B1 - tmp;
	}
	List l;
	l["B2"]=B2;
	l["mu"]=mu;
	l["tau"]=tau;
	l["ptau"]=ptau;
	l["der2"]=der2;
	l["i"]=i;
	return l;
}

RcppExport SEXP Rfast_spml_reg_helper(SEXP B1SEXP,SEXP B2SEXP,SEXP xSEXP,SEXP uSEXP,SEXP ciSEXP,SEXP siSEXP,SEXP conSEXP,SEXP tolSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< mat >::type B1(B1SEXP);
    traits::input_parameter< mat >::type B2(B2SEXP);
    traits::input_parameter< mat >::type x(xSEXP);
    traits::input_parameter< mat >::type u(uSEXP);
    traits::input_parameter< colvec >::type ci(ciSEXP);
    traits::input_parameter< colvec >::type si(siSEXP);
    traits::input_parameter< const double >::type con(conSEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    __result = wrap(spml_reg_helper(B1,B2,x,u,ci,si,con,tol));
    return __result;
END_RCPP
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////


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


////////////////////////////////////////////////////////////////////////////////////////////////
