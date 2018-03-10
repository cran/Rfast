//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include "mn.h"
#include <cmath>
#include "reg_lib.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
SEXP logistic_only(NumericMatrix X, NumericVector Y,const double tol){
  int maxiters = 100;
  const unsigned int n=X.nrow(),pcols=X.ncol();

  unsigned int i=0;
  int j=0;
  colvec y(Y.begin(),n,false), be(2),expyhat(n),W(n,fill::zeros),x_col(n),x2_col(n),de(n);
  mat x(X.begin(),n,pcols,false), yhat(n,1),p(n,1);
  SEXP F=Rf_allocVector(REALSXP,pcols);
  double d1=0,d2=0,t=0,dera2=0.0,sp=0.0,derb=0.0,dera=0.0,derab=0.0,derb2=0.0,*FF=REAL(F);
  const double my=mean(y),sy=my*n,lgmy=log(my/(1-my)),d0 = -2*(sy*log(my)+(n-sy)*log(1-my));

  double W0=my*(1-my);
  double dera20=n*W0;
  colvec de0(n);
  de0=y-my;
  double dera0=0;

  for(i=0;i<pcols;++i,++FF){
    d1=d0;
    be[0]=lgmy;
    be[1]=0;
    x_col=x.col(i);
    x2_col=arma::square(x_col);
    derb=sum(de0%x_col);
    derab=W0*sum(x_col);
    derb2=W0*sum(x2_col);
    t=dera20 * derb2 - derab*derab;
    be[0]=be[0]+(derb2 * dera0 - derab * derb)/t;
    be[1]=be[1]+( - derab * dera0 + dera20 * derb )/t;

    yhat.col(0) = be[0]+be[1]*x_col;
    expyhat=exp(-yhat.col(0));
    p.col(0) = 1 / ( 1 + expyhat );

    d2 = -2*calcDevRes(p,y,yhat);
    j=2;
    while(j++<maxiters && d1-d2>tol){
      d1=d2;
      W=p%(1-p);
      dera2=sum(W);
      sp=sum(p.col(0));
      de=y-p.col(0);
      dera=sy-sp;
      derb=sum(de%x_col);
      derab=sum(W%x_col);
      derb2=sum(W%x2_col);
      t=dera2 * derb2 - derab*derab;
      be[0]=be[0]+(derb2 * dera - derab * derb)/t;
      be[1]=be[1]+( - derab * dera + dera2 * derb )/t;
      yhat.col(0) = be[0]+be[1]*x_col;
      expyhat=exp(-yhat.col(0));
      p.col(0) = 1 / ( 1 + expyhat );

      d2 = -2*calcDevRes(p,y,yhat);
    }

    *FF = d2;
  }
  return F;
}

// logistic
RcppExport SEXP Rfast_logistic_only(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP) {
  BEGIN_RCPP
  RObject __result;
  RNGScope __rngScope;
  traits::input_parameter< NumericMatrix >::type x(xSEXP);
  traits::input_parameter< NumericVector >::type y(ySEXP);
  traits::input_parameter< const double >::type tol(tolSEXP);
  __result = logistic_only(x,y,tol);
  return __result;
  END_RCPP
}

//[[Rcpp::export]]
NumericMatrix logistic_only_b(NumericMatrix X, NumericVector Y,const double tol){
  int maxiters = 100;
  const unsigned int n=X.nrow(),pcols=X.ncol();

  unsigned int i=0;
  int j=0;
  colvec y(Y.begin(),n,false), be(2),expyhat(n),W(n,fill::zeros),x_col(n),x2_col(n),de(n);
  mat x(X.begin(),n,pcols,false), yhat(n,1),p(n,1);
  NumericMatrix F(3,pcols);
  double d1=0,d2=0,t=0,dera2=0.0,sp=0.0,derb=0.0,dera=0.0,derab=0.0,derb2=0.0;
  const double my=mean(y),sy=my*n,lgmy=log(my/(1-my)),d0 = -2*(sy*log(my)+(n-sy)*log(1-my));

  double W0=my*(1-my);
  double dera20=n*W0;
  colvec de0(n);
  de0=y-my;
  double dera0=0;

  for(i=0;i<pcols;++i){
    d1=d0;
    be[0]=lgmy;
    be[1]=0;
    x_col=x.col(i);
    x2_col=arma::square(x_col);
    derb=sum(de0%x_col);
    derab=W0*sum(x_col);
    derb2=W0*sum(x2_col);
    t=dera20 * derb2 - derab*derab;
    be[0]=be[0]+(derb2 * dera0 - derab * derb)/t;
    be[1]=be[1]+( - derab * dera0 + dera20 * derb )/t;

    yhat.col(0) = be[0]+be[1]*x_col;
    expyhat=exp(-yhat.col(0));
    p.col(0) = 1 / ( 1 + expyhat );

    d2 = -2*calcDevRes(p,y,yhat);
    j=2;
    while(j++<maxiters && (d1-d2)>tol){
      d1=d2;
      W=p%(1-p);
      dera2=sum(W);
      sp=sum(p.col(0));
      de=y-p.col(0);
      dera=sy-sp;
      derb=sum(de%x_col);
      derab=sum(W%x_col);
      derb2=sum(W%x2_col);
      t=dera2 * derb2 - derab*derab;
      be[0]=be[0]+(derb2 * dera - derab * derb)/t;
      be[1]=be[1]+( - derab * dera + dera2 * derb )/t;
      yhat.col(0) = be[0]+be[1]*x_col;
      expyhat=exp(-yhat.col(0));
      p.col(0) = 1 / ( 1 + expyhat );

      d2 = -2*calcDevRes(p,y,yhat);
    }
    F(0,i)=d2;
    F(1,i)=be[0];
    F(2,i)=be[1];
  }
  return F;
}

RcppExport SEXP Rfast_logistic_only_b(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP) {
  BEGIN_RCPP
  RObject __result;
  RNGScope __rngScope;
  traits::input_parameter< NumericMatrix >::type x(xSEXP);
  traits::input_parameter< NumericVector >::type y(ySEXP);
  traits::input_parameter< const double >::type tol(tolSEXP);
  __result = wrap(logistic_only_b(x,y,tol));
  return __result;
  END_RCPP
}
