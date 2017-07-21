//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"
#include <cmath>

using namespace arma;

//[[Rcpp::export]]
SEXP logistic_only(NumericMatrix X, NumericVector Y,const double tol){
  const unsigned int n=X.nrow(),pcols=X.ncol();
  unsigned int i;
  colvec b_old(2),b_new(2),yhat(n),expyhat(n),y(Y.begin(),n,false),W(n,fill::zeros),p(n),x_col(n),x2_col(n),de(n);
  mat x(X.begin(),n,pcols,false);
  SEXP F=Rf_allocVector(REALSXP,pcols);
  colvec::iterator expyhatiter;
  double dif,s,t,dera2=0.0,sp=0.0,derb=0.0,dera=0.0,derab=0.0,derb2=0.0,*FF=REAL(F);
  const double my=mean(y),sy=n*my,lgmy=log(my/(1-my));
  for(i=0;i<pcols;++i,++FF){
    b_old[0]=lgmy;
    b_old[1]=0;
    x_col=x.col(i);
    x2_col=square(x_col);
    for(dif=1.0;dif>tol;){
      dera2=sp=t=0;
      yhat = b_old[0]+b_old[1]*x_col;
      expyhat=exp(yhat);
      p = expyhat / ( 1 + expyhat );
      W=p%(1-p);
      dera2=sum(W);
      sp=sum(p);
      de=y-p;
      dera=sy-sp;
      derb=sum(de%x_col);
      derab=sum(W%x_col);
      derb2=sum(W%x2_col);
      t=dera2 * derb2 - derab*derab;
      b_new[0]=b_old[0]+(derb2 * dera - derab * derb)/t;
      b_new[1]=b_old[1]+( - derab * dera + dera2 * derb )/t;
      dif=sum(abs(b_new-b_old));
      b_old=b_new;
    }  
    for(s=0,expyhatiter=expyhat.begin();expyhatiter!=expyhat.end();++expyhatiter)
      s+=log1p(*expyhatiter);
    *FF=2.0*(s-sum(y%yhat));
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
