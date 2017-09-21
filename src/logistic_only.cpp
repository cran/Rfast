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


//[[Rcpp::export]]
NumericMatrix logistic_only_b(NumericMatrix X, NumericVector Y,const double tol){
  const unsigned int n=X.nrow(),pcols=X.ncol();
  unsigned int j,d=2,i;
  colvec b_old(d),b_new(d),L1(d),yhat(n),expyhat,y(Y.begin(),n,false),W(n,fill::zeros);
  mat z(n,2,fill::ones),inv_L2(d,d),tmp,z_tr(2,n),ytr=y.t(),x(X.begin(),n,pcols,false);
  vec p(n);
  NumericMatrix F(3,pcols);
  colvec::iterator expyhatiter=expyhat.begin();
  double dif,s,t,sw=0.0,szw=0.0,sz2w=0.0,my = mean(y),lgmy=log(my/(1-my));
  for(i=0;i<pcols;++i){
  	b_old(0)=lgmy;
  	b_old(1)=0;
    z.col(1)=x.col(i);
    z_tr=z.t();
    for(dif=1.0,s=0.0;dif>tol;){
      sw=szw=sz2w=0.0;
      yhat = z*b_old;
      expyhat=exp(yhat);
      p = expyhat / ( 1 + expyhat );
      for(j=0;j<n;j++){
        t=p.at(j);
        W.at(j)=t*(1-t);
        sw+=W.at(j);
      }
      szw=sum(W%z.col(1));
      sz2w=sum(W%square(z.col(1)));
      L1=z_tr*(y-p);
      t=1.0/(sw*sz2w-szw*szw);
      inv_L2.at(0,0)=sz2w*t;
      inv_L2.at(0,1)=inv_L2.at(1,0)=-szw*t;
      inv_L2.at(1,1)=sw*t;
      b_new=b_old+inv_L2*L1;
      dif=sum(abs(b_new-b_old));
      b_old=b_new;
    }  
    tmp=ytr*yhat;
    for(expyhatiter=expyhat.begin();expyhatiter!=expyhat.end();++expyhatiter)
      s+=log(1+*expyhatiter);
    F(0,i)=2.0*(s-tmp.at(0));
    F(1,i)=b_new(0);
    F(2,i)=b_new(1);
  }
  return F;
}

// logistic
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
