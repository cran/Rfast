//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"
#include <cmath>

using namespace arma;

//[[Rcpp::export]]
NumericVector logistic_only(NumericMatrix X, NumericVector Y){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=2;
  unsigned int j,i;
  char e='e';
  colvec b_old(d),b_new(d),L1(d),yhat(n),expyhat,y(Y.begin(),n,false),W(n,fill::zeros),p(n),z_tr_i(n),x2_col;
  mat z(n,2,fill::ones),inv_L2(d,d),z_tr(2,n,fill::ones),x(X.begin(),n,pcols,false);
  NumericVector F(pcols);
  colvec::iterator expyhatiter=expyhat.begin();
  double dif,s,t,sw=0.0,szw=0.0,sz2w=0.0,my = mean(y),lgmy=log(my/(1-my));
  for(i=0;i<pcols;++i){
    b_old(0)=lgmy;
    b_old(1)=0;
    z_tr_i=x.col(i);
    z.col(1)=z_tr_i;
    z_tr.row(1)=mat(z_tr_i.begin(),1,n,false);
    x2_col=square(z_tr_i);
    for(dif=1.0,s=0.0;dif>0.000000001;){
      sw=szw=sz2w=0.0;
      yhat = z*b_old;
      expyhat=(e^yhat);
      p = expyhat / ( 1 + expyhat );
      for(j=0;j<n;j++){
        t=p.at(j);
        W.at(j)=t*(1-t);
        sw+=W.at(j);
      }
      szw=sum(W%z_tr_i);
      sz2w=sum(W%x2_col);
      L1=z_tr*(y-p);
      t=1.0/(sw*sz2w-szw*szw);
      inv_L2.at(0,0)=sz2w*t;
      inv_L2.at(0,1)=inv_L2.at(1,0)=-szw*t;
      inv_L2.at(1,1)=sw*t;
      b_new=b_old+inv_L2*L1;
      dif=sum(abs(b_new-b_old));
      b_old=b_new;
    }  
    for(expyhatiter=expyhat.begin();expyhatiter!=expyhat.end();++expyhatiter)
      s+=log1p(*expyhatiter);
    F[i]=2.0*(s-sum(y%yhat));
  }
  return F;
}

// logistic
RcppExport SEXP Rfast_logistic_only(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type y(ySEXP);
    __result = wrap(logistic_only(x,y));
    return __result;
END_RCPP
}
