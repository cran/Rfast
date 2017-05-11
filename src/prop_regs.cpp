//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"
#include <cmath>

using namespace arma;
using namespace std;

//[[Rcpp::export]]
NumericVector prop_regs(NumericMatrix X, NumericVector Y,const double tol,const string varb){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=2;
  unsigned int j,i;
  char e='e';
  colvec b_old(d),b_new(d),L1(d),yhat(n),expyhat,y(Y.begin(),n,false),W(n,fill::zeros),p(n),z_tr_i(n),x2_col,u(n,fill::zeros),u2(n);
  mat z(n,2,fill::ones),inv_L2(d,d),z_tr(2,n,fill::ones),x(X.begin(),n,pcols,false);
  NumericVector F(pcols);
  double dif,s,t,sw=0.0,szw=0.0,sz2w=0.0,my = mean(y),lgmy=log(my/(1-my)),vb,b12;
  for(i=0;i<pcols;++i){
    b_old(0)=lgmy;
    b_old(1)=0;
    z_tr_i=x.col(i);
    z.col(1)=z_tr_i;
    z_tr.row(1)=mat(z_tr_i.begin(),1,n,false);
    x2_col=square(z_tr_i);
    for(dif=1.0,s=0.0;dif>tol;){
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
      u=y-p;
      L1=z_tr*u;
      t=1.0/(sw*sz2w-szw*szw);
      inv_L2.at(0,0)=sz2w*t;
      inv_L2.at(0,1)=inv_L2.at(1,0)=-szw*t;
      inv_L2.at(1,1)=sw*t;
      b_new=b_old+inv_L2*L1;
      dif=sum(abs(b_new-b_old));
      b_old=b_new;
    }  
    u2=square(u);
    if(varb=="quasi"){
      b12=sum(u2%z_tr_i);
      vb=-szw*(-szw*sum(u2)+sw*b12)/((sw*sz2w-szw*szw)*(sw*sz2w-szw*szw))+sw*(-szw*b12+sum(u2%x2_col)*sw)/((sw*sz2w-szw*szw)*(sw*sz2w-szw*szw));
    }else if(varb=="glm"){
      s=sum(u2/W)/(n-2);
      vb=s*sw/(sw*sz2w-szw*szw);
    }else{
      stop("Unsupported varb. Enter \"glm\" or \"quasi\".");
    }
    F[i]=b_new[1]*b_new[1]/vb;
  }
  return F;
}

RcppExport SEXP Rfast_prop_regs(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP,SEXP varbSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type y(ySEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const string >::type varb(varbSEXP);
    __result = wrap(prop_regs(x,y,tol,varb));
    return __result;
END_RCPP
}
