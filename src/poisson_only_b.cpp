//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
NumericMatrix poisson_only_b(NumericMatrix X, NumericVector Y,double ylogy,const double tol){
  const unsigned int n=X.nrow(),pcols=X.ncol();
  unsigned int i,d=2;
  char e='e';
  colvec b_old(d),b_new(d),L1(d),yhat(n),y(Y.begin(),n,false);
  mat z(n,2,fill::ones),inv_L2(d,d),ytr=y.t(),z_tr(2,n),x(X.begin(),n,pcols,false);
  vec m(n),z_col_1(n);
  NumericMatrix F(3,pcols);
  double dif,sm=0.0,szm=0.0,sz2m=0.0,t,lgmeany=log(mean(y));
  for(i=0;i<pcols;++i){
  	b_old(0)=lgmeany;
  	b_old(1)=0;
    z.col(1)=x.col(i);
    z_col_1=z.col(1);
    z_tr=z.t();
    for(dif=1.0;dif>tol;){
      sm=szm=sz2m=0.0;
      yhat=z*b_old;
      m=(e^yhat);
      L1=z_tr*(y-m);
      sm=sum(m);
      szm=sum(m%z_col_1);
      sz2m=sum(m%square(z_col_1));
      t=1.0/(sm*sz2m-szm*szm);
      inv_L2.at(0,0)=sz2m*t;
      inv_L2.at(0,1)=inv_L2.at(1,0)=-szm*t;
      inv_L2.at(1,1)=sm*t;
      b_new=b_old+inv_L2*L1;
      dif=sum(abs(b_new-b_old));
      b_old=b_new;
    }
    F(0,i)=2.0*(ylogy-y*yhat);
    F(1,i)=b_new(0);
    F(2,i)=b_new(1);
  }
  return F;
}

// poisson
RcppExport SEXP Rfast_poisson_only_b(SEXP xSEXP,SEXP ySEXP,SEXP ylogySEXP,SEXP tolSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type y(ySEXP);
    traits::input_parameter< double >::type ylogy(ylogySEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    __result = wrap(poisson_only_b(x,y,ylogy,tol));
    return __result;
END_RCPP
}
