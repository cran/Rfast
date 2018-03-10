#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
NumericMatrix quasi_poisson_only(NumericMatrix X, NumericVector Y, const double ylogy, const double tol,const int maxiters){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=2;
  unsigned int i;
  int ij;

  colvec b_old(d),b_new(d),L1(d),yhat(n);
  vec y(Y.begin(),n,false);
  mat x(X.begin(),n,pcols,false), z(n,2,fill::ones),inv_L2(d,d),ytr=y.t(),z_tr(2,n,fill::ones);
  vec m(n),z_col_1(n);
  NumericMatrix F(2,pcols);
  double dif,sm=0.0,szm=0.0,sz2m=0.0,t,lgmeany=log(mean(y));
  for(i=0;i<pcols;++i){
    b_old(0)=lgmeany;
    b_old(1)=0;
    z_col_1=x.col(i);
    z.col(1)=z_col_1;
    z_tr.row(1)=mat(z_col_1.begin(),1,n,false);
    ij=2;
    for(dif=1.0;dif>0.000000001;){
      sm=szm=sz2m=0.0;
      yhat=z*b_old;
      m=(exp(yhat));
      L1=z_tr*(y-m);
      sm=sum(m);
      szm=sum(m%z_col_1);
      sz2m=sum(m%arma::square(z_col_1));
      t=1.0/(sm*sz2m-szm*szm);
      inv_L2.at(0,0)=sz2m*t;
      inv_L2.at(0,1)=inv_L2.at(1,0)=-szm*t;
      inv_L2.at(1,1)=sm*t;
      b_new=b_old+inv_L2*L1;
      dif=sum(abs(b_new-b_old));
      b_old=b_new;
      if(++ij==maxiters)
        break;
    }
    F(0,i)= 2.0*(ylogy-sum(y%yhat));
    F(1,i) = sum(arma::square(y-m)/m)/(n-d);
  }
  return F;
}

RcppExport SEXP Rfast_quasi_poisson_only(SEXP xSEXP,SEXP ySEXP,SEXP ylogySEXP,SEXP tolSEXP,SEXP maxitersSEXP) {
  BEGIN_RCPP
  RObject __result;
  RNGScope __rngScope;
  traits::input_parameter< NumericMatrix >::type x(xSEXP);
  traits::input_parameter< NumericVector >::type y(ySEXP);
  traits::input_parameter< const double >::type ylogy(ylogySEXP);
  traits::input_parameter< const double >::type tol(tolSEXP);
  traits::input_parameter< const int >::type maxiters(maxitersSEXP);
  __result = wrap(quasi_poisson_only(x,y,ylogy,tol,maxiters));
  return __result;
  END_RCPP
}
