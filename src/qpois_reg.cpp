//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
List qpois_reg(NumericMatrix X, NumericVector Y,const double ylogy,const double tol){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=pcols;
  char e='e';
  colvec b_old(d,fill::zeros),b_new(d),L1(d),yhat(n),y(Y.begin(),n,false),m(n),phi(n);
  mat L2,x(X.begin(),n,pcols,false),x_tr(n,pcols);
  double dif;
  b_old(0)=log(mean(y));
  x_tr=x.t();
  for(dif=1.0;dif>tol;){
    yhat=x*b_old;
    m=(e^yhat);
    phi=y-m;
    L1=x_tr*phi;
    L2=x.each_col()%m;
    L2=x_tr*L2;
    b_new=b_old+solve(L2,L1);
    dif=sum(abs(b_new-b_old));
    b_old=b_new;
  }
  List l;
  l["deviance"]=2.0*(ylogy-sum(y%yhat));
  l["be"]=b_new;
  l["L2"]=L2;
  l["phi"]=sum(square(phi)/m)/(n-pcols);
  return l;
}

RcppExport SEXP Rfast_qpois_reg(SEXP xSEXP,SEXP ySEXP,SEXP ylogySEXP,SEXP tolSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type y(ySEXP);
    traits::input_parameter< const double >::type ylogy(ylogySEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    __result = wrap(qpois_reg(x,y,ylogy,tol));
    return __result;
END_RCPP
}
