//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
List glm_logistic(NumericMatrix X, NumericVector Y,const double tol){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=pcols;
  unsigned int j;
  const char e='e';
  colvec b_old(d,fill::zeros),b_new(d),L1(d),yhat(n),expyhat,y(Y.begin(),n,false),W(n,fill::zeros);
  mat L2,x(X.begin(),n,pcols,false),x_tr(n,pcols);
  vec p(n);
  colvec::iterator expyhatiter=expyhat.begin();
  double dif,s=0.0,t,my = mean(y);
  x_tr=x.t();
  b_old(0)=log(my)-log(1-my);
  for(dif=1.0;dif>tol;){
    yhat = x*b_old;
    expyhat=(e^yhat);
    p = expyhat / ( 1 + expyhat );
    for(j=0;j<n;++j){
      t=p.at(j);
      W.at(j)=t-t*t;
    }
    L1=x_tr*(y-p);
    L2=x.each_col()%W;
    L2=x_tr*L2;
    b_new=b_old+solve(L2,L1);
    dif=sum(abs(b_new-b_old));
    b_old=b_new;
  }
  t=sum(y%yhat);
  for(expyhatiter=expyhat.begin();expyhatiter!=expyhat.end();++expyhatiter)
    s+=log1p(*expyhatiter);
  List l;
  l["deviance"]=2.0*(s-t);
  l["be"]=b_new;
  l["L2"]=L2;
  return l;
}

RcppExport SEXP Rfast_glm_logistic(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type y(ySEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    __result = wrap(glm_logistic(x,y,tol));
    return __result;
END_RCPP
}
