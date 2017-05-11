//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
List prop_reg(NumericMatrix X, NumericVector Y){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=pcols;
  unsigned int j,itters=0;
  const char e='e';
  colvec b_old(d,fill::zeros),b_new(d),L1(d),yhat(n),expyhat,y(Y.begin(),n,false),W(n,fill::zeros);
  mat L2,x(X.begin(),n,pcols,false),x_tr(n,pcols);
  vec p(n);
  double dif,t,my = mean(y);
  x_tr=x.t();
  b_old(0)=log(my)-log(1-my);
  for(dif=1.0;dif>0.000000001;++itters){
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
  List l;
  l["p"]=p;
  l["be"]=b_new;
  l["der2"]=L2;
  l["i"]=itters;
  return l;
}

RcppExport SEXP Rfast_prop_reg(SEXP xSEXP,SEXP ySEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type y(ySEXP);
    __result = wrap(prop_reg(x,y));
    return __result;
END_RCPP
}
