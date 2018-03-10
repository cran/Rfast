//Author: Manos Papadakis
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "templates.h"

using namespace Rcpp;
using namespace arma;

static double calcDevRes(colvec p,colvec y,colvec expyhat){
  int psize = p.n_elem;
  double s=0.0;
  for(int i=0;i<psize;i++){
    if(y(i)==1){
      if(p(i) == 0){
        s+= expyhat(i);
      }
      else{
        s+=log(p(i));
      }
    }
    else{
      if(p(i) == 1){
        s+= expyhat(i);
      }
      else{
        s+=log(1-p(i));
      }
    }
  }
  
  return s;
}


//[[Rcpp::export]]
List glm_logistic(NumericMatrix X, NumericVector Y,const double tol,const int maxiters){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=pcols;
  colvec be(d,fill::zeros),yhat(n),expyhat,y(Y.begin(),n,false),W(n,fill::zeros),p(n);
  mat x(X.begin(),n,pcols,false);
  double my = accu(y)/n,d1=(n*my*log(my)+(n-n*my)*log(1-my)),d2;
  be(0)=log(my)-log(1-my);
  mat der=cross_x_y<mat,mat,colvec>(x,y-my);
  mat der2=cross_x_y<mat,mat,colvec>(x,x*my*(1-my));
  be=be+solve(der2,der);
  yhat = x*be;
  expyhat=exp(-yhat);
  p = 1 / (1 + expyhat);
  d2=calcDevRes(p,y,expyhat);
  int i=2;
  for(;d2-d1>tol && i<maxiters;++i){
    d1=d2;
    der=cross_x_y<mat,mat,colvec>(x,y-p);
  	W=p%(1-p);
  	der2=cross_x_y<mat,mat,colvec>(x,x.each_col()%W);
  	be=be+solve(der2,der);
  	yhat = x*be;
  	expyhat=exp(-yhat );
  	p = 1 / (1 + expyhat);
  	d2=calcDevRes(p,y,expyhat);
  }
  List l;
  l["deviance"]= -2.0 * d2;
  l["be"]=be;
  l["der2"]=der2;
  l["iter"]=i;
  return l;
}

RcppExport SEXP Rfast_glm_logistic(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP,SEXP maxitersSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type y(ySEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const int >::type maxiters(maxitersSEXP);
    __result = wrap(glm_logistic(x,y,tol,maxiters));
    return __result;
END_RCPP
}


//////////////////////////////////////////////////////////////////////////


List glm_poisson(NumericMatrix X, NumericVector Y,const double ylogy,const double tol){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=pcols;
  colvec b_old(d,fill::zeros),b_new(d),L1(d),yhat(n),y(Y.begin(),n,false),m(n);
  mat L2,x(X.begin(),n,pcols,false),x_tr(n,pcols);
  double dif;
  b_old(0)=log(mean(y));
  x_tr=x.t();
  for(dif=1.0;dif>tol;){
    yhat=x*b_old;
    m=exp(yhat);
    L1=x_tr*(y-m);
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
  return l;
}

RcppExport SEXP Rfast_glm_poisson(SEXP xSEXP,SEXP ySEXP,SEXP ylogySEXP,SEXP tolSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type y(ySEXP);
    traits::input_parameter< const double >::type ylogy(ylogySEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    __result = wrap(glm_poisson(x,y,ylogy,tol));
    return __result;
END_RCPP
}

////////////////////////////////////////////////////////////////////////
