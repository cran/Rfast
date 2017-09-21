//Author: Manos Papadakis
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

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



static mat cross_x_y(mat x,mat y){
  const int ncl=x.n_cols,nrw=x.n_rows,p=y.n_cols;
  mat f(ncl,p);
  colvec yv(nrw);
  int i,j;
  for(i=0;i<p;++i){
    yv=y.col(i);
    for(j=0;j<ncl;++j){
      f(j,i)=sum(x.col(j)%yv);
    }
  }
  return f;
}


//[[Rcpp::export]]
List glm_logistic(NumericMatrix X, NumericVector Y,const double tol,const int maxiters){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=pcols;
  colvec be(d,fill::zeros),yhat(n),expyhat,y(Y.begin(),n,false),W(n,fill::zeros),p(n);
  mat x(X.begin(),n,pcols,false);
  double my = accu(y)/n,d1=(n*my*log(my)+(n-n*my)*log(1-my)),d2;
  be(0)=log(my)-log(1-my);
  mat der=cross_x_y(x,y-my);
  mat der2=cross_x_y(x,x*my*(1-my));
  be=be+solve(der2,der);
  yhat = x*be;
  expyhat=exp(-yhat);
  p = 1 / (1 + expyhat);
  d2=calcDevRes(p,y,expyhat);
  int i=2;
  for(;d2-d1>tol && i<maxiters;++i){
    d1=d2;
    der=cross_x_y(x,y-p);
  	W=p%(1-p);
  	der2=cross_x_y(x,x.each_col()%W);
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
