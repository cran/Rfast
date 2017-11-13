//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>

using namespace Rcpp;
using namespace std;

//[[Rcpp::plugins(cpp11)]]

//[[Rcpp::export]]
int nth_index(NumericVector X,const int elem,const bool descend){
  IntegerVector ind=seq(1,X.size());
  NumericVector x=clone(X);
  if(descend){
      nth_element(ind.begin(),ind.begin()+elem-1,ind.end(),[&](int i,int j){return x[i-1]>x[j-1];});
  }else{
      nth_element(ind.begin(),ind.begin()+elem-1,ind.end(),[&](int i,int j){return x[i-1]<x[j-1];});
  }
  return ind[elem-1];
}

//[[Rcpp::export]]
double nth(NumericVector X,const int elem,const bool descend){
  NumericVector x=clone(X);
  if(descend){
      nth_element(x.begin(),x.begin()+elem-1,x.end(),[&](double a,double b){return a>b;});
  }else{
      nth_element(x.begin(),x.begin()+elem-1,x.end());
  }
  return x[elem-1];
}

// nth
RcppExport SEXP Rfast_nth(SEXP xSEXP,SEXP elemSEXP,SEXP descendSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< const int >::type elem(elemSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    __result = wrap(nth(x,elem,descend));
    return __result;
END_RCPP
}

// nth_index
RcppExport SEXP Rfast_nth_index(SEXP xSEXP,SEXP elemSEXP,SEXP descendSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< const int >::type elem(elemSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    __result = wrap(nth_index(x,elem,descend));
    return __result;
END_RCPP
}
